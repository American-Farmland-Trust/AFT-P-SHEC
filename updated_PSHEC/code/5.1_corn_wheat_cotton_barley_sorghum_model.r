# R version 4.2.1 (2022-06-23) or higher
# This script is used to 
# 1) select the best model to predict crop yield, 
# 2) extract the relationship between SOM and yield, while holding other variables constant
# 3) use bootstrap to generate 90% prediction interval 
# Use the tidymodels frame work to compare different preprocessing&model combinations

#' 
#' # Setup
## ----setup-----------------------------------------------------------------------------------------------------------------------------------------
library(tidymodels)  # includes the workflows package
tidymodels_prefer() # resolve the name conflicts

library(ranger) # implements random forest
library(multilevelmod) # provide extenstions for fitting multilevel models in the tidymodels framework
library(lme4) # for linear and generalized linear mixed-effects models
library(rules) # rule based machine learning models with tidymodels
library(finetune) # model tuning capabilities in the tidymodels framework.
library(DALEXtra) #  creating explainable machine learning visualizations
library(ggpmisc) # extends ggplot2 to add annotations and statistical information to plots
library(rsample) # for resampling datasets (e.g., cross-validation, bootstrapping)
library(ggplot2) # data visualization

#

for (crop in c('CORN','COTTON','BARLEY','SORGHUM','WHEAT')) {
  
  cat("Start the modeling for:", crop, "\n")
  
  # Define the output directory ------####
  output_dir <- paste0('/home/meng/Documents/updated_PSHEC/models/',crop,'_model/')
  
  # Ensure the directory exists before the loop starts
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  

#' 
#' # Read in data
#' 
## ----data------------------------------------------------------------------------------------------------------------------------------------------

# read yield data, weather data + soil type, drainage class, and slope
data <- readRDS(paste0('~/Documents/updated_PSHEC/merged_data/',crop,'_all_data_soil_weather_N_n15_om10_AFT.rds'))

# check unique states
# unique(data$state_alpha)

#[1] "AL" "AR" "CA" "CO" "DE" "GA" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "MD" "MI" "MN" "MS" "MO" "MT" "NE" "NJ" "NY" "NC" "ND" "OH" "OK" "PA" "SC" "SD" "TN" "TX" "VA" "WA"
#[34] "WV" "WI" "WY"

# convert state_alpha from characters to factors 
data$state_alpha <- as.factor(data$state_alpha)
data$ssurgo_order_mode <- as.factor(data$ssurgo_order_mode)

# Reorder drainage levels as this is an ordinal variable
# the order of drainage class is as follows
data <- data %>%
  mutate(ssurgo_drainage_class_mode = factor(ssurgo_drainage_class_mode, levels = c("Excessively drained", "Somewhat excessively drained",
                                                                                    "Well drained", "Moderately well drained",
                                                                                    "Somewhat poorly drained", "Poorly drained",
                                                                                    "Very poorly drained")))

# run the model for rainfed and irrigated separately, and the all model 

model_function <- function(i) {
  
  # filter data if running for irrigated and rainfed
  
  if (i %in% c('irrigated', 'rainfed')) {
    data <- data %>%
      dplyr::filter(irrigated == i)
  } 
  
  # print the progress
  cat("Start the workflow for:", paste0(i, "_", crop), "\n")
  
  #' 
  #' # Start the tidymodels workflow
  #' 
  #' ## 1. Split the data using the 80:20 for training-to-test and resample the training set using five repeats of 10-fold cross-validation
  ## ----splitdata-------------------------------------------------------------------------------------------------------------------------------------
  
  # Splitting the data into 80% training and 20% testing
  # Using strata to make sure the resamples have equivalent proportions as the original data between rainfed and irrigated if apporporate
  set.seed(1501)  # Set a seed for reproducibility
  corn_split <- initial_split(data, prop = 0.8) # strata = irrigated
  
  # Extract the training and testing sets
  corn_train <- training(corn_split)
  corn_test  <- testing(corn_split)
  
  # Resample the training set using five repeats of 10-fold cross-validation
  set.seed(1502)
  corn_folds <- 
    vfold_cv(corn_train, repeats = 5) #, strata = irrigated
  
  #' 
  #' ## 2. create the data preprocessing recipe
  #' 
  ## ----recipe----------------------------------------------------------------------------------------------------------------------------------------
  # the basic recipe contains all available variables
  basic_rec <- 
    recipe(Yield_decomp_add ~ 
             ssurgo_om_mean + 
             Mean.percent.irrigated + # add irrigation percentage
             ssurgo_clay_mean + ssurgo_silt_mean + ssurgo_sand_mean + 
             ssurgo_awc_mean + ssurgo_aws_mean + ssurgo_fifteenbar_mean +
             ssurgo_cec_mean +ssurgo_h +  
             Ninput_kgN.ha.yr_mean + # add nitrogen input
             annual_cum_ppt + annual_tmean + annual_GDD + # add selected weather variable
             ssurgo_slope_mean + ssurgo_drainage_class_mode + 
             #ssurgo_order_mode + # soil order was originally added but removed due to low importance
             GEOID + state_alpha, data =corn_train) %>%
    add_role(ssurgo_om_mean, new_role = 'om') %>% # add the role of om so that it won't be removed in the following steps
    update_role(GEOID, new_role = 'ID') # update the role of GEOID as it will not be used as a predictor 
  
  # Conditionally remove Mean.percent.irrigated if i == 'rainfed'
  if (i == "rainfed") {
    basic_rec <- basic_rec %>%
      step_rm(Mean.percent.irrigated)
  }
  
  
  #' 
  #' 2.2 Specific recipes for different models
  ## ----spec------------------------------------------------------------------------------------------------------------------------------------------
  library(embed)
  
  # selected less correlated variables and two-way interaction terms as the predictors; 
  # state was converted to dummy variables
  lasso_corr_d_int2 <- 
    basic_rec %>%
    # Mark original numeric predictors with a custom role
    update_role(all_numeric_predictors(), old_role = 'predictor', new_role = "numeric_original") %>%
    # handle multicollinearity
    step_corr(all_numeric_predictors(),has_role('numeric_original'),-all_of("ssurgo_om_mean"), threshold = 0.7) %>%
    # encode oridnal variable as integers
    step_integer(ssurgo_drainage_class_mode) %>%
    # encode categorical variable as dummy variable
    #step_dummy(ssurgo_order_mode, one_hot = TRUE) %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    # normalize all numeric predictors, including dummy variables
    step_normalize(has_role("numeric_original") | starts_with("state_alpha_")|starts_with("ssurgo_drainage_class_")) %>%
    # interactions
    step_interact(terms = ~ has_role("numeric_original") * has_role("numeric_original"), sep = '*') %>%
    step_interact(terms = ~ has_role("numeric_original") * starts_with('state_alpha_'), sep = '*')
  
  #lasso_corr_d_int2_prep<-prep(lasso_corr_d_int2)
  # Check the mappings created by step_integer
  #encoding_info <- tidy(lasso_corr_d_int2_prep, number = 5) 
  # check transformed data
  #lasso_corr_d_int2_data <- bake(lasso_corr_d_int2_prep, new_data = NULL)
  
  
  # selected less correlated variables and two-way interaction terms are the predictors; state was convert into a single set of scores derived from a generalized linear mixed model. lmer(outcome ~ 1 + (1 | predictor)
  lasso_corr_m_int2 <- 
    basic_rec %>%
    # Mark original numeric predictors with a custom role
    update_role(all_numeric_predictors(), old_role = 'predictor', new_role = "numeric_original") %>%
    # handle multicollinearity
    step_corr(all_numeric_predictors(),has_role('numeric_original'),-all_of("ssurgo_om_mean"), threshold = 0.7) %>%
    # encode oridnal variable as integers
    step_integer(ssurgo_drainage_class_mode) %>%
    # encode categorical variable as dummy variable
    #step_dummy(ssurgo_order_mode, one_hot = TRUE) %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    # normalize all numeric predictors, including dummy variables
    step_normalize(has_role("numeric_original") | starts_with("state_alpha_")|starts_with("ssurgo_drainage_class_")) %>%
    step_interact(terms = ~ has_role("numeric_original") * has_role("numeric_original"), sep = '*') %>%
    step_lencode_mixed(state_alpha, outcome = vars(Yield_decomp_add)) #%>%
  
  # note: used corr = 0.7 as the cut off because a study showed that 
  #collinearity becomes a more serious problem (inflating the variance of estimated regression coefficients, 
  #and therefore not necessarily finding the 'significant' ones correctly) 
  #at r>0.7 (reference:  https://doi.org/10.1111/j.1600-0587.2012.07348.x)
  
  # check preprocessed data
  #prep_rec <- prep(basic_rec)
  # check transformed data
  #processed_data <- bake(prep_rec, new_data = NULL)
  # check names
  #names(processed_data)
  #[1] "ssurgo_om_mean"             "Mean.percent.irrigated"     "ssurgo_clay_mean"           "ssurgo_silt_mean"           "ssurgo_sand_mean"          
  #[6] "ssurgo_awc_mean"            "ssurgo_aws_mean"            "ssurgo_fifteenbar_mean"     "ssurgo_cec_mean"            "ssurgo_h"                  
  #[11] "Ninput_kgN.ha.yr_mean"      "annual_cum_ppt"             "annual_tmean"               "annual_GDD"                 "ssurgo_slope_mean"         
  #[16] "ssurgo_drainage_class_mode" "GEOID"                      "state_alpha"                "Yield_decomp_add"  
  
  # Note: these variables are removed due to collinearity: 
  # ssurgo_sand_mean, ssurgo_awc_mean, ssurgo_aws_mean, ssurgo_fifteenbar_mean, ssurgo_cec_mean, annual_GDD
  
  # normalize numerical variables, no conversion
  normalize_rec <-
    basic_rec %>%
    step_normalize(all_numeric_predictors()) 
  
  # convert categorical variables to numeric, and normalize all variables
  norm_dum_rec <-
    basic_rec %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    #step_dummy(ssurgo_order_mode, one_hot = TRUE) %>%
    step_integer(ssurgo_drainage_class_mode) %>%
    step_normalize(all_numeric_predictors())
  
  # control for collinearity, convert categorical variables to numeric, and normalize all variables
  norm_corr_dum_rec <-
    basic_rec %>%
    # handle multicollinearity
    step_corr(all_numeric_predictors(),-has_role('om'), threshold = 0.7) %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    #step_dummy(ssurgo_order_mode, one_hot = TRUE) %>%
    step_integer(ssurgo_drainage_class_mode) %>%
    step_normalize(all_numeric_predictors())
  
  # control for collinearity, conversion, and normalize; for SVM specifically
  norm_corr_dum_rec_svm <- 
    basic_rec %>%
    update_role(all_numeric_predictors(), old_role = 'predictor', new_role = "numeric_original") %>%
    # handle multicollinearity
    step_corr(all_numeric_predictors(),has_role('numeric_original'),-all_of("ssurgo_om_mean"), threshold = 0.8) %>%
    # encode oridnal variable as integers
    step_integer(ssurgo_drainage_class_mode) %>%
    # encode categorical variable as dummy variable
    #step_dummy(ssurgo_order_mode, one_hot = TRUE) %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    # normalize all numeric predictors, including dummy variables
    step_normalize(has_role("numeric_original") | starts_with("state_alpha_")|starts_with("ssurgo_drainage_class_"))
  
  # control for collinearity, conversion, and normalize; for GAM
  norm_corr_rec <- 
    basic_rec %>%
    # handle multicollinearity
    step_corr(all_numeric_predictors(),-has_role('om'), threshold = 0.7) %>%
    step_normalize(all_numeric_predictors())
  
  
  # summary of different models and perferences 
  # Model	Handle Collinearity?	Scale Needed?	            Categorical to Numeric?     recipe 
  # LASSO	No (requires control)	Yes (requires scaling)	  Yes (requires conversion)   customized
  # CART	      Yes	                No	                  No                          norm_rec
  # RF	        Yes	                No	                  No                          norm_rec
  # XGBoost	    Yes	                Yes (prefer scaling)	Yes (requires conversion)   norm_dum_rec
  # Cubist	    Yes	                Yes (prefer scaling)	Yes (requires conversion)   norm_dum_rec
  # GAM	  No (requires control)	    Optional	            No                          norm_corr_rec
  # NNET	Yes (optional)	      Yes (requires scaling)	  Yes (requires conversion)   didn't use
  # SVM   No (require control) Yes                   Yes                              norm_corr_dum_rec
  # BART  Yes                   Yes (prefer scaling)      Yes (requires conversion)   norm_dum_rec
  
  #' 
  #' ## 3. create model specification 
  #' 
  ## ----model-----------------------------------------------------------------------------------------------------------------------------------------
  
  library(ranger)
  library(xgboost)
  library(Cubist)
  library(dbarts)
  
  #Note: used lasso model to select variables with penalty: lasso model was recommended over stepwise models; and the final predictive model also does a better job than the stepwise model; 
  #see discussions here:https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
  lasso_model <- linear_reg(penalty = tune(),mixture = 1) %>%
    set_mode('regression') %>%
    set_engine('glmnet') # In the glmnet model, mixture = 1 is a pure lasso model while mixture = 0 indicates that ridge regression is being used.
  
  # cart and rf models can also be used: these are non-linear regressions
  cart_model <- 
    decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
    set_engine("rpart") %>% 
    set_mode("regression") # a regression tree model 
  
  rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  #nnet_model <- 
  #  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  #  set_engine("nnet", MaxNWts = 2600) %>% 
  #  set_mode("regression")
  
  # note: The analysis in M. Kuhn and Johnson (2013) specifies that the neural network should have up to 27 hidden units in the layer. The extract_parameter_set_dials() function extracts the parameter set, which we modify to have the correct parameter range:https://www.tmwr.org/workflow-sets.html
  #nnet_param <- 
  #  nnet_model %>% 
  # extract_parameter_set_dials() %>% 
  #  update(hidden_units = hidden_units(c(1, 27)))
  
  # XGBoost 
  xgb_model <- 
    boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
               min_n = tune(), sample_size = tune(), trees = tune()) %>% 
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  # cubist model
  cubist_model <- 
    cubist_rules(committees = tune(), neighbors = tune()) %>% 
    set_engine("Cubist") 
  
  # support vector machines (SVM)
  # linear kernel 
  svm_linear_model <- svm_linear(cost = tune()) %>%
    set_mode("regression") %>%
    set_engine("kernlab")
  # radial basis kernel
  svm_rbf_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
    set_mode("regression") %>%
    set_engine("kernlab")
  
  # Bayesian Additive Regression Trees (BART)
  bart_model <- bart(trees = tune(), prior_terminal_node_coef = tune()) %>%
    set_mode("regression") %>%
    set_engine("dbarts")
  
  gam_model <- gen_additive_mod(adjust_deg_free = tune()) %>%
    set_mode('regression') %>%
    set_engine('mgcv')
  
  #' 
  #' ## create model workflow_set
  #' 
  ## ----workflow--------------------------------------------------------------------------------------------------------------------------------------
  library(multilevelmod)
  library(lme4)
  library(rules)
  
  # add machine learning flows
  all_workflows <- 
    workflow_set(
      preproc = list(
        lasso_corr_d_2 = lasso_corr_d_int2,
        lasso_corr_m_2 = lasso_corr_m_int2, 
        cart = normalize_rec, 
        rf = normalize_rec,
        #nnet = normalize_rec,
        xgb = norm_dum_rec,
        xgb_corr = norm_corr_dum_rec,
        cubist = norm_dum_rec,
        svm_l = norm_corr_dum_rec_svm,
        svm_rbf = norm_corr_dum_rec_svm,
        bart = norm_dum_rec,
        gam = norm_corr_rec
      ),
      models = list(
        lasso_model, lasso_model, #lasso_model, lasso_model,
        cart_model,
        rf_model,
        #nnet_model,
        xgb_model,
        xgb_model,
        cubist_model,
        svm_linear_model,
        svm_rbf_model,
        bart_model,
        gam_model
      ),
      cross = FALSE
    )
  
  #' 
  #' ## Run all workflows
  #' 
  ## ----run-------------------------------------------------------------------------------------------------------------------------------------------
  library(finetune)
  # to effectively screen a large set of models using the race method:https://www.tmwr.org/grid-search.html#racing
  library(doParallel) # set up parallel backend
  
  # Register parallel backend
  cl <- makePSOCKcluster(parallel::detectCores()-2)  # Use defined number of core
  registerDoParallel(cl)
  
  race_ctrl <-
    control_race(
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    )
  
  race_results <-
    all_workflows %>%
    workflow_map(
      "tune_race_anova",
      seed = 1503,
      resamples = corn_folds,
      grid = 25,
      control = race_ctrl
    )
  
  # Stop the cluster after tuning
  stopCluster(cl)
  
  #plot_label <-  c("random_forest","xgb_boost_tree","cubist_rules",
  #                 "cart_decision_tree","neural_network",
  #                 "linear_interactions*3", 
  #                 "linear_interactions*2", 
  #                 "generalized additive model_soil", 
  #                 "generalized additive model_om",
  #                 "linear_interactions*3 , random_state" , 
  #                 "linear_interactions*2 + random_state"
  #) 
  saveRDS(race_results, paste0(output_dir,i,'_',crop,'_race_results.rds'))
  
  # Identify all workflow IDs in the workflow set
  wflow_ids <- race_results %>%
    distinct(wflow_id) %>%
    pull(wflow_id)
  
  # Extract the results for each flow
  all_results_pull <- wflow_ids %>%
    set_names() %>%
    map(~ extract_workflow_set_result(race_results, .x))
  
  # Figure out which ones are "bad": having no .metrics or zero rows
  res_info <- tibble(
    wflow_id = names(all_results_pull),
    n_metrics = map_int(all_results_pull, function(res) {
      # If it’s a try-error or empty list, it’s obviously “bad.”
      if (inherits(res, "try-error") || (is.list(res) && length(res) == 0)) {
        return(NA_integer_)
      }
      
      # Otherwise, try collect_metrics().
      # If that fails, we also mark it as NA.
      out <- tryCatch({
        metrics <- collect_metrics(res)   # summarises performance
        nrow(metrics)                     # how many summary rows
      }, error = function(e) NA_integer_)
      
      out
    })
  )
  
  res_info
  # A tibble with columns: wflow_id, n_metrics (some might be NA for errors)
  
  # Identify the "bad" ones
  bad_flows <- res_info %>%
    filter(is.na(n_metrics) | n_metrics == 0) %>%
    pull(wflow_id)
  
  bad_flows
  
  # Filter them out
  race_results_filtered <- race_results %>%
    filter(!wflow_id %in% bad_flows)
  
  # open a pdf device with a name
  
  pdf(paste0(output_dir,i,'_',crop,'_model_selection.pdf'))
  
  # show the results of models
  model_rmse <- autoplot(
    race_results_filtered,
    rank_metric = "rmse",  
    metric = "rmse",       
    select_best = TRUE,
    size = 5
  ) +
    labs(x = 'Model Rank', y = 'Root mean square error (RMSE)') +
    #scale_y_continuous(limits = c(1.2, 2.8)) +
    geom_text(aes(y = mean + 0.3, label = wflow_id),size = 4.5, angle = 90, hjust = 0.9) +
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text  = element_text(size =16),
      axis.title = element_text(size =16)
    )
  
  model_rmse
  
  ggsave(model_rmse, filename = paste0(output_dir,i,'_',crop,'_model_selection.png'))
  
  # show metrics
  best_wf<-race_results_filtered %>%
    rank_results(select_best = TRUE) %>%
    filter(.metric == "rmse") %>%
    filter(rank == 1) %>%
    droplevels() 
  
  best_wf_name <- best_wf$wflow_id
  
  
  #' 
  #' ## Finalize the final model
  #' 
  ## ----final-----------------------------------------------------------------------------------------------------------------------------------------
  best_results <- 
    race_results_filtered %>% 
    extract_workflow_set_result(best_wf_name) %>% 
    select_best(metric = "rmse")
  #best_results
  
  #rf_test_results <- 
  #  race_results_filtered %>% 
  #  extract_workflow(best_wf_name) %>% 
  #  finalize_workflow(best_results) %>% 
  #  last_fit(split = corn_split)
  
  #rf_test_metrics <- collect_metrics(rf_test_results)
  
  
  #' 
  #' 
  #' # Create the final model
  ## ----final_model-----------------------------------------------------------------------------------------------------------------------------------
  
  
  #---------------ML model--------#
  
  final_wf <- 
    race_results_filtered %>% 
    extract_workflow(best_wf_name) %>% 
    finalize_workflow(best_results) 
  
  #saveRDS(final_wf, file = 'data/soil/Irrigation_models/corn_irrigation/corn_irrigation_final_model.rds')
  
  final_rf_fit <- 
    final_wf %>% 
    fit(corn_train)
  
  saveRDS(final_rf_fit, file = paste0(output_dir,i,'_',crop,'_model_selection_fit_objective.rds'))
  
  library(vip)
  vip_plot <- final_rf_fit %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 20)
  vip_plot
  
  ggsave(vip_plot, filename = paste0(output_dir,i,'_',crop,'_model_selection_VIP.png'))
  
  dev.off() # close the pdf device
  
  
  ## --------------------- evaluation model ----------------------#
  
  # evaluate train and test, plus test irrigated vs. rainfed
  
  
  if (i == 'all') {
    
    corn_test_rainfed <- corn_test %>% filter(irrigated == 'rainfed')
    corn_test_irrigated <- corn_test %>% filter(irrigated == 'irrigated')
    
    all_predictions <- bind_rows(
      augment(final_rf_fit, new_data = corn_train) %>% 
        mutate(type = "train"),
      augment(final_rf_fit, new_data = corn_test) %>% 
        mutate(type = "test"),
      augment(final_rf_fit, new_data = corn_test_rainfed) %>% 
        mutate(type = "test_rainfed"),
      augment(final_rf_fit, new_data = corn_test_irrigated) %>% 
        mutate(type = "test_irrigated"))
    
  } else {
    
    all_predictions <- bind_rows(
      augment(final_rf_fit, new_data = corn_train) %>% 
        mutate(type = "train"),
      augment(final_rf_fit, new_data = corn_test) %>% 
        mutate(type = "test"))
    
  }
  
  rmse_combine <- all_predictions %>%
    group_by(type) %>%
    metrics(Yield_decomp_add, .pred) %>%
    dplyr::select(type,.metric,.estimate) %>%
    rename(metric = .metric, estimate = .estimate) %>%
    unite("type",type:metric, sep = '_') #%>%
  #pivot_wider(names_from = type, values_from = estimate)
  
  write.csv(rmse_combine, file = paste0(output_dir,i,'_',crop,'_model_selection_metrics.csv'))
  
  }

model_name <- c('irrigated','rainfed','all')

lapply(model_name, model_function)

}
