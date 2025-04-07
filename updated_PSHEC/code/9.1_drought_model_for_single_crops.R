# Run drought models 
# test models:
#i.	Separate Corn, Cotton, sorghum, soybeans, wheat; 
#ii.	Combined model for barley, oats, peanuts, 
#iii.	Combined model for barley, oats, peanuts, sugarbeets, and tobacco
#iv.	A single big model including all crops, except sugarbeets and tobacco (due to low number of observations)
#v.	A single big model including all crops

# this file is for individual crops

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

# use FarmLbsN_perAc_mean for oats, sugarbeets, tobacco

input_dir <- 'data/merged_drought_data/'

group_function <- function(LAR_name, ...) {
  
  crop_name <- c(...)
  
  # create a name for the group
  group_name <- paste(crop_name, collapse = '_')
  
  cat("Start the modeling for:", group_name, "\n")
  
  # Define the output directory ------####
  output_dir <- paste0('models/drought_models/',group_name,'_',LAR_name,'_model/')
  
  # Ensure the directory exists before the loop starts
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  
  #' 
  #' # Read in data
  #' 
  ## ----data------------------------------------------------------------------------------------------------------------------------------------------
  
  # read yield data, weather data + soil type, drainage class, and slope
  # read and combine data for crops
  
  data_list<- lapply(crop_name, function(crop) {
    
    df <- readRDS(paste0(input_dir,LAR_name,'/',crop,'_',LAR_name,'_DSCI_400_merged_drought_data.rds'))
    
    # conditionally change column names so that all crops have the same column names
    
    if (crop %in% c('OATS','SUGARBEETS','TOBACCO')) {
      df <- df %>%
        dplyr::rename(Ninput_kgN.ha.yr_mean = FarmLbsN_perAc_mean)
    } else if (crop %in% c('SOYBEANS','PEANUTS')) {
      df <- df %>%
        dplyr::mutate(Ninput_kgN.ha.yr_mean = 0)
    }
    
    return(df)
    
  })
  
  data <- dplyr::bind_rows(data_list)
  
  # convert state_alpha from characters to factors 
  data$state_alpha <- as.factor(data$state_alpha)
  #data$crop <- as.factor(data$crop)
  
  # Reorder drainage levels as this is an ordinal variable
  # the order of drainage class is as follows
  data <- data %>%
    mutate(ssurgo_drainage_class_mode = factor(ssurgo_drainage_class_mode, levels = c("Excessively drained", "Somewhat excessively drained",
                                                                                      "Well drained", "Moderately well drained",
                                                                                      "Somewhat poorly drained", "Poorly drained",
                                                                                      "Very poorly drained")))
  
  # print the progress
  cat("Start the workflow for:", group_name,"_",LAR_name,"\n")
  
  #' 
  #' # Start the tidymodels workflow
  #' 
  #' ## 1. Split the data using the 80:20 for training-to-test and resample the training set using five repeats of 10-fold cross-validation
  ## ----splitdata-------------------------------------------------------------------------------------------------------------------------------------
  
  # Splitting the data into 80% training and 20% testing
  # Using strata to make sure the resamples have equivalent proportions as the original data among crops
  set.seed(1501)  # Set a seed for reproducibility
  corn_split <- initial_split(data, prop = 0.8)

  
  # Extract the training and testing sets
  corn_train <- training(corn_split)
  corn_test  <- testing(corn_split)
  
  # Resample the training set using five repeats of 10-fold cross-validation
  set.seed(1502)
  corn_folds <- vfold_cv(corn_train, repeats = 5)
  
  #' 
  #' ## 2. create the data preprocessing recipe
  #' 
  ## ----recipe----------------------------------------------------------------------------------------------------------------------------------------
  # the basic recipe contains all available variables
  basic_rec <- 
    recipe(Yield_change ~ 
             #crop + # remove crop variable for individual crops
             ssurgo_om_mean + 
             Mean.percent.irrigated + # add irrigation percentage
             ssurgo_clay_mean +  ssurgo_sand_mean + 
             #ssurgo_silt_mean +
             #ssurgo_awc_mean + ssurgo_aws_mean + ssurgo_fifteenbar_mean +
             #ssurgo_cec_mean +
             ssurgo_h +  
             Ninput_kgN.ha.yr_mean + # add nitrogen input
             annual_cum_ppt + 
             annual_tmean + 
             annual_GDD + # add selected weather variable
             ssurgo_slope_mean + ssurgo_drainage_class_mode + 
             DSCI.mean +
             GEOID + state_alpha, data =corn_train) %>%
    add_role(ssurgo_om_mean, new_role = 'om') %>% # add the role of om so that it won't be removed in the following steps
    update_role(GEOID, new_role = 'ID') # update the role of GEOID as it will not be used as a predictor 
  
  
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
    #step_dummy(crop, one_hot = TRUE) %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    # normalize all numeric predictors, including dummy variables
    step_normalize(has_role("numeric_original") | starts_with("state_alpha_")|starts_with("ssurgo_drainage_class_")) %>%
    # interactions
    step_interact(terms = ~ has_role("numeric_original") * has_role("numeric_original"), sep = '*') %>%
    step_interact(terms = ~ has_role("numeric_original") * starts_with('state_alpha_'), sep = '*')
  
  
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
    #step_dummy(crop, one_hot = TRUE) %>%
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
    #step_dummy(crop, one_hot = TRUE) %>%
    step_integer(ssurgo_drainage_class_mode) %>%
    step_normalize(all_numeric_predictors())
  
  # control for collinearity, convert categorical variables to numeric, and normalize all variables
  norm_corr_dum_rec <-
    basic_rec %>%
    # handle multicollinearity
    step_corr(all_numeric_predictors(),-has_role('om'), threshold = 0.7) %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    #step_dummy(crop, one_hot = TRUE) %>%
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
    step_dummy(crop, one_hot = TRUE) %>%
    step_dummy(state_alpha, one_hot = TRUE) %>%
    # normalize all numeric predictors, including dummy variables
    step_normalize(has_role("numeric_original") | starts_with("state_alpha_")|starts_with("crop_")|starts_with("ssurgo_drainage_class_"))
  
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
  #cart_model <- 
  #  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  #  set_engine("rpart") %>% 
  #  set_mode("regression") # a regression tree model 
  
  #rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  #  set_engine("ranger") %>% 
  #  set_mode("regression")
  
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
  #svm_linear_model <- svm_linear(cost = tune()) %>%
  #  set_mode("regression") %>%
  #  set_engine("kernlab")
  # radial basis kernel
  #svm_rbf_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  #  set_mode("regression") %>%
  #  set_engine("kernlab")
  
  # Bayesian Additive Regression Trees (BART)
  #bart_model <- bart(trees = tune(), prior_terminal_node_coef = tune()) %>%
  #  set_mode("regression") %>%
  #  set_engine("dbarts")
  
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
        #cart = normalize_rec, 
        #rf = normalize_rec,
        #nnet = normalize_rec,
        xgb = norm_dum_rec,
        xgb_corr = norm_corr_dum_rec,
        cubist = norm_dum_rec,
        #svm_l = norm_corr_dum_rec_svm,
        #svm_rbf = norm_corr_dum_rec_svm,
        #bart = norm_dum_rec,
        gam = norm_corr_rec
      ),
      models = list(
        lasso_model, lasso_model, #lasso_model, lasso_model,
        #cart_model,
        #rf_model,
        #nnet_model,
        xgb_model,
        xgb_model,
        cubist_model,
        #svm_linear_model,
        #svm_rbf_model,
        #bart_model,
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
  cl <- makePSOCKcluster(parallel::detectCores()-8)  # Use defined number of core
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
  
  saveRDS(race_results_filtered, paste0(output_dir,group_name,'_',LAR_name,'_race_results.rds'))
  
  # open a pdf device with a name
  
  #pdf(paste0(output_dir,i,'_',crop,'_model_selection.pdf'))
  
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
  
  #model_rmse
  
  ggsave(model_rmse, filename = paste0(output_dir,group_name,'_',LAR_name,'_model_selection.png'))
  
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
  
  saveRDS(final_rf_fit, file = paste0(output_dir,group_name,'_',LAR_name,'_model_selection_fit_objective.rds'))
  
  library(vip)
  
  # Extract the fitted model from the workflow
  fit_model <- extract_fit_parsnip(final_rf_fit)
  
  # Check if variable importance is available
  if (!is.null(fit_model$fit$variable.importance)) {
    vip_plot <- vip(fit_model, num_features = 20)
    
    ggsave(vip_plot, filename = paste0(output_dir,group_name,'_',LAR_name,'_model_selection_VIP.png'))
    
  } else {
    message("Variable importance is not available for this model; skipping vip plot.")
  }
  
  #dev.off() # close the pdf device
  
  
  ## --------------------- evaluation model ----------------------#
  
  # evaluate train and test
    
    all_predictions <- bind_rows(
      augment(final_rf_fit, new_data = corn_train) %>% 
        mutate(type = "train"),
      augment(final_rf_fit, new_data = corn_test) %>% 
        mutate(type = "test"))
    
    rmse_combine <- all_predictions %>%
      group_by(type) %>%
      metrics(Yield_decomp_add, .pred) %>%
      dplyr::select(type,.metric,.estimate) %>%
      rename(metric = .metric, estimate = .estimate) %>%
      unite("type",type:metric, sep = '_') #%>%
    #pivot_wider(names_from = type, values_from = estimate)

  write.csv(rmse_combine, file = paste0(output_dir,group_name,'_',LAR_name,'_model_selection_metrics.csv'))
  
}


# Define the groups as a list
groups_list <- list(
  c('CORN'),
  c('SOYBEANS'),
  c('WHEAT'),
  c('COTTON'),
  c('SORGHUM')
)

LAR_names <- c('LAR_0.2','LAR_0.3')

# Create all combinations of groups and LAR_names
params <- crossing(group = groups_list, LAR_name = LAR_names)

# Iterate over each combination with pwalk
pwalk(params, function(group, LAR_name) {
  # Call your function with LAR_name and the unpacked group crops
  do.call(group_function, c(list(LAR_name), as.list(group)))
})

