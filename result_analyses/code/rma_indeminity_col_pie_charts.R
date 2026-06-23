library(dplyr)
library(ggplot2)
library(scales)
library(ggsci)


pie_chart_function <- function(crop) {
  
  data <- readRDS(file = paste0('data/rma_data/',crop, '_SOB_COL_summary_stats.rds'))
  
  data_mean <- data %>%
    select(GEOID, year, indemnity_col, cause_category) %>%
    group_by(cause_category) %>%
    summarise(indemnity_col_mean = mean(indemnity_col), .groups = 'drop') %>%
    arrange(desc(indemnity_col_mean)) %>%  
    mutate(cause_category = factor(cause_category, levels = cause_category))

  pie_plot <- ggplot(data_mean, aes(x = "", y = indemnity_col_mean, fill = cause_category)) +
    geom_bar(stat = "identity", width = 1, color = 'white', linewidth = 0.8) +
    coord_polar("y", start = 0) +
    scale_fill_d3(palette = "category20") +  
    labs(fill = "Cause Category",
        title = paste0("Indemnity by Cause Category (",crop,": 2000-2023)")) +
    theme_void() +
    theme(plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))
  
  
  ggsave(pie_plot, file = paste0('../result_analyses/intermediate_results/rma_pie_chart/',crop,'_mean_indeminity_COL_pie_chart.png'))
  
  
}

crops <- c('CORN','SOYBEANS','WHEAT','COTTON',
  'BARLEY','OATS','SORGHUM','PEANUTS',
  'SUGARBEETS','TOBACCO')

lapply(crops, pie_chart_function)
