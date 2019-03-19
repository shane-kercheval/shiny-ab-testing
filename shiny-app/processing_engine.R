library(tidyverse)
library(lubridate)
library(scales)

source('r_scripts/helpers_check_data_integrity.R', chdir=TRUE)
source('r_scripts/helpers_plots.R', chdir=TRUE)
source('r_scripts/helpers_processing.R', chdir=TRUE)
source('r_scripts/definitions.R')

##############################################################################################################
# load data (in this case simulated data)
##############################################################################################################
experiment_data <- load_data()

experiments_summary <- experiments__get_summary(experiment_data,
                                                days_of_prior_data=global__prior_number_of_days,
                                                confidence_level=global__confidence_level)
saveRDS(experiments_summary, file='processed_data/experiments_summary.RDS')

experiments_daily_summary <- experiments__get_daily_summary(experiment_data,
                                                            experiments_summary,
                                                            confidence_level=global__confidence_level)
saveRDS(experiments_daily_summary, file='processed_data/experiments_daily_summary.RDS')

historical_conversion_rates <- get_historical_conversion_rates(experiment_data,
                                                               include_last_n_days=180,
                                                               exclude_last_n_days=30)
saveRDS(historical_conversion_rates, file='processed_data/historical_conversion_rates.RDS')

saveRDS(max(experiment_data$website_traffic$visit_date), file='processed_data/latest_website_traffic_datetime.RDS')
