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

# shift all dates relative to today so we can test excluding people (who recently entered into the
# experiment) based on the attribution window
max_date <- max(max(experiment_data$experiment_traffic$first_joined_experiment),
                #max(conversion_events$conversion_date),
                max(experiment_data$website_traffic$visit_date))

days_offset <- Sys.Date() - as.Date(max_date)

experiment_data$experiment_traffic$first_joined_experiment <- experiment_data$experiment_traffic$first_joined_experiment + days_offset
experiment_data$website_traffic$visit_date <- experiment_data$website_traffic$visit_date + days_offset
experiment_data$conversion_events$conversion_date <- experiment_data$conversion_events$conversion_date + days_offset

experiments_summary <- experiments__get_summary(experiment_data,
                                                days_of_prior_data=global__prior_number_of_days,
                                                confidence_level=global__confidence_level)
saveRDS(experiments_summary, file='processed_data/experiments_summary.RDS')
experiments_daily_summary <- experiments__get_daily_summary(experiment_data,
                                                            experiments_summary,
                                                            confidence_level=global__confidence_level)
saveRDS(experiments_daily_summary, file='processed_data/experiments_daily_summary.RDS')
