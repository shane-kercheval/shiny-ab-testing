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
