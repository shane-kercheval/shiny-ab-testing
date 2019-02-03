library('testthat')
library(tidyverse)
library(lubridate)
library(scales)

source('../shiny-app/r_scripts/helpers.R', chdir=TRUE)
source('unit_test_helpers.R')

# to run from command line, use:
# library('testthat')
# test_file("test_helpers.R")


    # experiment_info <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_info.csv'))
    # attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv'))
    # website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))
    # experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv'))
    # conversion_rate_data <- as.data.frame(read_csv('../shiny-app/simulated_data/conversion_rate_data.csv'))



##############################################################################################################
# Misc Helpers
##############################################################################################################
test_that("Misc Helpers: calculate_total_sample_size", {
    
    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.1,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 7525)

    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 29711)

    result <- calculate_total_sample_size(original_conversion_rate=0.20,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 51165)

    result <- calculate_total_sample_size(original_conversion_rate=0.10,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 115526)
    
    
    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.2,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 1926)

    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.1,
                                          power=0.9,
                                          alpha=0.05)
    expect_true(result == 10073)
    
    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.1,
                                          power=0.9,
                                          alpha=0.01)
    expect_true(result == 14265)
    
    ##########################################################################################################
    # goes from .95 to 1.0
    ##########################################################################################################
    result <- calculate_total_sample_size(original_conversion_rate=0.95,
                                          percent_increase=(1-.95)/.95,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 304)
    ##########################################################################################################
    # this will give a value larger than 1.0, so 1.0 will be used, so it should be the same as the previous
    # results
    ##########################################################################################################
    new_result <- calculate_total_sample_size(original_conversion_rate=0.95,
                                              percent_increase=0.30,
                                              power=0.8,
                                              alpha=0.05)
    expect_true(result == new_result)
    
    ##########################################################################################################
    # test with `0` conversion rate
    ##########################################################################################################
    result <- calculate_total_sample_size(original_conversion_rate=0,
                                          percent_increase=0.10,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(is.infinite(result))
})

test_that("Misc Helpers: calculate_days_required", {

    daily_traffic <- 10000
    total_sample_size_required <- c(29711, 51165, 115526)  # from calculate_total_sample_size unit test 
    expected_days_required <- ceiling(total_sample_size_required / daily_traffic)
    days_required <- calculate_days_required(daily_traffic=daily_traffic,
                                             conversion_rates=c(0.3, 0.2, 0.1),
                                             percent_increase=0.05,
                                             power=0.8,
                                             alpha=0.05)
    
    expect_true(all(expected_days_required == days_required))
})

test_that("Misc Helpers: create_cohort", {
    base_date <- ymd('2019-01-01')
    dates <- base_date + 1:500

    expect_true(all(create_cohort(dates) == format(dates, '%Y-%W')))
    expect_true(all(create_cohort(dates, cohort_format = '%m') == format(dates, '%Y-%m')))
})

##############################################################################################################
# Historical Traffic & Conversion Rates
##############################################################################################################
test_that("test_helpers: create", {

    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))


    website_traffic__to_daily_num_users <- function(website_traffic, per_path=TRUE) {

        # counts UNIQUE user visits per day (and optionally per path)
        # per day and per path counts will not sum to the same total because a single user can visit multiple
        # pages in the same day, so they will be represented >=1 rows for a single day when per_path is TRUE,
        # but will only be count once when per_path is FALSE
        # the same day counts as 1)
        if(per_path) {
        
            # 
            return (website_traffic %>% count(visit_date, path) %>% rename(num_users=n_distinct(user_id)))    
        } else {
            
            return (website_traffic %>% count(visit_date))
        }
    }

    website_traffic__to_cohort_num_users <- function(website_traffic, cohort_format='%W') {
        
        # a single count/value of 1 represents multiple (>=1) visits to the same page by the same user in
        # the same day
        return (website_traffic %>% 
                    mutate(cohort=create_cohort(visit_date, cohort_format = cohort_format)) %>%
                    count(cohort, path))
    }
    
    temp <- website_traffic %>% count(visit_date, path) %>% rename(num_users=n)
    temp2 <- website_traffic %>% group_by(visit_date) %>% summarise(num_users=n_distinct(user_id))
    
    
    
    
    
    
    website_traffic__to_daily_traffic(website_traffic)
    
    website_traffic__to_cohort_traffic_per_path(website_traffic)
    
})



