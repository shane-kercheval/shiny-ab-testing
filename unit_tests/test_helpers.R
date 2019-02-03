library('testthat')
library(tidyverse)
library(lubridate)
library(scales)

source('../shiny-app/r_scripts/helpers.R', chdir=TRUE)
source('unit_test_helpers.R')

# to run from command line, use:
# library('testthat')
# test_file("test_helpers.R")


    # experiment_info <- as.data.frame(read_csv('data/cached_simulated_data/experiment_info.csv'))
    # attribution_windows <- as.data.frame(read_csv('data/cached_simulated_data/attribution_windows.csv'))
    # website_traffic <- as.data.frame(read_csv('data/cached_simulated_data/website_traffic.csv'))
    # experiment_traffic <- as.data.frame(read_csv('data/cached_simulated_data/experiment_traffic.csv'))
    # conversion_rate_data <- as.data.frame(read_csv('data/cached_simulated_data/conversion_rate_data.csv'))



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
test_that("test_helpers: website_traffic__to_xxx_num_users", {

    website_traffic <- as.data.frame(read_csv('data/cached_simulated_data/website_traffic.csv'))

    daily_first_time_num_users <- website_traffic__to_daily_num_users(website_traffic, only_first_time_visits=TRUE)
    daily_first_time_path_num_users <- website_traffic__to_daily_num_users(website_traffic, per_path = TRUE, only_first_time_visits=TRUE)

    cohort_first_time_num_users <- website_traffic__to_cohort_num_users(website_traffic, only_first_time_visits=TRUE)
    cohort_first_time_path_num_users <- website_traffic__to_cohort_num_users(website_traffic, per_path = TRUE, only_first_time_visits=TRUE)
    
    # because we are only counting first-time visits, the sum across all dates should match regardless if
    # counted by daily/cohorted; the sum should also equal the number of distinct users in the dataset
    expect_equal(min(daily_first_time_num_users$visit_date), min(website_traffic$visit_date))
    expect_equal(sum(daily_first_time_num_users$num_users), length(unique(website_traffic$user_id)))
    expect_equal(min(daily_first_time_path_num_users$visit_date), min(website_traffic$visit_date))
    expect_equal(sum(daily_first_time_path_num_users$num_users), length(unique(website_traffic$user_id)))

    expect_equal(sum(cohort_first_time_num_users$num_users), length(unique(website_traffic$user_id)))
    expect_equal(sum(cohort_first_time_path_num_users$num_users), length(unique(website_traffic$user_id)))
})

test_that("test_helpers: website_traffic__plot_traffic", {

    website_traffic <- as.data.frame(read_csv('data/cached_simulated_data/website_traffic.csv'))

    website_traffic__plot_traffic <- function(website_traffic,
                                              only_first_time_visits=FALSE,
                                              is_weekly=TRUE,
                                              filter_year_end_beginning_weeks=TRUE,
                                              per_path=FALSE) {
        if(is_weekly) {
            
            cohort_format <- '%W'
            cohort_name <- "Week"
            
        } else {
            
            cohort_format <- '%m'
            cohort_name <- "Month"
        }
        
        caption <- ""
        if(only_first_time_visits) {
            
            title <- paste("First-Time User Visits Per", cohort_name)
            subtitle <- paste0("Represents the number of new users to the website for a given ", tolower(cohort_name),".")
            y_label <- paste0("First-Time User Visits (per ", cohort_name, ")")
            
        } else {
            title <- paste("Unique User Visits Per", cohort_name)
            subtitle <- paste0("Users are only counted once in the given ",
                               tolower(cohort_name),
                               "but same user may be represented in multiple ",
                               tolower(cohort_name), ".")
            y_label <- paste0("Unique User Visits (per ", cohort_name, ")")
        }
        
        num_users_data <- website_traffic__to_cohort_num_users(website_traffic,
                                                               cohort_format=cohort_format,
                                                               per_path = per_path,
                                                               only_first_time_visits = only_first_time_visits)
        
        if(is_weekly && filter_year_end_beginning_weeks) {
            
            num_users_data <- num_users_data %>% filter(!str_detect(string=cohort, pattern='-00') & !str_detect(string=cohort, pattern='-53'))
            caption <- "\nPartial weeks at the end and beginning of the year are excluded."
        }
        num_users_data %>%
            ggplot(aes(x=cohort, y=num_users, group = 1)) +
            geom_line() +
            geom_point() +
            expand_limits(y = 0) +
            geom_text(aes(label = prettify_numerics(num_users)), vjust = -0.5) +
            scale_y_continuous(labels = comma_format()) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
            labs(title = title,
                 subtitle = subtitle,
                 x = cohort_name,
                 y = y_label,
                 caption=caption)
    }

    plot_object <- website_traffic__plot_traffic(website_traffic,
                                                 only_first_time_visits=TRUE,
                                                 is_weekly=TRUE,
                                                 filter_year_end_beginning_weeks=TRUE,
                                                 per_path=FALSE)
    plot_object %>% test_save_plot(file='data/plot_helpers/website_traffic__plot_traffic/first-visits-weekly-filter.png')
    
  
        

        
    
    
    str_detect(string=, pattern='-01')
    
    
    
    cohort_first_time_path_num_users <- website_traffic__to_cohort_num_users(website_traffic, per_path = TRUE, only_first_time_visits=TRUE)


    
    
    

    
    top_x_paths
    

    
})

test_that('prettify_numerics', {

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=0, sd=0.001)),
                 c("1.37e-03", "-5.65e-04", "3.63e-04", "6.33e-04", "4.04e-04", "-1.06e-04", "1.51e-03", "-9.47e-05", "2.02e-03", "-6.27e-05"))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=0, sd=0.1)),
                 c(0.14, -0.06, 0.04, 0.06, 0.04, -0.01, 0.15, -0.01, 0.20, -0.01))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=0)),
                 c(1.4, -0.6, 0.4, 0.6, 0.4, -0.1, 1.5, -0.1, 2.0, -0.1))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=10)),
                 c(11.4, 9.4, 10.4, 10.6, 10.4, 9.9, 11.5, 9.9, 12.0, 9.9))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=100, sd=10)),
                 c(114, 94, 104, 106, 104, 99, 115, 99, 120, 99))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=1000, sd=100)),
                 c("1.14K", "0.94K", "1.04K", "1.06K", "1.04K", "0.99K", "1.15K", "0.99K", "1.2K", "0.99K"))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=10000, sd=1000)),
                 c("11.4K", "9.4K", "10.4K", "10.6K", "10.4K", "9.9K", "11.5K", "9.9K", "12K", "9.9K"))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=100000, sd=10000)),
                 c("113.7K", "94.4K", "103.6K", "106.3K", "104K", "98.9K", "115.1K", "99.1K", "120.2K", "99.4K" ))

    set.seed(42)
    expect_equal(prettify_numerics(rnorm(n=10, mean=1000000, sd=100000)),
                 c("1.14M", "0.94M", "1.04M", "1.06M", "1.04M", "0.99M", "1.15M", "0.99M", "1.2M", "0.99M"))
})
