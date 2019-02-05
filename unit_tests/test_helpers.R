library('testthat')
library(tidyverse)
library(lubridate)
library(scales)

source('../shiny-app/r_scripts/helpers.R', chdir=TRUE)
source('../shiny-app/r_scripts/plot_helpers.R', chdir=TRUE)
source('unit_test_helpers.R')

# to run from command line, use:
# library('testthat')
# test_file("test_helpers.R")

##############################################################################################################
# Misc Helpers
##############################################################################################################
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

test_that("Misc Helpers: calculate_total_sample_size", {

    # test pre-calculated examples
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
test_that("test_helpers: website_traffic__get_user_first_visit", {

    website_traffic <- as.data.frame(read_csv('data/cached_simulated_data/website_traffic.csv'))

    users_first_visit <- website_traffic__get_user_first_visit(website_traffic)

    # this is another way of creating the data, although it is MUCH slower (almost too slow for a unit test)
    # could speed it up if we don't want to test path
    expected_first_visits <- website_traffic %>%
        group_by(user_id) %>%
        summarise(first_visit = min(visit_date),
                  path = path[visit_date == first_visit])

    expect_true(all(users_first_visit %>% arrange(user_id) == expected_first_visits %>% arrange(user_id)))

})

test_that("test_helpers: website_traffic__to_xxx_num_users", {

    website_traffic <- as.data.frame(read_csv('data/cached_simulated_data/website_traffic.csv'))

    daily_first_time_num_users <- website_traffic__to_daily_num_users(website_traffic, only_first_time_visits=TRUE)
    write.csv(daily_first_time_num_users, file='data/helpers/website_traffic__to_xxx_num_users/daily_first_time_num_users.csv', row.names = FALSE)
    daily_first_time_path_num_users <- website_traffic__to_daily_num_users(website_traffic, top_n_paths = 10, only_first_time_visits=TRUE)
    write.csv(daily_first_time_path_num_users, file='data/helpers/website_traffic__to_xxx_num_users/daily_first_time_path_num_users.csv', row.names = FALSE)


    daily_first_time_2paths_num_users <- website_traffic__to_daily_num_users(website_traffic, top_n_paths = 2, only_first_time_visits=TRUE)
    write.csv(daily_first_time_2paths_num_users, file='data/helpers/website_traffic__to_xxx_num_users/daily_first_time_2paths_num_users.csv', row.names = FALSE)


    cohort_first_time_num_users <- website_traffic__to_cohort_num_users(website_traffic, only_first_time_visits=TRUE)
    write.csv(cohort_first_time_num_users, file='data/helpers/website_traffic__to_xxx_num_users/cohort_first_time_num_users.csv', row.names = FALSE)
    cohort_first_time_path_num_users <- website_traffic__to_cohort_num_users(website_traffic, top_n_paths = 10, only_first_time_visits=TRUE)
    write.csv(cohort_first_time_path_num_users, file='data/helpers/website_traffic__to_xxx_num_users/cohort_first_time_path_num_users.csv', row.names = FALSE)

    # first, let's check that top_n_paths gives expected paths and levels (10 should include all paths, and
    # add an 'Other' level)

    # check all paths exist
    expected_paths <- c("example.com", "example.com/features", "example.com/pricing", "example.com/demo")
    expect_equal(as.character(sort(unique(daily_first_time_path_num_users$path))), expected_paths)
    # check expected levels
    expect_equal(levels(daily_first_time_path_num_users$path), c(expected_paths, 'Other'))

    # check top 2 paths exist, plus Other
    expected_paths <- c( "example.com", "example.com/features", "Other")
    expect_equal(as.character(sort(unique(daily_first_time_2paths_num_users$path))), expected_paths)
    # check expected levels
    expect_equal(levels(daily_first_time_2paths_num_users$path), expected_paths)

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

    plot_object <- website_traffic__plot_traffic(website_traffic,
                                                 only_first_time_visits=TRUE,
                                                 is_weekly=TRUE,
                                                 filter_year_end_beginning_weeks=TRUE,
                                                 top_n_paths=NULL)
    plot_object %>% test_save_plot(file='data/plot_helpers/website_traffic__plot_traffic/first-visits-weekly-filter.png')

    plot_object <- website_traffic__plot_traffic(website_traffic,
                                                 only_first_time_visits=TRUE,
                                                 is_weekly=TRUE,
                                                 filter_year_end_beginning_weeks=TRUE,
                                                 top_n_paths=10)  # too high, but should still work.
    plot_object %>% test_save_plot(file='data/plot_helpers/website_traffic__plot_traffic/first-visits-weekly-filter_top_10_paths.png')

    plot_object <- website_traffic__plot_traffic(website_traffic,
                                                 only_first_time_visits=TRUE,
                                                 is_weekly=TRUE,
                                                 filter_year_end_beginning_weeks=TRUE,
                                                 top_n_paths=3)
    plot_object %>% test_save_plot(file='data/plot_helpers/website_traffic__plot_traffic/first-visits-weekly-filter_top_3_paths.png')
})

test_that("test_helpers: experiments__get_conversion_rates", {

    experiment_info <- as.data.frame(read_csv('data/cached_simulated_data/experiment_info.csv'))
    experiment_traffic <- as.data.frame(read_csv('data/cached_simulated_data/experiment_traffic.csv'))
    attribution_windows <- as.data.frame(read_csv('data/cached_simulated_data/attribution_windows.csv'))
    website_traffic <- as.data.frame(read_csv('data/cached_simulated_data/website_traffic.csv'))
    conversion_rates <- as.data.frame(read_csv('data/cached_simulated_data/conversion_rates.csv'))

    check_data__experiment_info(experiment_info)
    check_data__experiment_traffic(experiment_traffic, experiment_info)
    check_data__attribution_windows(attribution_windows, experiment_info)
    check_data__website_traffic(website_traffic)
    check_data__conversion_rates(conversion_rates, attribution_windows)

    # I want to manipulate the simulated dataset to
    # 1) make sure the dates are relative to today (experiments__get_conversion_rates filter out based on\
    #    Sys.Date())
    # 2) simulate people entering the experiment who already converted (so that we can ensure they are not
    #    counted as converted
    max_experiment_traffic_date <- max(experiment_traffic$first_joined_experiment)
    days_offset <- Sys.Date() - max_experiment_traffic_date

    experiment_traffic <- experiment_traffic %>%
        mutate(first_joined_experiment = first_joined_experiment + days_offset)

    conversion_rates <- conversion_rates %>%
        # the minus 1 will make it so some people will have already converted
        mutate(conversion_date = conversion_date + days_offset - 1)

    user_conversion_rates <- experiments__get_conversion_rates(experiment_traffic,
                                                                    attribution_windows,
                                                                    conversion_rates)

    expect_equal(min(user_conversion_rates$days_from_experiment_to_conversion), -1)

    # make sure no one who converted before they joined the experiment is counted as converted
    temp <- user_conversion_rates %>% filter(days_from_experiment_to_conversion < 0)
    expect_false(any(temp$converted_within_window))

    # make sure no one who converted after the attribution window is counted as converted
    temp <- user_conversion_rates %>% filter(first_joined_experiment + attribution_window < conversion_date)
    expect_false(any(temp$converted_within_window))

    # make sure no one that joined the experiment x days ago, where x is attribution window, is in the dataset
    expect_equal(nrow(user_conversion_rates %>%
                          filter(first_joined_experiment + attribution_window >= Sys.Date())),
                 0)

    # make sure the data is unique by experiment/metric/user
    expect_true(all((user_conversion_rates %>% count(experiment_id, metric_id, user_id))$n == 1))

})

test_that("test_helpers: private__filter_experiment_traffic_via_attribution", {

    experiment_info <- as.data.frame(read_csv('data/cached_simulated_data/experiment_info.csv'))
    experiment_traffic <- as.data.frame(read_csv('data/cached_simulated_data/experiment_traffic.csv'))
    attribution_windows <- as.data.frame(read_csv('data/cached_simulated_data/attribution_windows.csv'))

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <-max(experiment_traffic$first_joined_experiment)
    days_offset <- Sys.Date() - max_date

    experiment_traffic$first_joined_experiment <- experiment_traffic$first_joined_experiment + days_offset

    filtered_traffic <- private__filter_experiment_traffic_via_attribution(experiment_info,
                                                                           experiment_traffic,
                                                                           attribution_windows)

    # If we add the attribution window to the first-joined-experiment, the max we should get is Today
    # meaning we are filtering out `today - attribution-window`
    max_joined_per_metric <- filtered_traffic %>%
        mutate(joined_plus_attribution = first_joined_experiment + attribution_window) %>%
        group_by(metric_id) %>%
        summarise(max_joined_plus_attribution = max(joined_plus_attribution))

    # This might fail due to time-zones
    expect_true(all(Sys.Date() - max_joined_per_metric$max_joined_plus_attribution == 1))

    # for all experiments that ended before max attribution date (relative to today),
    # the end date should equal the end date on the filtered dataset
    start_end_dates <- experiment_traffic %>%
        group_by(experiment_id) %>%
        summarise(start_date = min(first_joined_experiment),
                  end_date = max(first_joined_experiment)) %>%
        filter(end_date < Sys.Date() - max(attribution_windows$attribution_window))

    start_end_dates_filtered <- filtered_traffic %>%
        group_by(experiment_id) %>%
        summarise(start_date = min(first_joined_experiment),
                  end_date = max(first_joined_experiment)) %>%
        filter(experiment_id %in% start_end_dates$experiment_id)

    expect_true(all(start_end_dates == start_end_dates_filtered))
})

test_that("test_helpers: experiments__get_summary", {

    experiment_info <- as.data.frame(read_csv('data/cached_simulated_data/experiment_info.csv'))
    experiment_traffic <- as.data.frame(read_csv('data/cached_simulated_data/experiment_traffic.csv'))
    attribution_windows <- as.data.frame(read_csv('data/cached_simulated_data/attribution_windows.csv'))
    website_traffic <- as.data.frame(read_csv('data/cached_simulated_data/website_traffic.csv'))
    conversion_rates <- as.data.frame(read_csv('data/cached_simulated_data/conversion_rates.csv'))

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <- max(max(experiment_traffic$first_joined_experiment),
                    max(conversion_rates$conversion_date),
                    max(website_traffic$visit_date))

    days_offset <- Sys.Date() - max_date

    experiment_traffic$first_joined_experiment <- experiment_traffic$first_joined_experiment + days_offset
    website_traffic$visit_date <- website_traffic$visit_date + days_offset
    conversion_rates$conversion_date <- conversion_rates$conversion_date + days_offset

    experiments_summary <- experiments__get_summary(experiment_info,
                                     experiment_traffic,
                                     website_traffic,
                                     attribution_windows,
                                     conversion_rates,
                                     days_of_prior_data=15)

    expect_equal(nrow(distinct(experiments_summary %>% select(experiment_id, start_date, end_date))),
                 length(unique(experiment_info$experiment_id)))

    expect_false(any(is.na(experiments_summary)))
    # for the first experiment, these should equal the attribution window plus 1 day padding the end-date is Today
    expect_true(all(experiments_summary$end_date - experiments_summary$last_join_date == c(3, 4, 6, 8, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0)))
    expect_true(all(distinct(experiments_summary %>% select(experiment_id, metric_id)) %>% arrange(experiment_id) == attribution_windows %>% select(-attribution_window)))

    expect_true(all(experiments_summary$baseline_conversion_rate == experiments_summary$baseline_successes / experiments_summary$baseline_trials))
    expect_true(all(experiments_summary$variant_conversion_rate == experiments_summary$variant_successes / experiments_summary$variant_trials))
    expect_true(all(experiments_summary$percent_change_from_baseline == (experiments_summary$variant_conversion_rate - experiments_summary$baseline_conversion_rate) / experiments_summary$baseline_conversion_rate))


    expect_true(all(with(experiments_summary, baseline_alpha == prior_alpha + baseline_successes)))
    expect_true(all(with(experiments_summary, baseline_beta == prior_beta + baseline_trials - baseline_successes)))

    expect_true(all(with(experiments_summary, variant_alpha == prior_alpha + variant_successes)))
    expect_true(all(with(experiments_summary, variant_beta == prior_beta + variant_trials - variant_successes)))
    expect_true(all(with(experiments_summary, bayesian_conf.low < bayesian_cr_diff_estimate & bayesian_cr_diff_estimate < bayesian_conf.high)))

    expect_true(all(with(experiments_summary, cr_diff_estimate == variant_conversion_rate - baseline_conversion_rate)))

    p_values <- with(experiments_summary, pmap_dbl(list(baseline_successes, baseline_trials, variant_successes, variant_trials),
        function(bs, bt, vs, vt) {
            prop.test(x=c(bs, vs), n=c(bt, vt))$p.value
        }
    ))
    expect_true(all(p_values == experiments_summary$p_value))

    write.csv(experiments_summary, 'data/helpers/experiments_summary.csv')

    ##########################################################################################################
    # plots
    ##########################################################################################################
    plot_object <- plot_bayesian(experiments_summary,
                                 experiment=unique(experiments_summary$experiment_id)[1],
                                 metric='Sign Up')

    plot_object %>% test_save_plot(file='data/plot_helpers/plot_bayesian/experiment_1_signup.png')
    
    plot_object <- plot_bayesian(experiments_summary,
                                 experiment=unique(experiments_summary$experiment_id)[1],
                                 metric='Sign Up',
                                 show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot_bayesian/experiment_1_signup_no_prior.png')
    
    
    plot_object <- plot_bayesian(experiments_summary,
                                 experiment="New Signup CTA Color",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = TRUE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot_bayesian/signup_color_pay.png')
    
    plot_object <- plot_bayesian(experiments_summary,
                                 experiment="New Signup CTA Color",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot_bayesian/signup_color_pay_no_prior.png')
    
    plot_object <- plot_bayesian(experiments_summary,
                                 experiment="Redesign Website",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = TRUE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot_bayesian/redesign_website_pay.png')
    
    plot_object <- plot_bayesian(experiments_summary,
                                 experiment="Redesign Website",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot_bayesian/redesign_website_pay_no_prior.png')
})
