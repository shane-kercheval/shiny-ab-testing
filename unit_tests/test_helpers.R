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
    daily_first_time_path_num_users <- website_traffic__to_daily_num_users(website_traffic, top_n_paths = 10, only_first_time_visits=TRUE)


    daily_first_time_2paths_num_users <- website_traffic__to_daily_num_users(website_traffic, top_n_paths = 2, only_first_time_visits=TRUE)


    cohort_first_time_num_users <- website_traffic__to_cohort_num_users(website_traffic, only_first_time_visits=TRUE)
    cohort_first_time_path_num_users <- website_traffic__to_cohort_num_users(website_traffic, top_n_paths = 10, only_first_time_visits=TRUE)
    
    # first, let's check that top_n_paths gives expected paths and levels (10 should include all paths, and 
    # add an 'Other' level)
    
    # check all paths exist
    expected_paths <- c( "example.com", "example.com/features", "example.com/pricing", "example.com/demo")
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

test_that("test_helpers: experiments__get_experiment_conversion_rates", {

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
    # 1) make sure the dates are relative to today (experiments__get_experiment_conversion_rates filter out based on\
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
    
    user_conversion_rates <- experiments__get_experiment_conversion_rates(experiment_traffic,
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

    

experiments__get_summary <- function(experiment_info, experiment_traffic, website_traffic, attribution_windows, conversion_rates) {
# distinction between end_date and last_event_date is that end_date is the date of the last event we have in
# the entire experiment_traffic dataset, but we exclude people who have joined the experiment recently
# according to the attribution windows, so `last_event_date` is the date of the last event that was included
# and counted towards the successes/trials
# so the test could still be running but we only look

    # refactored this to a method because we'll use it for the actual experiments, but then we'll modify website
    # traffic to mock a PRIOR dataset for bayesian calculations.
    # specifically, based on how many days of prior data we want, we'll transform website_traffic
    # to look like experiment_traffic, but based on the prior dates.

    get_baseline_summary <- function(experiment_info, experiment_traffic, attribution_windows, conversion_rates) {
        ##########################################################################################################
        # Add trials i.e. count of people in experiment
        # TRIALS (i.e. count of peeople in experiment/varation) needs to exclude people who have entered into
        # the experiment less than x day ago, where x is the attribution window per metric
        # so the Experiment Summary will be per experiment/metric (the variation_data will be in the columns)
        ##########################################################################################################

        experiment_start_end_dates <- experiment_traffic %>%
            group_by(experiment_id) %>%
            summarise(start_date = min(first_joined_experiment),
                      end_date = max(first_joined_experiment))
        
        # this will duplicate each row in experiment_traffic for each metric
        experiment_summary <- inner_join(experiment_traffic, attribution_windows, by='experiment_id') %>%
            # we only want the people who have had enough time to convert, given the attribution window for a
            # given metric (i.e. exclude people who join within the attribution window relative to today)
            filter(first_joined_experiment < Sys.Date() - attribution_window) %>%
            mutate(metric_id = fct_reorder(metric_id, attribution_window)) %>%
            inner_join(experiment_info,
                       by=c('experiment_id', 'variation')) %>%
            group_by(experiment_id, metric_id, is_baseline) %>%
            summarise(last_event_date = max(first_joined_experiment),
                      trials = n()) %>%
            ungroup() %>%
            # on the offchance the start dates (or end dates) are different between the baseline/metric/variation 
            # (e.g. started the experiment at ~midnight and 1 person went into the baseline on day x and the next
            # went
            # into the variation on day x+1)
            group_by(experiment_id, metric_id) %>%
            mutate(last_event_date = max(last_event_date)) %>%
            ungroup() %>%
            # now format so there is 1 row per experiment
            spread(is_baseline, trials)
            
            # now we need to rename `TRUE` and `FALSE` columns to baseline/variant
            # but, if this is "prior" experiment data, theren't won't be a `FALSE`, so we need to check
            if("FALSE" %in% colnames(experiment_summary)) {
                
                experiment_summary <- experiment_summary %>%
                    rename(baseline_trials=`TRUE`,
                           variant_trials=`FALSE`) %>%
                    select(experiment_id, last_event_date, metric_id, baseline_trials, variant_trials)
                
            } else {
                
                experiment_summary <- experiment_summary %>%
                    rename(baseline_trials=`TRUE`) %>%
                    select(experiment_id, last_event_date, metric_id, baseline_trials)
                
                    
            }

        experiment_summary <- inner_join(experiment_summary, experiment_start_end_dates, by='experiment_id') %>%
            select(experiment_id, start_date, end_date, everything()) %>%
            # most recent ended (which is really just the last event, so it may not be stopped), so if
            # there are multiple experiments that are still running, sort by the most recent started
            arrange(desc(end_date), desc(start_date), metric_id)

        stopifnot(!any(duplicated(experiment_summary %>% select(experiment_id, metric_id))))

        ##########################################################################################################
        # Add successes and conversion rates
        ##########################################################################################################
        
        # experiments__get_experiment_conversion_rates will exclude traffic based on first_joined_experiment &
        # attribution windows like we did above
        experiment_conversion_rates <- experiments__get_experiment_conversion_rates(experiment_traffic,
                                                                                    attribution_windows,
                                                                                    conversion_rates) %>%
            filter(converted_within_window) %>%
            count(experiment_id, variation, metric_id) %>%
            rename(successes=n) %>%
            inner_join(experiment_info, by=c('experiment_id', 'variation')) %>%
            select(-variation) %>%
            spread(is_baseline, successes)
        
        # now we need to rename `TRUE` and `FALSE` columns to baseline/variant
        # but, if this is "prior" experiment data, theren't won't be a `FALSE`, so we need to check
        if("FALSE" %in% colnames(experiment_conversion_rates)) {
            
            experiment_summary <- inner_join(experiment_summary,
                                             experiment_conversion_rates,
                                             by=c('experiment_id', 'metric_id')) %>%
                rename(baseline_successes=`TRUE`,
                       variant_successes=`FALSE`) %>% 
                mutate(baseline_conversion_rate=baseline_successes / baseline_trials,
                       variant_conversion_rate=variant_successes / variant_trials,
                       percent_change_from_baseline = (variant_conversion_rate - baseline_conversion_rate) / baseline_conversion_rate)


    
        } else {
            
            experiment_summary <- inner_join(experiment_summary,
                                             experiment_conversion_rates,
                                             by=c('experiment_id', 'metric_id')) %>%
                rename(baseline_successes=`TRUE`) %>%
                mutate(baseline_conversion_rate=baseline_successes / baseline_trials)
        }
        
        

        return (experiment_summary)
    }

    experiment_summary <- get_baseline_summary(experiment_info, experiment_traffic, attribution_windows, conversion_rates)
    
    ##########################################################################################################
    # Add P-Value Information
    ##########################################################################################################
    
    p_values <- pmap(list(experiment_summary$baseline_successes,
                          experiment_summary$baseline_trials,
                          experiment_summary$variant_successes,
                          experiment_summary$variant_trials),
                     function(bs, bt, vs, vt) get_p_values_info(bs, bt, vs, vt))
 
    experiment_summary_pvalues <- experiment_summary
    
    experiment_summary$p_value <- map_dbl(p_values, ~ .['p_value'])
    experiment_summary$cr_diff_estimate <- map_dbl(p_values, ~ .['cr_diff_estimate'])
    experiment_summary$p_value_conf_low <- map_dbl(p_values, ~ .['conf.low'])
    experiment_summary$p_value_conf_high <- map_dbl(p_values, ~ .['conf.high'])

    # experiment_summary$p_value_conf_low / experiment_summary$baseline_conversion_rate
    # experiment_summary$p_value_conf_high / experiment_summary$baseline_conversion_rate

    ##########################################################################################################
    # Add Bayesian Information
    ##########################################################################################################
    
    
    # we need to modify website_traffic to mock experiment_traffic in order to genearate a PRIOR dataset for
    # bayesian calculations.
    # specifically, based on how many days of prior data we want, we'll transform website_traffic
    # to look like experiment_traffic, but based on the prior dates.
    # NOTE: Well only want to use the paths that the experiments where in
    
    day_of_prior_information <- 15
    
    experiment_prior_dates <- experiment_summary %>%
        select(experiment_id, start_date, metric_id) %>%
        inner_join(attribution_windows, by = c('experiment_id', 'metric_id')) %>%
        group_by(experiment_id) %>%
        # well take the min calculated start date so we allow enough time for the largest attribution window
        summarise(prior_start_date = min(start_date - day_of_prior_information - attribution_window - 1),
                  prior_end_date = prior_start_date + day_of_prior_information)
    
    experiment_prior_paths <- distinct(experiment_traffic %>% select(experiment_id, path))
    
    prior_data <- data.frame(user_id=NULL, first_joined_experiment=NULL, experiment_id=NULL, variation=NULL)
    for(experiment in unique(experiment_info$experiment_id)) {
        
        prior_start_date <- (experiment_prior_dates %>% filter(experiment_id == experiment))$prior_start_date
        prior_end_date <- (experiment_prior_dates %>% filter(experiment_id == experiment))$prior_end_date
        
        prior_paths <- experiment_prior_paths %>% filter(experiment_id == experiment)
        variation_name <- (experiment_info %>% 
            filter(experiment_id == experiment,
                   is_baseline))$variation
        
        prior_data <- rbind(prior_data,
                            website_traffic %>% 
                                filter(visit_date >= prior_start_date & visit_date <= prior_end_date,
                                       path %in% prior_paths$path) %>%
                                group_by(user_id) %>%
                                summarise(first_joined_experiment = min(visit_date)) %>%
                                mutate(experiment_id = experiment,
                                       variation = variation_name))
    }

    prior_summary <- get_baseline_summary(experiment_info=experiment_info,
                                          experiment_traffic=prior_data,
                                          attribution_windows=attribution_windows,
                                          conversion_rates=conversion_rates)
    prior_summary <- prior_summary %>%
        mutate(prior_alpha=baseline_successes,
               prior_beta=baseline_trials - baseline_successes) %>%
        select(experiment_id, metric_id, prior_alpha, prior_beta)

    experiment_summary <- inner_join(experiment_summary,
                                     prior_summary,
                                     by = c("experiment_id", "metric_id"))

    experiment_summary <- experiment_summary %>%
        mutate(baseline_alpha = prior_alpha + baseline_successes,
               baseline_beta = prior_beta + (baseline_trials - baseline_successes),
               variant_alpha = prior_alpha + variant_successes,
               variant_beta = prior_beta + (variant_trials - variant_successes))
    
    return (experiment_summary)
}



    credible_interval_approx <- function(alpha_a, beta_a, alpha_b, beta_b) {
    # https://github.com/dgrtwo/empirical-bayes-book/blob/master/bayesian-ab.Rmd
        u1 <- alpha_a / (alpha_a + beta_a)
        u2 <- alpha_b / (alpha_b + beta_b)
        var1 <- as.double(alpha_a) * beta_a / ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
        var2 <- as.double(alpha_b) * beta_b / ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
        mu_diff <- u2 - u1
        sd_diff <- sqrt(var1 + var2)
        
        # in D.R. code, the first player had a higher probability but a negative estimate (i.e. negative difference in conversion rate, mu_diff)
        # This doesn't make sense, so we'll 1) use the first player as the A group and the second as the B group), so B-A 
        # which gives the expected intervals (but flips the posterior probability), and 2) use 1-pnorm(...) to get the correct posterior probability
        c(posterior = 1 - pnorm(0, mu_diff, sd_diff),
                cr_diff_estimate = mu_diff,
                conf.low = qnorm(.025, mu_diff, sd_diff),
                conf.high = qnorm(.975, mu_diff, sd_diff))
    }
    
    
    plot_bayesian <- function(prior_alpha,
                              prior_beta,
                              baseline_alpha,
                              baseline_beta,
                              variant_alpha,
                              variant_beta,
                              show_prior_distribution=TRUE) {

            alpha_vector <- c(baseline_alpha, variant_alpha, prior_alpha)
            beta_vector <-  c(baseline_beta, variant_beta, prior_beta)

            if(show_prior_distribution) {

                x_min <- min(qbeta(0.001, alpha_vector, beta_vector))
                x_max <- max(qbeta(0.999, alpha_vector, beta_vector))
            
            } else {
            
                x_min <- min(qbeta(0.001, alpha_vector[1:2], beta_vector[1:2]))
                x_max <- max(qbeta(0.999, alpha_vector[1:2], beta_vector[1:2]))
            }


            x_axis_spread <- x_max - x_min

            # depending on the where we want to graph and how spread out the values are, we will want to get more/less granualar with our plot

            distro_names <- c("Baseline", "Variant", "Prior")
            distros <- data_frame(alpha = alpha_vector,
                                  beta = beta_vector,
                                  group = distro_names) %>%
                group_by(alpha, beta, group) %>%
                do(data_frame(x = seq(x_min, x_max, x_axis_spread / 1000))) %>%
                ungroup() %>%
                mutate(y = dbeta(x, alpha, beta),
                       Parameters = factor(paste0(group, ": alpha= ", alpha, ", beta= ", beta)))



            x_axis_break_steps <- 0.05


            if(x_axis_spread <= 0.02) {

                x_axis_break_steps <- 0.001

            } else if(x_axis_spread <= 0.05) {

                x_axis_break_steps <- 0.005

            } else if(x_axis_spread <= 0.15) {

                x_axis_break_steps <- 0.01

            } else if(x_axis_spread <= 0.5) {

                x_axis_break_steps <- 0.02
            }

            custom_colors <- rev(hue_pal()(3))

            if(!show_prior_distribution) {

                distros <- distros %>%
                    filter(!str_detect(Parameters, "Prior"))

                custom_colors <- custom_colors[1:2]
            }

            baseline_cred_low <- qbeta(0.025, baseline_alpha, baseline_beta)
            baseline_cred_high <- qbeta(0.975, baseline_alpha, baseline_beta)

            variant_cred_low <- qbeta(0.025, variant_alpha, variant_beta)
            variant_cred_high <- qbeta(0.975, variant_alpha, variant_beta)


            cia <- credible_interval_approx(alpha_a=baseline_alpha,
                                            beta_a=baseline_beta,
                                            alpha_b=variant_alpha,
                                            beta_b=variant_beta)
            percent_of_time_b_wins <- cia['posterior']

            # a_cr_simulation <- rbeta(1e6, baseline_alpha, baseline_beta)
            # b_cr_simulation <- rbeta(1e6, variant_alpha, variant_beta)
            # percent_of_time_b_wins <- mean(b_cr_simulation > a_cr_simulation)

            max_distros_20th <- max(distros$y) / 20
            plot_object <- ggplot(data=distros, aes(x, y, color = Parameters)) +
                geom_line() +
                geom_area(aes(fill=Parameters, group=Parameters), alpha=0.3, position = 'identity') +
                geom_errorbarh(aes(xmin = baseline_cred_low, xmax = baseline_cred_high, y = max_distros_20th * -1), height = max_distros_20th * 0.75, color = custom_colors[1], alpha=0.3) + 
                geom_errorbarh(aes(xmin = variant_cred_low, xmax = variant_cred_high, y = max_distros_20th * -2), height = max_distros_20th * 0.75, color = custom_colors[2], alpha=0.3) + 
                scale_x_continuous(breaks = seq(0, 1, x_axis_break_steps),
                                   labels = percent_format()) +
                theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
                coord_cartesian(xlim=c(x_min, x_max)) +
                labs(title='Posterior/Updated Probability Distributions of Baseline & Variant',
                     x="Conversion Rates",
                     y="Density of beta") +
                scale_fill_manual(values=custom_colors) +
                scale_color_manual(values=custom_colors)

        return (plot_object)
    }

    
    experiment <- unique(experiment_summary$experiment_id)[1]
    metric <- 'Sign Up'
    local_experiment <-  experiment_summary %>%
        filter(experiment_id  == experiment & metric_id == metric)
    plot_bayesian(prior_alpha=local_experiment$prior_alpha,
                  prior_beta=local_experiment$prior_beta,
                  baseline_alpha=local_experiment$baseline_alpha,
                  baseline_beta=local_experiment$baseline_beta,
                  variant_alpha=local_experiment$variant_alpha,
                  variant_beta=local_experiment$variant_beta)


})
