library('testthat')
library(tidyverse)
library(lubridate)
library(scales)

source('../shiny-app/r_scripts/helpers_check_data_integrity.R', chdir=TRUE)
source('../shiny-app/r_scripts/helpers_plots.R', chdir=TRUE)
source('../shiny-app/r_scripts/helpers_processing.R', chdir=TRUE)

source('unit_test_helpers.R')

# to run from command line, use:
# library('testthat')
# test_file("test_helpers_processing.R")


test_that("website_traffic__get_user_first_visit", {
    context("helpers_processing::website_traffic__get_user_first_visit")

    experiment_data <- load_data()

    users_first_visit <- website_traffic__get_user_first_visit(experiment_data)

    # this is another way of creating the data, although it is MUCH slower (almost too slow for a unit test)
    # could speed it up if we don't want to test path
    expected_first_visits <- experiment_data$website_traffic %>%
        group_by(user_id) %>%
        summarise(first_visit = min(visit_date),
                  path = path[visit_date == first_visit])

    expect_true(all(users_first_visit %>% arrange(user_id) == expected_first_visits %>% arrange(user_id)))
})

test_that("website_traffic__to_xxx_num_users", {
    context("helpers_processing::website_traffic__to_xxx_num_users")

    experiment_data <- load_data()
    
    daily_first_time_num_users <- website_traffic__to_daily_num_users(experiment_data, only_first_time_visits=TRUE)
    write.csv(daily_first_time_num_users, file='data/helpers/website_traffic__to_xxx_num_users/daily_first_time_num_users.csv', row.names = FALSE)
    daily_first_time_path_num_users <- website_traffic__to_daily_num_users(experiment_data, top_n_paths = 10, only_first_time_visits=TRUE)
    write.csv(daily_first_time_path_num_users, file='data/helpers/website_traffic__to_xxx_num_users/daily_first_time_path_num_users.csv', row.names = FALSE)

    daily_first_time_2paths_num_users <- website_traffic__to_daily_num_users(experiment_data, top_n_paths = 2, only_first_time_visits=TRUE)
    write.csv(daily_first_time_2paths_num_users, file='data/helpers/website_traffic__to_xxx_num_users/daily_first_time_2paths_num_users.csv', row.names = FALSE)

    cohort_first_time_num_users <- website_traffic__to_cohort_num_users(experiment_data, only_first_time_visits=TRUE)
    write.csv(cohort_first_time_num_users, file='data/helpers/website_traffic__to_xxx_num_users/cohort_first_time_num_users.csv', row.names = FALSE)
    cohort_first_time_path_num_users <- website_traffic__to_cohort_num_users(experiment_data, top_n_paths = 10, only_first_time_visits=TRUE)
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
    expect_equal(min(daily_first_time_num_users$visit_date),
                 min(experiment_data$website_traffic$visit_date))
    expect_equal(sum(daily_first_time_num_users$num_users),
                 length(unique(experiment_data$website_traffic$user_id)))
    expect_equal(min(daily_first_time_path_num_users$visit_date),
                 min(experiment_data$website_traffic$visit_date))
    expect_equal(sum(daily_first_time_path_num_users$num_users),
                 length(unique(experiment_data$website_traffic$user_id)))

    expect_equal(sum(cohort_first_time_num_users$num_users),
                 length(unique(experiment_data$website_traffic$user_id)))
    expect_equal(sum(cohort_first_time_path_num_users$num_users),
                 length(unique(experiment_data$website_traffic$user_id)))
})

test_that("plot__website_traffic", {
    context("helpers_processing::plot__website_traffic")
    
    experiment_data <- load_data()

    plot_object <- plot__website_traffic(experiment_data,
                                                 only_first_time_visits=TRUE,
                                                 is_weekly=TRUE,
                                                 filter_year_end_beginning_weeks=TRUE,
                                                 top_n_paths=NULL)
    expect_false(is.null(plot_object))
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__website_traffic/first-visits-weekly-filter.png')

    plot_object <- plot__website_traffic(experiment_data,
                                                 only_first_time_visits=TRUE,
                                                 is_weekly=TRUE,
                                                 filter_year_end_beginning_weeks=TRUE,
                                                 top_n_paths=10)  # too high, but should still work.
    expect_false(is.null(plot_object))
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__website_traffic/first-visits-weekly-filter_top_10_paths.png')

    plot_object <- plot__website_traffic(experiment_data,
                                                 only_first_time_visits=TRUE,
                                                 is_weekly=TRUE,
                                                 filter_year_end_beginning_weeks=TRUE,
                                                 top_n_paths=3)
    expect_false(is.null(plot_object))
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__website_traffic/first-visits-weekly-filter_top_3_paths.png')
})

test_that("experiments__determine_conversions", {
    context("helpers_processing::experiments__determine_conversions")

    experiment_data <- load_data()

    # I want to manipulate the simulated dataset to
    # 1) make sure the dates are relative to today (experiments__determine_conversions filter out based on\
    #    Sys.Date())
    # 2) simulate people entering the experiment who already converted (so that we can ensure they are not
    #    counted as converted
    max_experiment_traffic_date <- max(experiment_data$experiment_traffic$first_joined_experiment)
    days_offset <- Sys.Date() - as.Date(max_experiment_traffic_date)

    experiment_data$experiment_traffic <- experiment_data$experiment_traffic %>%
        mutate(first_joined_experiment = first_joined_experiment + days_offset)

    experiment_data$conversion_events <- experiment_data$conversion_events %>%
        # the minus 1 will make it so some people will have already converted
        mutate(conversion_date = conversion_date + days_offset - 1)

    user_conversion_events <- experiments__determine_conversions(experiment_data)

    # make sure no one who converted before they joined the experiment is counted as converted
    temp <- user_conversion_events %>% filter(days_from_experiment_to_conversion < 0)
    expect_false(any(temp$converted_within_window))

    # make sure no one who converted after the attribution window is counted as converted
    temp <- user_conversion_events %>% filter(first_joined_experiment + days(attribution_window) < conversion_date)
    expect_false(any(temp$converted_within_window))

    # make sure no one that joined the experiment x days ago, where x is attribution window, is in the dataset
    expect_equal(nrow(user_conversion_events %>%
                          filter(first_joined_experiment + days(attribution_window) >= Sys.Date())),
                 0)

    # make sure the data is unique by experiment/metric/user
    expect_true(setequal(user_conversion_events %>%
                             count(experiment_id, metric_id, user_id) %>% 
                             get_vector('n'),
                         1))
    
    # check attribution windows
    check_attribution_windows <- left_join(experiment_data$attribution_windows,
              user_conversion_events %>%
                  mutate(metric_id = as.character(metric_id)) %>%
                  group_by(experiment_id, metric_id) %>%
                  summarise(single_window = length(unique(attribution_window)) == 1,
                            attribution_window_used = min(attribution_window)),
              by=c("experiment_id", "metric_id")) %>%
        mutate(windows_match = attribution_window == attribution_window_used)
    expect_false(any(is.na(check_attribution_windows$single_window)))
    expect_true(all(check_attribution_windows$windows_match))

    # check days_from_experiment_to_conversion
    expect_identical(user_conversion_events$days_from_experiment_to_conversion,
                     as.numeric(difftime(user_conversion_events$conversion_date, 
                                         user_conversion_events$first_joined_experiment,
                                         units='days')))
    
    # check converted_within_window
    check_converted_within_window <- user_conversion_events %>%
        mutate(is_valid = ifelse(days_from_experiment_to_conversion >= 0 & 
                                     days_from_experiment_to_conversion <= attribution_window,
                                 converted_within_window == TRUE,
                                 converted_within_window == FALSE))
    
    expect_true(all(check_converted_within_window$is_valid))
    
    # check that everyone that has converted is in the dataset in the correct experiment

    # create dataset that contains all users for each metric they converted & each experiment they were
    # apart of. This dataset should exactly match the dataset returned from experiments__determine_conversions
    # in terms of user/experiment/metric
    expected_user_experiment_metric_combos <- inner_join(
        experiment_data$experiment_traffic %>% 
        select(user_id, experiment_id, first_joined_experiment),
        experiment_data$conversion_events %>% select(user_id, metric_id),
               by='user_id') %>%
        inner_join(experiment_data$attribution_windows, by=c('experiment_id', 'metric_id')) %>%
        filter(first_joined_experiment < Sys.Date() - attribution_window) %>%
        select(user_id, experiment_id, metric_id) %>%
        arrange(user_id, experiment_id, metric_id)
    
    found_user_experiment_metric_combos <- user_conversion_events %>% 
        mutate(metric_id = as.character(metric_id)) %>% # need to convert to character because arrange will sort by level, not name
        select(user_id, experiment_id, metric_id) %>% 
        arrange(user_id, experiment_id, metric_id)

    expect_dataframes_equal(expected_user_experiment_metric_combos, found_user_experiment_metric_combos)
})

test_that("private__filter_experiment_traffic_via_attribution", {
    context("helpers_processing::private__filter_experiment_traffic_via_attribution")

    experiment_data <- load_data()

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <- max(experiment_data$experiment_traffic$first_joined_experiment)
    days_offset <- Sys.Date() - as.Date(max_date)

    experiment_data$experiment_traffic$first_joined_experiment <- experiment_data$experiment_traffic$first_joined_experiment + days_offset

    filtered_traffic <- private__filter_experiment_traffic_via_attribution(experiment_data)

    # If we add the attribution window to the first-joined-experiment, the max we should get is Today
    # meaning we are filtering out `today - attribution-window`
    max_joined_per_metric <- filtered_traffic %>%
        mutate(joined_plus_attribution = first_joined_experiment + days(attribution_window)) %>%
        group_by(metric_id) %>%
        summarise(max_joined_plus_attribution = max(joined_plus_attribution))

    # This might fail due to time-zones
    expect_true(all(Sys.Date() > max_joined_per_metric$max_joined_plus_attribution))

    # for all experiments that ended before max attribution date (relative to today),
    # the end date should equal the end date on the filtered dataset
    start_end_dates <- experiment_data$experiment_traffic %>%
        group_by(experiment_id) %>%
        summarise(start_date = min(first_joined_experiment),
                  end_date = max(first_joined_experiment)) %>%
        filter(end_date < Sys.Date() - max(experiment_data$attribution_windows$attribution_window))

    start_end_dates_filtered <- filtered_traffic %>%
        group_by(experiment_id) %>%
        summarise(start_date = min(first_joined_experiment),
                  end_date = max(first_joined_experiment)) %>%
        filter(experiment_id %in% start_end_dates$experiment_id)

    expect_dataframes_equal(start_end_dates, start_end_dates_filtered)
})

test_that("experiments__get_summary", {
    context("helpers_processing::experiments__get_summary")

    experiment_data <- load_data()

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <- max(max(experiment_data$experiment_traffic$first_joined_experiment),
                    #max(conversion_events$conversion_date),
                    max(experiment_data$website_traffic$visit_date))

    days_offset <- Sys.Date() - as.Date(max_date)

    experiment_data$experiment_traffic$first_joined_experiment <- experiment_data$experiment_traffic$first_joined_experiment + days_offset
    experiment_data$website_traffic$visit_date <- experiment_data$website_traffic$visit_date + days_offset
    experiment_data$conversion_events$conversion_date <- experiment_data$conversion_events$conversion_date + days_offset

    experiments_summary <- experiments__get_summary(experiment_data, days_of_prior_data=15)

    expect_false(any(is.na(experiments_summary)))
    
    # ensure, for each metric within the same experiment, all start/end dates are the same
    ensure_same_dates_per_experiment <- experiments_summary %>%
        group_by(experiment_id) %>%
        summarise(all_same_start_dates = length(unique(start_date)) == 1,
                  all_same_end_dates = length(unique(end_date)) == 1)

    expect_true(all(ensure_same_dates_per_experiment$all_same_start_dates))
    expect_true(all(ensure_same_dates_per_experiment$all_same_end_dates))
    expect_true(setequal(experiment_data$experiment_info$experiment_id,
                         ensure_same_dates_per_experiment$experiment_id))
    
    # for the experiments that are finished, check that the number of trials equals the number of people
    # expected in the experiment
    check_trial_numbers <-  experiments_summary %>%
        group_by(experiment_id) %>%  # need to group by expueriment to make sure that all the metrics within the experiment are finished collecting data
        mutate(experiment_finished = all(last_join_date == end_date)) %>%
        ungroup() %>%
        filter(experiment_finished) %>%  # experiment finished
        group_by(experiment_id) %>%
        summarise(distinct_control_trials = length(unique(control_trials)) == 1,
                  distinct_variant_trials = length(unique(variant_trials)) == 1,
                  actual_trials = min(control_trials) + min(variant_trials)) %>%
        inner_join(experiment_data$experiment_traffic %>%
                       group_by(experiment_id) %>%
                       summarise(expected_trials = n()),
                   by='experiment_id') %>%
        mutate(expected_actual_match = expected_trials == actual_trials)
    expect_equal(nrow(check_trial_numbers), 2)
    expect_true(all(check_trial_numbers$distinct_control_trials))
    expect_true(all(check_trial_numbers$distinct_variant_trials))
    expect_true(all(check_trial_numbers$expected_actual_match))

    # for the first experiment, these should equal the attribution window plus 1 day padding the end-date is Today
    expect_identical(as.numeric(round(difftime(experiments_summary$end_date,
                                               experiments_summary$last_join_date,
                                               units = c('days')))),
                     c(3, 4, 6, 8, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0))
    
    expect_dataframes_equal(distinct(experiments_summary %>% select(experiment_id, metric_id)) %>%
                                arrange(experiment_id),
                            experiment_data$attribution_windows %>% select(-attribution_window))
    
    expect_identical(experiments_summary$control_conversion_rate,
                     experiments_summary$control_successes / experiments_summary$control_trials)
    expect_identical(experiments_summary$variant_conversion_rate,
                     experiments_summary$variant_successes / experiments_summary$variant_trials)
    expect_identical(experiments_summary$percent_change_from_control,
                     (experiments_summary$variant_conversion_rate - experiments_summary$control_conversion_rate) / experiments_summary$control_conversion_rate)

    with(experiments_summary, expect_identical(control_alpha, prior_alpha + control_successes))
    with(experiments_summary, expect_identical(control_beta, prior_beta + control_trials - control_successes))

    with(experiments_summary, expect_identical(variant_alpha, prior_alpha + variant_successes))
    with(experiments_summary, expect_identical(variant_beta, prior_beta + variant_trials - variant_successes))
    with(experiments_summary, expect_identical(bayesian_cr_difference, bayesian_variant_cr - bayesian_control_cr))
    with(experiments_summary, expect_true(all(bayesian_conf_low < bayesian_cr_difference & bayesian_cr_difference < bayesian_conf_high)))

    with(experiments_summary, expect_identical(frequentist_cr_difference, variant_conversion_rate - control_conversion_rate))

    p_values <- with(experiments_summary, pmap_dbl(list(control_successes, control_trials, variant_successes, variant_trials),
        function(bs, bt, vs, vt) {
            prop.test(x=c(bs, vs), n=c(bt, vt))$p.value
        }
    ))
    expect_identical(p_values, experiments_summary$p_value)

    write.csv(experiments_summary, 'data/helpers/experiments_summary.csv')

    ##########################################################################################################
    # plots
    ##########################################################################################################
    plot_object <- plot__bayesian_posterior(experiments_summary,
                                             experiment=unique(experiments_summary$experiment_id)[1],
                                             metric='Sign Up',
                                             confidence_level=0.95)

    plot_object %>% test_save_plot(file='data/plot_helpers/plot__bayesian_posterior/experiment_1_signup.png')
    
    plot_object <- plot__bayesian_posterior(experiments_summary,
                                             experiment=unique(experiments_summary$experiment_id)[1],
                                             metric='Sign Up',
                                             confidence_level=0.95,
                                             show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__bayesian_posterior/experiment_1_signup_no_prior.png')

    plot_object <- plot__bayesian_posterior(experiments_summary,
                                             experiment="New Signup CTA Color",
                                             metric='Pay/Subscribe',
                                             confidence_level=0.95,
                                             show_prior_distribution = TRUE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__bayesian_posterior/signup_color_pay.png')
    
    plot_object <- plot__bayesian_posterior(experiments_summary,
                                             experiment="New Signup CTA Color",
                                             metric='Pay/Subscribe',
                                             confidence_level=0.95,
                                             show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__bayesian_posterior/signup_color_pay_no_prior.png')
    
    plot_object <- plot__bayesian_posterior(experiments_summary,
                                             experiment="Redesign Website",
                                             metric='Pay/Subscribe',
                                             confidence_level=0.95,
                                             show_prior_distribution = TRUE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__bayesian_posterior/redesign_website_pay.png')
    
    plot_object <- plot__bayesian_posterior(experiments_summary,
                                             experiment="Redesign Website",
                                             metric='Pay/Subscribe',
                                             confidence_level=0.95,
                                             show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__bayesian_posterior/redesign_website_pay_no_prior.png')
})

test_that("experiments__get_base_summary_priors", {
    context("helpers_processing::experiments__get_base_summary_priors")

    experiment_data <- load_data()

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <- max(max(experiment_data$experiment_traffic$first_joined_experiment),
                    #max(conversion_events$conversion_date),
                    max(experiment_data$website_traffic$visit_date))

    days_offset <- Sys.Date() - as.Date(max_date)

    experiment_data$experiment_traffic$first_joined_experiment <- experiment_data$experiment_traffic$first_joined_experiment + days_offset
    experiment_data$website_traffic$visit_date <- experiment_data$website_traffic$visit_date + days_offset
    experiment_data$conversion_events$conversion_date <- experiment_data$conversion_events$conversion_date + days_offset

    # get base summary, required for private__create_prior_experiment_traffic
    experiments_base_summary <- experiments__get_base_summary(experiment_data)
    
    # ensure, for each metric within the same experiment, all start/end dates are the same
    ensure_same_dates_per_experiment <- experiments_base_summary %>%
        group_by(experiment_id) %>%
        summarise(all_same_start_dates = length(unique(start_date)) == 1,
                  all_same_end_dates = length(unique(end_date)) == 1,
                  # dates should be the same, so just get min
                  start_date=min(start_date),
                  end_date=min(end_date))
    
    expect_true(all(ensure_same_dates_per_experiment$all_same_start_dates))
    expect_true(all(ensure_same_dates_per_experiment$all_same_end_dates))
    expect_true(setequal(experiment_data$experiment_info$experiment_id,
                         ensure_same_dates_per_experiment$experiment_id))

    days_of_prior_data=15
    prior_data <- private__create_prior_experiment_traffic(experiment_data,
                                                           experiments_summary=experiments_base_summary,
                                                           days_of_prior_data=days_of_prior_data)
    
    # make sure all experiments are accounted for
    expect_true(setequal(prior_data$experiment_id, ensure_same_dates_per_experiment$experiment_id))
    
    # ensure min/max first_joined_experiment dates are within the experiment summary dates
    ensure_joined_dates <- experiments_base_summary %>% 
        select(experiment_id, start_date) %>%
        distinct() %>%  # works because each start date should be the same, verified above
        inner_join(experiment_data$attribution_windows %>%
                       group_by(experiment_id) %>%
                       summarise(max_attribution_window=max(attribution_window)),
                   by=c('experiment_id')) %>%
        mutate(prior_start_date = start_date - days(days_of_prior_data + max_attribution_window + 1),
               prior_end_date = prior_start_date + days(days_of_prior_data)) %>%
        select(experiment_id, contains('prior')) %>%
        inner_join(prior_data %>%
                       group_by(experiment_id) %>%
                       summarise(min_first_joined=min(first_joined_experiment),
                                 max_first_joined=max(first_joined_experiment)),
                   by='experiment_id') %>%
        mutate(valid_min = min_first_joined >= prior_start_date,
               valid_max = max_first_joined <= prior_end_date) %>%
        select(valid_min, valid_max)

    expect_true(all(ensure_joined_dates$valid_min))
    expect_true(all(ensure_joined_dates$valid_max))

    # make sure variation is set correctly (it actually doens't need to be the original, but it has to be
    # something, and it will cause some headaches if it isn't)
    x <- distinct(prior_data %>% select(experiment_id, variation)) %>% arrange(experiment_id)
    y <- experiment_data$experiment_info %>% filter(is_control) %>% arrange(experiment_id)
    expect_identical(x$variation, y$variation)
    
    # get the experiments summary, but based on the prior data (i.e. mocked to look like an experiment)
    prior_summary <- experiments__get_base_summary(experiment_data)
    
    expect_identical(prior_summary$control_successes / prior_summary$control_trials,
                     prior_summary$control_conversion_rate)
    
    prior_vs_control_cr <- prior_summary %>% 
        select(experiment_id, metric_id, control_conversion_rate) %>%
        rename(prior_conversion_rate = control_conversion_rate) %>%
        inner_join(experiments_base_summary %>% select(experiment_id, metric_id, control_conversion_rate),
                   by=c('experiment_id', 'metric_id')) %>%
        mutate(percent_diff = (prior_conversion_rate - control_conversion_rate) / control_conversion_rate)
    
    # ensure that the PERCENT CHANGE from prior vs control (i.e. not the absolute difference, which will be
    # much smaller) averages at max +- 3%. This is subjective, and based on random variation of the simualted
    # data it might be above. But our case, the baseline conversion rates don't change over time in our 
    # simulated data so this is a good gut-check.
    expect_true(abs(mean(prior_vs_control_cr$percent_diff)) < 0.03)
})

test_that("experiments__get_daily_summary", {
    context("helpers_processing::experiments__get_daily_summary")
    
    experiment_data <- load_data()

    # get base summary, required for private__create_prior_experiment_traffic
    experiments_summary <- experiments__get_summary(experiment_data)
    experiments_daily_summary <- experiments__get_daily_summary(experiment_data, experiments_summary)
    
    # the last/final days of the daily cumulative summary should equal the results from the 
    # experiments__get_summary
    last_days_daily_summary <- experiments_daily_summary %>%
        group_by(experiment_id, metric_id) %>%
        mutate(r = rank(-as.numeric(day_expired_attribution))) %>%
        ungroup() %>%
        filter(r == 1) %>%
        #select(-day_expired_attribution) %>%
        rename(control_successes = control_cumulative_successes,
               control_trials = control_cumulative_trials,
               variant_successes = variant_cumulative_successes,
               variant_trials = variant_cumulative_trials) %>%
        arrange(experiment_id, metric_id)
    
    expected_summary <- experiments_summary %>% 
        select(-contains('date'),
               -percent_change_from_control,
               -control_name,
               -variant_name) %>%
        arrange(experiment_id, metric_id)
                             
    found_summary <- last_days_daily_summary %>%
        select_(.dots=colnames(expected_summary))
    
    expect_dataframes_equal(expected_summary, found_summary)

    # check that there are NA values that correspond to the lag in the attribution windows (+1 day since 
    # we need to wait x FULL days; e.g. experiment started on 1st with 2-day window, but it started at noon,
    # so we need 2 full days and we don't want to start reporting with data for half a day on the 3rd, so
    # we start reporting on the 4th)
    
    # first check that the starting day matches the starting day of experiments_summary
    missing_data <- experiments_daily_summary %>% 
        filter(is.na(control_conversion_rate))
    
    missing_data_summary <- missing_data %>%
        group_by(experiment_id, metric_id) %>%
        summarise(min_date = min(day_expired_attribution),
                  num_missing_days = length(unique(day_expired_attribution))) %>%
        ungroup() %>%
        arrange(experiment_id, metric_id)
    
    summary_start_dates <- experiments_summary %>%
        mutate(min_date = as.Date(start_date)) %>%
        select(experiment_id, metric_id, min_date) %>%
        arrange(experiment_id, metric_id)
    
    expect_dataframes_equal(summary_start_dates, missing_data_summary %>% select(-num_missing_days))
    # then check that the number of days of NA values matches the attribution window for that metric + 1 day
    
    expected_missing_days <- experiment_data$attribution_windows %>%
        mutate(num_missing_days = attribution_window + 1) %>%
        select(-attribution_window)
    expect_dataframes_equal(expected_missing_days,
                            missing_data_summary %>% select(-min_date))

    # then check that the end date matches the end date of experiments_summary + attribution window + 1 day
    expected_end_dates <- experiments_summary %>% 
        mutate(end_date = as.Date(end_date)) %>%
        select(experiment_id, metric_id, end_date) %>%
        inner_join(expected_missing_days %>%
                       mutate(metric_id = factor(metric_id, levels=levels(experiments_summary$metric_id))),
                   by = c("experiment_id", "metric_id")) %>%
        mutate(end_date = end_date + days(num_missing_days)) %>%
        select(-num_missing_days) %>%
        arrange(experiment_id, metric_id)
    
    found_end_dates <- last_days_daily_summary %>%
        select(experiment_id, metric_id, day_expired_attribution) %>%
        rename(end_date = day_expired_attribution)

    expect_dataframes_equal(expected_end_dates, found_end_dates)
    
    ##### Plot each daily graph
    for(experiment in unique(experiments_summary$experiment_id)) {
        for(metric in unique(experiments_summary$metric_id)) {
            # experiment <- "Ask Additional Questions During Signup"
            # metric <- "Use Feature 1"
            # print(experiment)
            # print(metric)
            
            plot_object <- plot__daily_p_value(experiments_daily_summary, experiment, metric)
            expect_false(is.null(plot_object))
            plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__daily_p_value/',
                                                       experiment, '_',
                                                       str_remove_all(metric, '/'),
                                                       '.png'),
                                           size_inches=c(8,12))

            plot_object <- plot__daily_percent_change_frequentist(experiments_daily_summary, experiment, metric)
            expect_false(is.null(plot_object))
            plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__daily_percent_change_frequentist/',
                                                       experiment, '_',
                                                       str_remove_all(metric, '/'),
                                                       '.png'),
                                           size_inches=c(8,12))

            plot_object <- plot__daily_prob_variant_gt_control(experiments_daily_summary, experiment, metric)
            expect_false(is.null(plot_object))
            plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__daily_prob_variant_gt_control/',
                                                       experiment, '_',
                                                       str_remove_all(metric, '/'),
                                                       '.png'),
                                           size_inches=c(8,12))

            plot_object <- plot__daily_percent_change_bayesian(experiments_daily_summary, experiment, metric)
            expect_false(is.null(plot_object))
            plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__daily_percent_change_bayesian/',
                                                       experiment, '_',
                                                       str_remove_all(metric, '/'),
                                                       '.png'),
                                           size_inches=c(8,12))
        }
    }
})

test_that("plot__conversion_rates", {
    context("helpers_processing::plot__conversion_rates")
    
    experiment_data <- load_data()
    experiment_names <- sort(unique(experiment_data$experiment_info$experiment_id))
    experiments_summary <- experiments__get_summary(experiment_data, days_of_prior_data=15)

    for(experiment in experiment_names) {
        
        plot_object <- plot__conversion_rates(experiments_summary, experiment=experiment)
        expect_false(is.null(plot_object))
        plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__conversion_rates/',
                                                    experiment,'.png'),
                                       size_inches=c(8,12))
    }
})

test_that("plot__conversion_rates_bayesian", {
    context("helpers_processing::plot__conversion_rates_bayesian")
    
    experiment_data <- load_data()
    experiment_names <- sort(unique(experiment_data$experiment_info$experiment_id))
    experiments_summary <- experiments__get_summary(experiment_data, days_of_prior_data=15)
    
    for(experiment in experiment_names) {
        # experiment <- experiment_names[1]
        plot_object <- plot__conversion_rates_bayesian(experiments_summary, experiment=experiment)
        expect_false(is.null(plot_object))
        plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__conversion_rates_bayesian/',
                                                   experiment,'.png'),
                                       size_inches=c(8,12))
    }
})

test_that("plot__percent_change_frequentist", {
    context("helpers_processing::plot__percent_change_frequentist")
    
    experiment_data <- load_data()
    experiment_names <- sort(unique(experiment_data$experiment_info$experiment_id))
    experiments_summary <- experiments__get_summary(experiment_data, days_of_prior_data=15)
    
    for(experiment in experiment_names) {
        
        plot_object <- plot__percent_change_frequentist(experiments_summary, experiment=experiment)
        expect_false(is.null(plot_object))
        plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__percent_change_frequentist/',
                                                   experiment,'.png'),
                                       size_inches=c(8,12))
    }
})

test_that("plot__percent_change_bayesian", {
    context("helpers_processing::plot__percent_change_bayesian")
    
    experiment_data <- load_data()
    experiment_names <- sort(unique(experiment_data$experiment_info$experiment_id))
    experiments_summary <- experiments__get_summary(experiment_data, days_of_prior_data=15)
    
    for(experiment in experiment_names) {
        
        plot_object <- plot__percent_change_bayesian(experiments_summary, experiment=experiment)
        expect_false(is.null(plot_object))
        plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__percent_change_bayesian/',
                                                   experiment,'.png'),
                                       size_inches=c(8,12))
    }
})

test_that("plot__percent_change_conf_frequentist", {
    context("helpers_processing::plot__percent_change_conf_frequentist")
    
    experiment_data <- load_data()
    experiment_names <- sort(unique(experiment_data$experiment_info$experiment_id))
    experiments_summary <- experiments__get_summary(experiment_data, days_of_prior_data=15)
    
    for(experiment in experiment_names) {
        
        plot_object <- plot__percent_change_conf_frequentist(experiments_summary, experiment=experiment)
        expect_false(is.null(plot_object))
        plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__percent_change_conf_frequentist/',
                                                   experiment,'.png'),
                                       size_inches=c(8,12))
    }
})

test_that("plot__percent_change_conf_bayesian", {
    context("helpers_processing::plot__percent_change_conf_bayesian")
    
    experiment_data <- load_data()
    experiment_names <- sort(unique(experiment_data$experiment_info$experiment_id))
    experiments_summary <- experiments__get_summary(experiment_data, days_of_prior_data=15)
    
    for(experiment in experiment_names) {
        
        plot_object <- plot__percent_change_conf_bayesian(experiments_summary, experiment=experiment)
        expect_false(is.null(plot_object))
        plot_object %>% test_save_plot(file=paste0('data/plot_helpers/plot__percent_change_conf_bayesian/',
                                                   experiment,'.png'),
                                       size_inches=c(8,12))
    }
})

test_that("historical_conversion_rates",  {
    context("helpers_processing::historical_conversion_rates")

    experiment_data <- load_data()

    historical_crs <- get_historical_conversion_rates(experiment_data)

    # check that the metric/attr-windows are expected    
    expected_attr_windows <- experiment_data$attribution_windows %>%
        group_by(metric_id) %>%
        summarise(median_attr_window = median(attribution_window)) %>%
        arrange(median_attr_window)

    expect_dataframes_equal(historical_crs %>% select(metric_id, median_attr_window),
                            expected_attr_windows %>% 
                                mutate(metric_id = factor(metric_id, levels=expected_attr_windows$metric_id)))

    expect_identical(historical_crs$median_attr_window,
                     floor(historical_crs$median_days_from_first_visit_to_conversion))
    expect_identical(historical_crs$median_attr_window,
                     floor(historical_crs$mean_days_from_first_visit_to_conversion))

    expect_equal(historical_crs$conversion_rate_within_window / historical_crs$historical_conversion_rate,
                 historical_crs$percent_cr_window_realized)
    
    # these are the baseline_conversion_rates found in the simulation data
    expect_identical(round(historical_crs$historical_conversion_rate, 2), c(0.10, 0.07, 0.05, 0.03))

    plot_object <- plot__conversion_rates_historical(historical_crs) 
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__conversion_rates/plot__conversion_rates_historical.png')
    
    plot_object <- plot__conversion_rates_attribution(historical_crs)
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__conversion_rates/plot__conversion_rates_attribution.png')
    
    ########################
    # test get__cohorted_traffic_conversions, get__cohorted_conversions_snapshot, & plots
    ########################
    cohort_format <- '%W'
    #cohort_format <- '%m'
    metric_name <- 'Sign Up'
    
    traffic_conversions <- get__cohorted_traffic_conversions(experiment_data,
                                                                       metric=metric_name,
                                                                       cohort_format=cohort_format)
    snapshots <- c(3, 5, 10)
    cohort_label='Week'

    cohorted_snapshots <- get__cohorted_conversions_snapshot(traffic_conversions,
                                                             cohort_label=cohort_label,
                                                             snapshots=snapshots,
                                                             snapshot_max_days=30)

    plot_object <- plot__conversion_rates_snapshot_absolute(cohorted_snapshots=cohorted_snapshots,
                                                            cohort_label='Week')
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__conversion_rates/plot__conversion_rates_snapshot_absolute.png')

    plot_object <- plot__conversion_rates_snapshot_percent(cohorted_snapshots=cohorted_snapshots,
                                                           snapshot_max_days = 30,
                                                           cohort_label='Week') 
    plot_object %>% test_save_plot(file='data/plot_helpers/plot__conversion_rates/plot__conversion_rates_snapshot_percent.png')
})

test_that("ab_test_calculator", {
    context("helpers_processing::ab_test_calculator")

    experiment_data <- load_data()
    
    duration_calculator__url <- 'example.com'
    duration_calculator__metrics <- c("Sign Up", "Use Feature 1", "Talk to Sales", "Pay/Subscribe")
    duration_calculator__mde <- 0.05
    duration_calculator__alpha <- 0.05
    duration_calculator__beta <- 0.20

    power <- 1 - duration_calculator__beta

    historical_conversion_rates <- get_historical_conversion_rates(experiment_data)

    calc_results <- site__ab_test_calculator(experiment_data,
                                             historical_conversion_rates,
                                             experiment_path=duration_calculator__url,
                                             metrics=duration_calculator__metrics,
                                             minimum_detectable_effect=duration_calculator__mde,
                                             alpha=duration_calculator__alpha,
                                             power=power,
                                             simulated_experiment_length = 30)
    expect_false(any(is.na(calc_results$results)))
        
    expect_identical(as.character(calc_results$results$Metric), duration_calculator__metrics)
    expect_equal(round(calc_results$daily_traffic / 1000) * 1000, 8000)

    expected_sample_size <- calculate_total_sample_size(original_conversion_rate=historical_conversion_rates$historical_conversion_rate[1],
                                                        percent_increase=duration_calculator__mde,
                                                        power=power,
                                                        alpha=duration_calculator__alpha)
    expect_equal(calc_results$results$`Estimated Users Required`[1], expected_sample_size)
})
