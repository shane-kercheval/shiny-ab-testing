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

    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))

    users_first_visit <- website_traffic__get_user_first_visit(website_traffic)

    # this is another way of creating the data, although it is MUCH slower (almost too slow for a unit test)
    # could speed it up if we don't want to test path
    expected_first_visits <- website_traffic %>%
        group_by(user_id) %>%
        summarise(first_visit = min(visit_date),
                  path = path[visit_date == first_visit])

    expect_true(all(users_first_visit %>% arrange(user_id) == expected_first_visits %>% arrange(user_id)))
})

test_that("website_traffic__to_xxx_num_users", {
    context("helpers_processing::website_traffic__to_xxx_num_users")

    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))

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

test_that("website_traffic__plot_traffic", {
    context("helpers_processing::website_traffic__plot_traffic")
    
    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))

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

test_that("experiments__determine_conversions", {
    context("helpers_processing::experiments__determine_conversions")

    experiment_info <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_info.csv'))
    experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv'))
    attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv'))
    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))
    conversion_events <- as.data.frame(read_csv('../shiny-app/simulated_data/conversion_events.csv'))

    check_data__experiment_info(experiment_info)
    check_data__experiment_traffic(experiment_traffic, experiment_info)
    check_data__attribution_windows(attribution_windows, experiment_info)
    check_data__website_traffic(website_traffic)
    check_data__conversion_events(conversion_events, attribution_windows)

    # I want to manipulate the simulated dataset to
    # 1) make sure the dates are relative to today (experiments__determine_conversions filter out based on\
    #    Sys.Date())
    # 2) simulate people entering the experiment who already converted (so that we can ensure they are not
    #    counted as converted
    max_experiment_traffic_date <- max(experiment_traffic$first_joined_experiment)
    days_offset <- Sys.Date() - as.Date(max_experiment_traffic_date)

    experiment_traffic <- experiment_traffic %>%
        mutate(first_joined_experiment = first_joined_experiment + days_offset)

    conversion_events <- conversion_events %>%
        # the minus 1 will make it so some people will have already converted
        mutate(conversion_date = conversion_date + days_offset - 1)

    user_conversion_events <- experiments__determine_conversions(experiment_traffic,
                                                                 attribution_windows,
                                                                 conversion_events)

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
    expect_true(all((user_conversion_events %>% count(experiment_id, metric_id, user_id))$n == 1))
    
    # check attribution windows
    check_attribution_windows <- left_join(attribution_windows,
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
    expect_true(all(user_conversion_events$days_from_experiment_to_conversion == 
                        difftime(user_conversion_events$conversion_date, 
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
        experiment_traffic %>% 
        select(user_id, experiment_id, first_joined_experiment),
               conversion_events %>% select(user_id, metric_id),
               by='user_id') %>%
        inner_join(attribution_windows, by=c('experiment_id', 'metric_id')) %>%
        filter(first_joined_experiment < Sys.Date() - attribution_window) %>%
        select(user_id, experiment_id, metric_id) %>%
        arrange(user_id, experiment_id, metric_id)
    
    found_user_experiment_metric_combos <- user_conversion_events %>% 
        mutate(metric_id = as.character(metric_id)) %>% # need to convert to character because arrange will sort by level, not name
        select(user_id, experiment_id, metric_id) %>% 
        arrange(user_id, experiment_id, metric_id)
    expect_true(all(expected_user_experiment_metric_combos == found_user_experiment_metric_combos))
})

test_that("private__filter_experiment_traffic_via_attribution", {
    context("helpers_processing::private__filter_experiment_traffic_via_attribution")

    experiment_info <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_info.csv'))
    experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv'))
    attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv'))

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <- max(experiment_traffic$first_joined_experiment)
    days_offset <- Sys.Date() - as.Date(max_date)

    experiment_traffic$first_joined_experiment <- experiment_traffic$first_joined_experiment + days_offset

    filtered_traffic <- private__filter_experiment_traffic_via_attribution(experiment_info,
                                                                           experiment_traffic,
                                                                           attribution_windows)

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

test_that("experiments__get_summary", {
    context("helpers_processing::experiments__get_summary")

    experiment_info <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_info.csv'))
    experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv'))
    attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv'))
    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))
    conversion_events <- as.data.frame(read_csv('../shiny-app/simulated_data/conversion_events.csv'))

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <- max(max(experiment_traffic$first_joined_experiment),
                    #max(conversion_events$conversion_date),
                    max(website_traffic$visit_date))

    days_offset <- Sys.Date() - as.Date(max_date)

    experiment_traffic$first_joined_experiment <- experiment_traffic$first_joined_experiment + days_offset
    website_traffic$visit_date <- website_traffic$visit_date + days_offset
    conversion_events$conversion_date <- conversion_events$conversion_date + days_offset

    experiments_summary <- experiments__get_summary(experiment_info,
                                                    experiment_traffic,
                                                    website_traffic,
                                                    attribution_windows,
                                                    conversion_events,
                                                    days_of_prior_data=15)

    expect_false(any(is.na(experiments_summary)))
    
    # ensure, for each metric within the same experiment, all start/end dates are the same
    ensure_same_dates_per_experiment <- experiments_summary %>%
        group_by(experiment_id) %>%
        summarise(all_same_start_dates = length(unique(start_date)) == 1,
                  all_same_end_dates = length(unique(end_date)) == 1)

    expect_true(all(ensure_same_dates_per_experiment$all_same_start_dates))
    expect_true(all(ensure_same_dates_per_experiment$all_same_end_dates))
    expect_true(all(sort(unique(experiment_info$experiment_id)) == sort(ensure_same_dates_per_experiment$experiment_id)))

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
        inner_join(experiment_traffic %>%
                       group_by(experiment_id) %>%
                       summarise(expected_trials = n()),
                   by='experiment_id') %>%
        mutate(expected_actual_match = expected_trials == actual_trials)
    expect_equal(nrow(check_trial_numbers), 2)
    expect_true(all(check_trial_numbers$distinct_control_trials))
    expect_true(all(check_trial_numbers$distinct_variant_trials))
    expect_true(all(check_trial_numbers$expected_actual_match))

    # for the first experiment, these should equal the attribution window plus 1 day padding the end-date is Today
    expect_true(all(round(difftime(experiments_summary$end_date , experiments_summary$last_join_date, units = c('days'))) == c(3, 4, 6, 8, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)))
    expect_true(all(distinct(experiments_summary %>% select(experiment_id, metric_id)) %>% arrange(experiment_id) == attribution_windows %>% select(-attribution_window)))

    expect_true(all(experiments_summary$control_conversion_rate == experiments_summary$control_successes / experiments_summary$control_trials))
    expect_true(all(experiments_summary$variant_conversion_rate == experiments_summary$variant_successes / experiments_summary$variant_trials))
    expect_true(all(experiments_summary$percent_change_from_control == (experiments_summary$variant_conversion_rate - experiments_summary$control_conversion_rate) / experiments_summary$control_conversion_rate))

    expect_true(all(with(experiments_summary, control_alpha == prior_alpha + control_successes)))
    expect_true(all(with(experiments_summary, control_beta == prior_beta + control_trials - control_successes)))

    expect_true(all(with(experiments_summary, variant_alpha == prior_alpha + variant_successes)))
    expect_true(all(with(experiments_summary, variant_beta == prior_beta + variant_trials - variant_successes)))
    expect_true(all(with(experiments_summary, bayesian_conf.low < bayesian_cr_diff_estimate & bayesian_cr_diff_estimate < bayesian_conf.high)))

    expect_true(all(with(experiments_summary, cr_diff_estimate == variant_conversion_rate - control_conversion_rate)))

    p_values <- with(experiments_summary, pmap_dbl(list(control_successes, control_trials, variant_successes, variant_trials),
        function(bs, bt, vs, vt) {
            prop.test(x=c(bs, vs), n=c(bt, vt))$p.value
        }
    ))
    expect_true(all(p_values == experiments_summary$p_value))

    write.csv(experiments_summary, 'data/helpers/experiments_summary.csv')

    ##########################################################################################################
    # plots
    ##########################################################################################################
    plot_object <- experiments_summary__plot_bayesian(experiments_summary,
                                 experiment=unique(experiments_summary$experiment_id)[1],
                                 metric='Sign Up')

    plot_object %>% test_save_plot(file='data/plot_helpers/experiments_summary__plot_bayesian/experiment_1_signup.png')
    
    plot_object <- experiments_summary__plot_bayesian(experiments_summary,
                                 experiment=unique(experiments_summary$experiment_id)[1],
                                 metric='Sign Up',
                                 show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/experiments_summary__plot_bayesian/experiment_1_signup_no_prior.png')

    plot_object <- experiments_summary__plot_bayesian(experiments_summary,
                                 experiment="New Signup CTA Color",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = TRUE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/experiments_summary__plot_bayesian/signup_color_pay.png')
    
    plot_object <- experiments_summary__plot_bayesian(experiments_summary,
                                 experiment="New Signup CTA Color",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/experiments_summary__plot_bayesian/signup_color_pay_no_prior.png')
    
    plot_object <- experiments_summary__plot_bayesian(experiments_summary,
                                 experiment="Redesign Website",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = TRUE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/experiments_summary__plot_bayesian/redesign_website_pay.png')
    
    plot_object <- experiments_summary__plot_bayesian(experiments_summary,
                                 experiment="Redesign Website",
                                 metric='Pay/Subscribe',
                                 show_prior_distribution = FALSE)
    
    plot_object %>% test_save_plot(file='data/plot_helpers/experiments_summary__plot_bayesian/redesign_website_pay_no_prior.png')
})

test_that("experiments__get_summary", {
    context("helpers_processing::experiments__get_summary")

    experiment_info <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_info.csv'))
    experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv'))
    attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv'))
    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv'))
    conversion_events <- as.data.frame(read_csv('../shiny-app/simulated_data/conversion_events.csv'))

    ###############
    # shift all dates relative to today so we can test excluding people (who recently entered into the
    # experiment) based on the attribution window
    ###############
    max_date <- max(max(experiment_traffic$first_joined_experiment),
                    #max(conversion_events$conversion_date),
                    max(website_traffic$visit_date))

    days_offset <- Sys.Date() - as.Date(max_date)

    experiment_traffic$first_joined_experiment <- experiment_traffic$first_joined_experiment + days_offset
    website_traffic$visit_date <- website_traffic$visit_date + days_offset
    conversion_events$conversion_date <- conversion_events$conversion_date + days_offset

    # get base summary, required for private__create_prior_experiment_traffic
    experiments_base_summary <- experiments__get_base_summary(experiment_info,
                                                         experiment_traffic,
                                                         attribution_windows,
                                                         conversion_events)
    
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
    expect_true(all(sort(unique(experiment_info$experiment_id)) == sort(ensure_same_dates_per_experiment$experiment_id)))

    days_of_prior_data=15
    prior_data <- private__create_prior_experiment_traffic(website_traffic=website_traffic,
                                                           experiments_summary=experiments_base_summary,
                                                           experiment_traffic=experiment_traffic,
                                                           experiment_info=experiment_info,
                                                           attribution_windows=attribution_windows,
                                                           days_of_prior_data=days_of_prior_data)
    
    # make sure all experiments are accounted for
    expect_true(all(sort(unique(prior_data$experiment_id)) == sort(ensure_same_dates_per_experiment$experiment_id)))
    
    # ensure min/max first_joined_experiment dates are within the experiment summary dates
    ensure_joined_dates <- experiments_base_summary %>% 
        select(experiment_id, start_date) %>%
        distinct() %>%  # works because each start date should be the same, verified above
        inner_join(attribution_windows %>%
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
    y <- experiment_info %>% filter(is_control) %>% arrange(experiment_id)
    expect_true(all(x$variation == y$variation))

    # get the experiments summary, but based on the prior data (i.e. mocked to look like an experiment)
    prior_summary <- experiments__get_base_summary(experiment_info=experiment_info,
                                                   experiment_traffic=prior_data,
                                                   attribution_windows=attribution_windows,
                                                   conversion_events=conversion_events)
    
    expect_true(all(prior_summary$control_successes / prior_summary$control_trials == prior_summary$control_conversion_rate))
    
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
