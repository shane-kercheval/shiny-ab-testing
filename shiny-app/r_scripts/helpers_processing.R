source('helpers_stats.R')
source('helpers_misc.R')

#' loads the datasets, returns them as a named list (the goal is to encapsulate the undelying datasets and
#' pass the various functions a consistent object)
#' 
#' must ensure that the `data_path__` fields below are defined in the environment. e.g. the paths
#' could differ between production/development/unit-test environments.
load_data <- function() {

    get_list <- function() {

        experiment_info <- as.data.frame(read_csv(data_path__experiment_info))
        experiment_traffic <- as.data.frame(read_csv(data_path__experiment_traffic))
        attribution_windows <- as.data.frame(read_csv(data_path__attribution_windows))
        website_traffic <- as.data.frame(read_csv(data_path__website_traffic))
        conversion_events <- as.data.frame(read_csv(data_path__conversion_events))

        return (list(
            experiment_info=experiment_info,
            experiment_traffic=experiment_traffic,
            attribution_windows=attribution_windows,
            website_traffic=website_traffic,
            conversion_events=conversion_events
        ))
    }

    dataset_list <- suppressMessages(get_list())

    check_data__experiment_info(dataset_list$experiment_info)
    check_data__experiment_traffic(dataset_list$experiment_traffic, dataset_list$experiment_info)
    check_data__attribution_windows(dataset_list$attribution_windows, dataset_list$experiment_info)
    check_data__website_traffic(dataset_list$website_traffic)
    check_data__conversion_events(dataset_list$conversion_events, dataset_list$attribution_windows)

    return (dataset_list)
}

#' Gets the row of the first visit for each user-id
#'
#' @param experiment_data list of data-frames from load_data
website_traffic__get_user_first_visit <- function(experiment_data) {
    
    # we're going to get the first occurance; i initially did this by grouping by user-id and dplyr:rank based
    # on visit_date, but this was sloooowwwwww. So I'm going to user by user & date, then remove duplicates
    # which is fast; i've verified they produce identical results
    website_traffic <- experiment_data$website_traffic %>% arrange(user_id, visit_date)
    
    return (website_traffic[!duplicated(website_traffic %>% select(user_id)), ])
}

#' Counts UNIQUE users per day (and optionally per path).
#' If only_first_time_visits is TRUE, the user is only counted the first date they appear in the dataset. In
#'      this case, calls with `top_n_paths=NULL` and `top_n_paths=x` will sum to the same values.
#' 
#' If only_first_time_visits is TRUE, then per day and per path counts will not sum to the same total because
#'      a single user can visit multiple pages in the same day, so they will be represented in >=1 rows for a
#'      single day when top_n_paths is not NULL, but will only be count once in a day when top_n_paths is NULL
#' 
#' NOTE: count of users from `website_traffic__to_daily_num_users` will not sum to
#'      `website_traffic__to_cohort_num_users` (if for example you create the same cohort and then group-by
#'      and sum) because `website_traffic__to_cohort_num_users` will only count the user-id once in the
#'      cohorted period where the user-id may have had visits across multiple days
#' 
#' @param experiment_data list of data-frames from load_data
#' @param top_n_paths if specified, count by the top (i.e. highest traffic) paths, grouping the remaining
#'      paths into an 'Other' category. 
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to
#'      website)
website_traffic__to_daily_num_users <- function(experiment_data,
                                                top_n_paths=NULL,
                                                only_first_time_visits=FALSE) {
    
    if(only_first_time_visits) {

        website_traffic <- website_traffic__get_user_first_visit(experiment_data)

    } else {

        website_traffic <- experiment_data$website_traffic
    }
    
    if(!is.null(top_n_paths)) {

        path_levels <- c(head(website_traffic %>% count(path, sort=TRUE), top_n_paths)$path, 'Other')
        website_traffic <- website_traffic %>% mutate(path = fct_lump(path, n=top_n_paths))
        levels(website_traffic$path) <- path_levels
    
        return (website_traffic %>% count(visit_date, path) %>% rename(num_users=n))

    } else {
        
        return ( website_traffic %>% group_by(visit_date) %>% summarise(num_users=n_distinct(user_id)))
    }
}

#' Counts UNIQUE users per cohorted period (and optionally per path).
#' If only_first_time_visits is TRUE, the user is only counted the first time they appear in the dataset. In
#'      this case, calls with `top_n_paths=NULL` and `top_n_paths=x` will sum to the same values.
#' 
#' If only_first_time_visits is TRUE, then per cohorted period and per path counts will not sum to the same
#'      total because a single user can visit multiple pages in the same cohorted period, so they will be
#'      represented in >=1 rows for a single cohorted period when top_n_paths is not NULL, but will only be
#'      count once in a cohorted period when top_n_paths is NULL
#' 
#' @param experiment_data list of data-frames from load_data
#' @param top_n_paths if specified, count by the top (i.e. highest traffic) paths, grouping the remaining
#'      paths into an 'Other' category.
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to
#'      website)
website_traffic__to_cohort_num_users <- function(experiment_data,
                                                 cohort_format='%W',
                                                 top_n_paths=NULL,
                                                 only_first_time_visits=FALSE) {

    if(only_first_time_visits) {

        website_traffic <- website_traffic__get_user_first_visit(experiment_data)

    } else {

        website_traffic <- experiment_data$website_traffic
    }
    
    website_traffic <- website_traffic %>%
        mutate(cohort = create_cohort(visit_date, cohort_format = cohort_format))

    if(!is.null(top_n_paths)) {

        path_levels <- c(head(website_traffic %>% count(path, sort=TRUE), top_n_paths)$path, 'Other')
        website_traffic <- website_traffic %>% mutate(path = fct_lump(path, n=top_n_paths))
        levels(website_traffic$path) <- path_levels
    
        
        return (website_traffic %>% count(cohort, path) %>% rename(num_users=n))

    } else {
        
        return ( website_traffic %>% group_by(cohort) %>% summarise(num_users=n_distinct(user_id)))
    }
}

##############################################################################################################
# Experiment Summary Functions
##############################################################################################################

#' Returns a dataset that is unique by experiment/metric/user that has contains the conversion rates when
#'      considering the attribution windows for each metric. If the user converted (for a given metric) before
#'      they joined the experiment, or after the attribution window for that metric, then the conversion is
#'      not attributed to the experiment and will not be counted (for that experiment)
#' 
#' @param experiment_data list of data-frames from load_data
experiments__determine_conversions <- function(experiment_data) {

    # use this rather than Sys.Date() in case data is not refreshed daily or we are using simulated data
    current_date <- max(experiment_data$website_traffic$visit_date)

    # dataset only contains users that converted
    # dataset is per user, per experment, per converted metric
    conversion_events <- experiment_data$experiment_traffic %>% 
        # duplicates user-records when user has converted with multiple metrics
        inner_join(experiment_data$conversion_events, by='user_id') %>%
        # now we need to get the attribution time windows to figure out if they converted within the
        # time-frame
        inner_join(experiment_data$attribution_windows, by=c('experiment_id', 'metric_id')) %>%
        # but, we have to filter out anyone who first joined the experiment in the last x days, where x is
        # less than the attribution window for that metric, because they haven't been given the full amount
        # of time to convert. And even though (by definition of being in this dataset) they have already
        # converted, we don't want to count them yet because it will misrepresent the future conversion rate
        # (i.e. there are still people who are in this group that will convert)
        # so, when we calculate the "total" users (i.e. denominator) we will also filter out users that joined
        # the experiment in the last x days, to keep the numerator/denominator in the conversion rates in the 
        # same time period.
        filter(first_joined_experiment < current_date - days(attribution_window)) %>%
        mutate(days_from_experiment_to_conversion = as.numeric(difftime(conversion_date,  # negative days means the conversion happened before the experiment started
                                                                        first_joined_experiment,
                                                                        units = 'days')),
               converted_within_window = days_from_experiment_to_conversion >= 0 & 
                   days_from_experiment_to_conversion <= attribution_window,
               metric_id = fct_reorder(metric_id, attribution_window)) %>%
        select(user_id, experiment_id, variation, metric_id, first_joined_experiment, conversion_date,
               days_from_experiment_to_conversion, attribution_window, converted_within_window)

        return (conversion_events)
}

#' Gets the "base" summary ror each experiment/metric.
#' 
#' Includes the start/end/last-event dates, the number of successes and trials (and conversion rate) of the
#' control and variant groups, etc.
#' 
#' The difference between `end_date` and `last_join_date` comes from the fact that we exclude users who have
#' joined the experiment in the last x days where x is the attribution window for that given
#' experiment/metric. Otherwise (if we included those users) it would distort (likely over-state) the future
#' conversion rate. 
#' 
#' @param experiment_data list of data-frames from load_data
experiments__get_base_summary <- function(experiment_data) {

    experiment_start_end_dates <- experiment_data$experiment_traffic %>%
        group_by(experiment_id) %>%
        summarise(start_date = min(first_joined_experiment),
                  end_date = max(first_joined_experiment))

    # filter the traffic by attribution windows;
    # this will duplicate each row in experiment_traffic for each metric; since attribution is based on metric
    filtered_experiment_traffic <- private__filter_experiment_traffic_via_attribution(experiment_data)
    
    # i want to cache the is_control/variation-name here so in know i'm joining the actual variation used for
    # the control; i could join on the variation names after the fact but there's an increase risk because
    # e.g. i could have messed up the control/variation combos but joining after the fact wouldn't illuminate
    # this
    cache_experiment_variations <- distinct(filtered_experiment_traffic %>% 
                                                select(experiment_id, variation, is_control)) %>% 
        spread(is_control, variation)

    # now we need to rename `TRUE` and `FALSE` columns to control/variant
    # but, if this is "prior" experiment data, theren't won't be a `FALSE`, so we need to check
    if("FALSE" %in% colnames(cache_experiment_variations)) {

        cache_experiment_variations <- cache_experiment_variations %>%
            rename(control_name=`TRUE`,
                   variant_name=`FALSE`)

    } else {

        cache_experiment_variations <- cache_experiment_variations %>%
            rename(control_name=`TRUE`)
    }

    stopifnot(nrow(cache_experiment_variations) == length(unique(experiment_data$experiment_info$experiment_id)))
    
    experiments_summary <- filtered_experiment_traffic %>%
        group_by(experiment_id, metric_id, is_control) %>%
        summarise(last_join_date = max(first_joined_experiment),
                  trials = n()) %>%
        ungroup() %>%
        # on the offchance the start dates (or end dates) are different between the control/metric/variation 
        # (e.g. started the experiment at ~midnight and 1 person went into the control on day x and the next
        # went
        # into the variation on day x+1)
        group_by(experiment_id, metric_id) %>%
        mutate(last_join_date = max(last_join_date)) %>%
        ungroup() %>%
        # now format so there is 1 row per experiment
        spread(is_control, trials)
    
    # now we need to rename `TRUE` and `FALSE` columns to control/variant
    # but, if this is "prior" experiment data, theren't won't be a `FALSE`, so we need to check
    if("FALSE" %in% colnames(experiments_summary)) {

        experiments_summary <- experiments_summary %>%
            rename(control_trials=`TRUE`,
                   variant_trials=`FALSE`) %>%
            select(experiment_id, last_join_date, metric_id, control_trials, variant_trials)

    } else {

        experiments_summary <- experiments_summary %>%
            rename(control_trials=`TRUE`) %>%
            select(experiment_id, last_join_date, metric_id, control_trials)
    }

    experiments_summary <- inner_join(experiments_summary,
                                      experiment_start_end_dates,
                                      by='experiment_id') %>%
        select(experiment_id, start_date, end_date, everything()) %>%
        # most recent ended (which is really just the last event, so it may not be stopped), so if
        # there are multiple experiments that are still running, sort by the most recent started
        arrange(desc(end_date), desc(start_date), metric_id)

    stopif(any(duplicated(experiments_summary %>% select(experiment_id, metric_id))))

    ##########################################################################################################
    # Add successes and conversion rates
    ##########################################################################################################
    
    # experiments__determine_conversions will exclude traffic based on first_joined_experiment &
    # attribution windows like we did above with private__filter_experiment_traffic_via_attribution
    experiment_conversion_events <- experiments__determine_conversions(experiment_data) %>%
        filter(converted_within_window) %>%
        count(experiment_id, variation, metric_id) %>%
        rename(successes=n) %>%
        inner_join(experiment_data$experiment_info, by=c('experiment_id', 'variation')) %>%
        select(-variation) %>%
        spread(is_control, successes)
    
    # now we need to rename `TRUE` and `FALSE` columns to control/variant
    # but, if this is "prior" experiment data, theren't won't be a `FALSE`, so we need to check
    if("FALSE" %in% colnames(experiment_conversion_events)) {
        
        experiments_summary <- inner_join(experiments_summary,
                                          experiment_conversion_events,
                                          by=c('experiment_id', 'metric_id')) %>%
            rename(control_successes=`TRUE`,
                   variant_successes=`FALSE`) %>% 
            mutate(control_conversion_rate=control_successes / control_trials,
                   variant_conversion_rate=variant_successes / variant_trials,
                   percent_change_from_control = (variant_conversion_rate - control_conversion_rate) / 
                       control_conversion_rate)

    } else {
        
        experiments_summary <- inner_join(experiments_summary,
                                          experiment_conversion_events,
                                          by=c('experiment_id', 'metric_id')) %>%
            rename(control_successes=`TRUE`) %>%
            mutate(control_conversion_rate=control_successes / control_trials)
    }
    
    experiments_summary <- inner_join(experiments_summary, cache_experiment_variations, by = 'experiment_id')

    return (experiments_summary)
}

#' Gets the summary ror each experiment/metric.
#' 
#' Includes the start/end/last-event dates, the number of successes and trials (and conversion rate) of the
#' control and variant groups, frequentist stats (e.g. p-value), bayesian stats, etc.
#' 
#' The difference between `end_date` and `last_join_date` comes from the fact that we exclude users who have
#' joined the experiment in the last x days where x is the attribution window for that given
#' experiment/metric. Otherwise (if we included those users) it would distort (likely over-state) the future
#' conversion rate. 
#' 
#' @param experiment_data list of data-frames from load_data
experiments__get_summary <- function(experiment_data,
                                     days_of_prior_data=15,
                                     confidence_level=0.95) {

    experiments_summary <- experiments__get_base_summary(experiment_data)
    
    ##########################################################################################################
    # Add P-Value Information
    ##########################################################################################################
    
    p_values <- pmap(list(experiments_summary$control_successes,
                          experiments_summary$control_trials,
                          experiments_summary$variant_successes,
                          experiments_summary$variant_trials),
                     function(bs, bt, vs, vt) get_p_values_info(bs, bt, vs, vt,
                                                                confidence_level=confidence_level))
 
    experiments_summary$p_value <- map_dbl(p_values, ~ .['p_value'])
    experiments_summary$frequentist_cr_difference <- map_dbl(p_values, ~ .['cr_diff_estimate'])
    experiments_summary$frequentist_conf_low <- map_dbl(p_values, ~ .['conf.low'])
    experiments_summary$frequentist_conf_high <- map_dbl(p_values, ~ .['conf.high'])

    ##########################################################################################################
    # Add Bayesian Information
    ##########################################################################################################
    prior_data <- private__create_prior_experiment_traffic(experiment_data,
                                                           experiments_summary,
                                                           days_of_prior_data)
    # get the experiments summary, but based on the prior data (i.e. mocked to look like an experiment)
    mock_prior_data <- list(
        experiment_info=experiment_data$experiment_info,
        experiment_traffic=prior_data,  # use "mocked" prior traffic/data rather than actual traffic
        attribution_windows=experiment_data$attribution_windows,
        website_traffic=experiment_data$website_traffic,
        conversion_events=experiment_data$conversion_events
    )
    prior_summary <- experiments__get_base_summary(experiment_data=mock_prior_data)

    prior_summary <- prior_summary %>%
        mutate(prior_alpha=control_successes,
               prior_beta=control_trials - control_successes) %>%
        select(experiment_id, metric_id, prior_alpha, prior_beta)

    experiments_summary <- inner_join(experiments_summary,
                                     prior_summary,
                                     by = c("experiment_id", "metric_id"))

    experiments_summary <- experiments_summary %>%
        mutate(control_alpha = prior_alpha + control_successes,
               control_beta = prior_beta + (control_trials - control_successes),
               variant_alpha = prior_alpha + variant_successes,
               variant_beta = prior_beta + (variant_trials - variant_successes))

    cia_list <- pmap(with(experiments_summary,
                          list(control_alpha, control_beta, variant_alpha, variant_beta)),
                     function(alpha_a, beta_a, alpha_b, beta_b){  
                         credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b,
                                                  confidence_level=confidence_level)
                     })

    experiments_summary <- experiments_summary %>%                 
        mutate(bayesian_prob_variant_gt_control=map_dbl(cia_list, ~.['posterior']),
               bayesian_cr_difference=map_dbl(cia_list, ~.['cr_diff_estimate']),
               bayesian_conf_low=map_dbl(cia_list, ~.['conf.low']),
               bayesian_conf_high=map_dbl(cia_list, ~.['conf.high']))

    experiments_summary <- experiments_summary %>%                 
        mutate(bayesian_control_cr = control_alpha / (control_alpha + control_beta),
               bayesian_variant_cr = variant_alpha / (variant_alpha + variant_beta))
    

               
    experiments_summary <- experiments_summary %>%
        select(
               # experiment information
               experiment_id,
               start_date,
               end_date,
               last_join_date,
               metric_id,

               # conversion rates
               control_conversion_rate,
               variant_conversion_rate,
               percent_change_from_control,

               # control & variation raw numbers
               control_name,
               control_successes,
               control_trials,

               variant_name,
               variant_successes,
               variant_trials,

               # frequentist
               p_value,
               frequentist_cr_difference,
               frequentist_conf_low,
               frequentist_conf_high,

               # bayesian
               prior_alpha,
               prior_beta,

               control_alpha,
               control_beta,
               variant_alpha,
               variant_beta,

               bayesian_control_cr,
               bayesian_variant_cr,

               bayesian_prob_variant_gt_control,
               bayesian_cr_difference,
               bayesian_conf_low,
               bayesian_conf_high)

    return (experiments_summary)
}

#' Filters the experiment_traffic according to the attribution window per experiment/metric.
#' As a result, this function adds a record for each metric
#' 
#' Originally used within the context of, for example, getting the total number of trials and/or the total
#' number of successes. But, we want to exclude users who have joined the experiment in the last x days where
#' x is the attribution window for that given experiment/metric. Otherwise (if we included those users) it 
#' would distort (likely over-state) the future conversion rate. 
#' 
#' @param experiment_data list of data-frames from load_data
private__filter_experiment_traffic_via_attribution <- function(experiment_data){

    # use this rather than Sys.Date() in case data is not refreshed daily or we are using simulated data
    current_date <- max(experiment_data$website_traffic$visit_date)

    inner_join(experiment_data$experiment_traffic,
               experiment_data$attribution_windows,
               by='experiment_id') %>%
        # we only want the people who have had enough time to convert, given the attribution window for a
        # given metric (i.e. exclude people who join within the attribution window relative to today)
        filter(first_joined_experiment < current_date - days(attribution_window)) %>%
        mutate(metric_id = fct_reorder(metric_id, attribution_window)) %>%
        inner_join(experiment_data$experiment_info,
                   by=c('experiment_id', 'variation'))
}

#' @param experiment_data list of data-frames from load_data
private__create_prior_experiment_traffic <- function(experiment_data,
                                                     experiments_summary,
                                                     days_of_prior_data=15) {

    # we need to modify website_traffic to mock experiment_traffic in order to genearate a PRIOR dataset for
    # bayesian calculations.
    # specifically, based on how many days of prior data we want, we'll transform/mock the website_traffic
    # to look like experiment_traffic, but based on the prior dates.
    # NOTE: Well only want to use the paths that the experiments where in
    experiment_prior_dates <- experiments_summary %>%
        select(experiment_id, start_date, metric_id) %>%
        inner_join(experiment_data$attribution_windows %>%
                       mutate(metric_id = factor(metric_id,
                                                 levels=levels(experiments_summary$metric_id))),
                   by = c('experiment_id', 'metric_id')) %>%
        group_by(experiment_id) %>%
        # well take the min calculated start date so we allow enough time for the largest attribution window
        summarise(prior_start_date = min(start_date - days(days_of_prior_data + attribution_window + 1)),
                  prior_end_date = prior_start_date + days(days_of_prior_data))

    experiment_prior_paths <- distinct(experiment_data$experiment_traffic %>% select(experiment_id, path))
    prior_data <- data.frame(user_id=NULL, first_joined_experiment=NULL, experiment_id=NULL, variation=NULL)

    for(experiment in unique(experiment_data$experiment_info$experiment_id)) {

        prior_start_date <- (experiment_prior_dates %>% filter(experiment_id == experiment))$prior_start_date
        prior_end_date <- (experiment_prior_dates %>% filter(experiment_id == experiment))$prior_end_date
        
        prior_paths <- experiment_prior_paths %>% filter(experiment_id == experiment)
        variation_name <- experiment_data$experiment_info %>%
            filter(experiment_id == experiment,
                   is_control) %>%
            get_vector('variation')
        
        prior_data <- rbind(prior_data,
                            experiment_data$website_traffic %>% 
                                filter(visit_date >= prior_start_date & visit_date <= prior_end_date,
                                       path %in% prior_paths$path) %>%
                                group_by(user_id) %>%
                                summarise(first_joined_experiment = min(visit_date)) %>%
                                mutate(experiment_id = experiment,
                                       variation = variation_name))
    }

    return (prior_data)
}

#' @param experiment_data list of data-frames from load_data
#' @param experiments_summary result of `experiments_get_summary()`
#' @param confidence_level the confidence level to use for the frequentist and byaesian methods
experiments__get_daily_summary <- function(experiment_data, experiments_summary, confidence_level=0.95) {

    # per metric, since it is based on attribution windows
    # for each day of the experiment, get the total number of trials (i.e. denominator); must allow enough
    # days corresponding to the attribution window
    t <- private__filter_experiment_traffic_via_attribution(experiment_data)
    
    t <- t %>%
        # gets the day where the attribution window has expired (i.e. day we can count the particular person
        # as a success/trial)
        mutate(day_expired_attribution = ceiling_date(first_joined_experiment + days(attribution_window),
                                                      unit='day')) 
  
    # for each day of the experiment, get the total number of successes (i.e. numerator); must allow enough
    # days corresponding to the attribution window
    conversion_events <- experiments__determine_conversions(experiment_data) %>%
        filter(converted_within_window) %>%
        select(user_id, experiment_id, variation, metric_id, converted_within_window)


    t <- left_join(t, conversion_events, by = c("user_id", "experiment_id", "variation", "metric_id"))

    cumulative_traffic <- t %>%
        mutate(converted_within_window = ifelse(is.na(converted_within_window),
                                                FALSE,
                                                converted_within_window)) %>%
        group_by(experiment_id, variation, metric_id, day_expired_attribution) %>%
        summarise(is_control = any(is_control),
                  trials = n_distinct(user_id),
                  trials_verify = n(),
                  successes = sum(converted_within_window)) %>%
        ungroup() %>%
        arrange(experiment_id, is_control, metric_id, day_expired_attribution) %>%
        group_by(experiment_id, variation, metric_id) %>%
        mutate(cumulative_trials=cumsum(trials),
               cumulative_successes=cumsum(successes)) %>%
        ungroup()
    
    stopifnot(identical(cumulative_traffic$trials, cumulative_traffic$trials_verify))
    
    cumulative_traffic <- cumulative_traffic %>%
        select(-trials, -trials_verify, -successes)
    
    # convert is_control column to wide version (e.g controL_cumulative_success, variant_cumulative_success)
    cumulative_traffic <- cumulative_traffic %>%
        select(-variation) %>%
        mutate(conversion_rate = cumulative_successes / cumulative_trials) %>%
        gather(variable, value, -c(experiment_id, metric_id, day_expired_attribution, is_control)) %>%
        mutate(variation = ifelse(is_control, 'control', 'variant')) %>%
        select(-is_control) %>%
        unite(temp, variation, variable) %>%
        spread(temp, value)
    #colnames(cumulative_traffic)

    # add frequentist data to cumulative traffic
    p_values <- pmap(list(cumulative_traffic$control_cumulative_successes,
                          cumulative_traffic$control_cumulative_trials,
                          cumulative_traffic$variant_cumulative_successes,
                          cumulative_traffic$variant_cumulative_trials),
                     function(bs, bt, vs, vt) get_p_values_info(bs, bt, vs, vt,
                                                                confidence_level=confidence_level))

    cumulative_traffic$p_value <- map_dbl(p_values, ~ .['p_value'])
    cumulative_traffic$frequentist_cr_difference <- map_dbl(p_values, ~ .['cr_diff_estimate'])
    cumulative_traffic$frequentist_conf_low <- map_dbl(p_values, ~ .['conf.low'])
    cumulative_traffic$frequentist_conf_high <- map_dbl(p_values, ~ .['conf.high'])
    
    # add bayesian data to cumulative traffic based on the prior alpha/beta from the experiments_summary data
    # per experiment/metric
    cumulative_traffic <- inner_join(cumulative_traffic,
                                     experiments_summary %>% 
                                         select(experiment_id, metric_id, prior_alpha, prior_beta),
                                     by = c("experiment_id", "metric_id"))
    cumulative_traffic <- cumulative_traffic %>%
        mutate(control_alpha = prior_alpha + control_cumulative_successes,
               control_beta = prior_beta + (control_cumulative_trials - control_cumulative_successes),
               variant_alpha = prior_alpha + variant_cumulative_successes,
               variant_beta = prior_beta + (variant_cumulative_trials - variant_cumulative_successes))
    
    cia_list <- pmap(with(cumulative_traffic,
                          list(control_alpha, control_beta, variant_alpha, variant_beta)),
                     function(alpha_a, beta_a, alpha_b, beta_b){  
                         credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b,
                                                  confidence_level=confidence_level)
                     })
    
    cumulative_traffic <- cumulative_traffic %>%                 
        mutate(bayesian_prob_variant_gt_control=map_dbl(cia_list, ~.['posterior']),
               bayesian_cr_difference=map_dbl(cia_list, ~.['cr_diff_estimate']),
               bayesian_conf_low=map_dbl(cia_list, ~.['conf.low']),
               bayesian_conf_high=map_dbl(cia_list, ~.['conf.high']))
    
    cumulative_traffic <- cumulative_traffic %>%
        mutate(bayesian_control_cr = control_alpha / (control_alpha + control_beta),
               bayesian_variant_cr = variant_alpha / (variant_alpha + variant_beta),
               bayesian_percent_change = (bayesian_variant_cr - bayesian_control_cr) /  bayesian_control_cr)
    
    ##########################################################################################################
    # we need at add on the missing dates at the beginning of the expriment to account for the attribution
    # windows, e.g. so a graph can still plot those dates and it is obvious that there is a delay
    ##########################################################################################################
    experiment_dates <- experiment_data$experiment_traffic %>% 
        mutate(day_expired_attribution = as.Date(first_joined_experiment)) %>%
        select(experiment_id, day_expired_attribution) %>%
        distinct() %>%
        inner_join(experiment_data$attribution_windows %>% select(-attribution_window),
                   by='experiment_id') %>%
        mutate(metric_id = factor(metric_id, levels = levels(cumulative_traffic$metric_id))) %>%
        select(experiment_id, metric_id, day_expired_attribution) %>%
        arrange(experiment_id, day_expired_attribution, metric_id)
    
    # This gives all the days (days of experiment + lag in attribution days)
    cumulative_traffic <- full_join(cumulative_traffic %>%
                                        mutate(day_expired_attribution = as.Date(day_expired_attribution)),
                                    experiment_dates,
                                    by = c("experiment_id", "metric_id", "day_expired_attribution")) %>%
        arrange(experiment_id, day_expired_attribution, metric_id)
    
    return (cumulative_traffic)
}

#' gives historical conversion rates per metric, along with conversion rates based on attribution windows
#' 
#' @param experiment_data list of data-frames from load_data
#' @param include_last_n_days number of prior days of data to include
#' @param exclude_last_n_days number of prior days of data to exclude
get_historical_conversion_rates <- function(experiment_data,
                                            include_last_n_days=180,
                                            exclude_last_n_days=30
                                            ) {

    attr_windows <- experiment_data$attribution_windows %>%
        group_by(metric_id) %>%
        summarise(median_attr_window = median(attribution_window)) %>%
        ungroup()

    # last date of traffic
    # use this because if data is updated daily it should give the same answer as Sys.Date()
    # but if not, or if working on simulated data, it will use the last date in the data
    last_date_of_traffic <- max(experiment_data$website_traffic$visit_date)

    traffic_conversions <- left_join(experiment_data$website_traffic %>%
                                        group_by(user_id) %>%
                                        summarise(first_visit = min(visit_date)) %>%
                                        ungroup(),
                                     experiment_data$conversion_events,
                                     by='user_id') %>%
        filter(first_visit >= last_date_of_traffic - days(include_last_n_days),
               first_visit < last_date_of_traffic - days(exclude_last_n_days)) %>%
        left_join(attr_windows, by='metric_id') %>%
        # negative days means the conversion happened before the experiment started; which shouldn't be possible in this dataset
        mutate(days_from_first_visit_to_conversion = as.numeric(difftime(conversion_date,
                                                                         first_visit,
                                                                         units = 'days')),
               converted_within_window = days_from_first_visit_to_conversion >= 0 &
                   days_from_first_visit_to_conversion <= median_attr_window,
               converted_within_window = ifelse(is.na(converted_within_window), FALSE, converted_within_window))

    # shouldnt' be possible to have a conversion before the first visit in this dataset; unlike experiment traffic
    stopif(any(traffic_conversions$days_from_first_visit_to_conversion <= 0, na.rm = TRUE))

    total_users <- length(unique(traffic_conversions$user_id))

    t <- traffic_conversions %>%
        filter(!is.na(metric_id)) %>%
        group_by(metric_id) %>%
        summarise(median_attr_window = min(median_attr_window),
                  mean_days_from_first_visit_to_conversion=mean(days_from_first_visit_to_conversion),
                  median_days_from_first_visit_to_conversion=median(days_from_first_visit_to_conversion),
                  historical_conversion_rate=n_distinct(user_id) / total_users,
                  conversion_rate_within_window=sum(converted_within_window) / total_users,
                  percent_cr_window_realized=conversion_rate_within_window / historical_conversion_rate) %>%
        arrange(desc(historical_conversion_rate))
    return (t %>% mutate(metric_id = factor(metric_id, levels=t$metric_id)))
}


#' Returns Traffic Data *Left Joined* with Conversion Event Data, for a specific metric; so it returns
#' all traffic data since it is left-joined
#' @param experiment_data list of data-frames from load_data
#' @param metric the metric to filter on
#' @param cohort_format string format of cohort e.g. "%W" or "%m"
get__cohorted_traffic_conversions <- function(experiment_data,
                                              metric,
                                              cohort_format) {
    left_join(experiment_data$website_traffic %>%
                  group_by(user_id) %>%
                  summarise(first_visit = min(visit_date)) %>%
                  ungroup() %>%
                  mutate(cohort = create_cohort(first_visit, cohort_format)),
              experiment_data$conversion_events %>%
                  filter(metric_id == metric),
              by='user_id')
}

#' This function returns conversion rates after n days (based on 3 different snapshots) relative to each
#'      user's first visit. It also returns the "percent of all conversions" for each snapshot, using 
#'      the conversion rate at snapshot_max_days as the "maximum" conversion rate i.e. the denominator 
#' @param traffic_conversions object returned by `get__cohorted_traffic_conversions()`
#' @param cohort_label the label of the cohort e.g. "Week" or "Month"
#' @param snapshots a vector of the number of days allowed from the first visit to the conversion to count
#'      towards the snapshot; each number in the vector represents a different snapshot
#' @param snapshot_max_days the **maximum** number of days allowed from the first visit to conversion;
#'      this is used as the denominator 
get__cohorted_conversions_snapshot <- function(traffic_conversions,
                                               cohort_label,
                                               snapshots=c(1, 3, 5),
                                               snapshot_max_days=30) {

    stopifnot(all(snapshots <= snapshot_max_days))
    stopifnot(all(snapshots >= 1))
    
    label_lookup <- paste(snapshots, "Days")
    names(label_lookup) <- paste("Snapshot", 1:length(snapshots))
    
    value_lookup <- snapshots
    names(value_lookup) <- as.character(label_lookup)
    
    # use this rather than Sys.Date() in case data is not refreshed daily or we are using simulated data
    current_date <- max(traffic_conversions$first_visit)
    
    if(cohort_label == "Week") {
        
        cohort_format <- "%W"
        
    } else if (cohort_label == "Month") {
        
        cohort_format <- "%m"
        
    }else {
        
        stopifnot(FALSE)
    }
    
    traffic_conversions <- traffic_conversions %>%
        filter(cohort != create_cohort(current_date, cohort_format = cohort_format)) %>%
        mutate(days_to_convert=as.numeric(difftime(conversion_date, first_visit, units = 'days')))
    
    for(index in 1:length(snapshots)) {
        #index <- 1
        
        column_name <- names(label_lookup[index])
        traffic_conversions <- traffic_conversions %>%
            mutate(!!column_name := !is.na(conversion_date) &
                                    days_to_convert > 0 &
                                    days_to_convert <= snapshots[index])
    }
    
    traffic_conversions <- traffic_conversions %>%
        mutate(`Max Snapshot`:= !is.na(conversion_date) &
                   days_to_convert > 0 &
                   days_to_convert <= snapshot_max_days)
    
    stopifnot(length(unique(traffic_conversions %>% filter(!is.na(metric_id)) %>% get_vector('metric_id', return_unique = TRUE))) == 1)
    stopif(traffic_conversions$user_id %>% duplicated() %>% any())
    
    conversions_absolute <- traffic_conversions %>%
        select(cohort, first_visit, contains("Snapshot ")) %>%
        gather(snapshot, converted, -c(cohort, first_visit)) %>%
        group_by(cohort, snapshot) %>%
        summarise(max_first_visit=max(first_visit),
                  num_users=n(),  # verified above there are not any duplicated user-ids
                  num_converted=sum(converted),
                  conversion_rate=num_converted / num_users) %>%
        ungroup()
    
    # make sure the number of users for each snapshot within a cohort is the same
    stopifnot(all(conversions_absolute %>%
                      group_by(cohort) %>%
                      summarise(unique_num_users = length(unique(num_users))) %>%
                      get_vector('unique_num_users') == 1))
    
    
    conversions_absolute <- conversions_absolute %>%
        mutate(snapshot_label = label_lookup[snapshot],
               snapshot_value = value_lookup[snapshot_label],
               conversion_rate = ifelse(max_first_visit > current_date - days(snapshot_value), NA, conversion_rate))

    # this time, we only want to look at those who have converted within the max time allowed
    # and, we only want to keep the cohorts where the last person to join the cohort has had enough to convert >= snapshot_max_days
    conversions_percent_of_all <- traffic_conversions %>%
        filter(`Max Snapshot`) %>%
        select(cohort, first_visit, contains("Snapshot ")) %>%
        gather(snapshot, converted, -c(cohort, first_visit)) %>%
        group_by(cohort, snapshot) %>%
        summarise(max_first_visit=max(first_visit),
                  num_users=n(),  # verified above there are not any duplicated user-ids
                  num_converted=sum(converted),
                  conversion_rate_percent_of_all=num_converted / num_users) %>%
        ungroup()
    
    # make sure the number of users for each snapshot within a cohort is the same
    stopifnot(all(conversions_percent_of_all %>%
                      group_by(cohort) %>%
                      summarise(unique_num_users = length(unique(num_users))) %>%
                      get_vector('unique_num_users') == 1))
    
    
    conversions_percent_of_all <- conversions_percent_of_all %>%
        mutate(snapshot_label = label_lookup[snapshot],
               snapshot_value = value_lookup[snapshot_label],
               conversion_rate_percent_of_all = ifelse(max_first_visit > current_date - days(snapshot_max_days), NA, conversion_rate_percent_of_all))
    
    expect_dataframes_equal(conversions_absolute %>% select(-max_first_visit, -num_users, -conversion_rate), 
                            conversions_percent_of_all %>% select(-max_first_visit, -num_users, -conversion_rate_percent_of_all))
    
    conversions_final <- inner_join(conversions_absolute %>%
                                        select(cohort,
                                               snapshot,
                                               snapshot_label,
                                               snapshot_value,
                                               num_converted,
                                               num_users,
                                               conversion_rate),
                                    conversions_percent_of_all %>%
                                        select(cohort, snapshot, conversion_rate_percent_of_all),
                                    by=c('cohort', 'snapshot')) %>%
        arrange(cohort, snapshot)
    
    stopifnot(nrow(conversions_final) == nrow(conversions_absolute))
    
    conversions_final <- conversions_final %>%
        mutate(cohort=factor(cohort, levels=unique(conversions_final$cohort)),
               snapshot=factor(snapshot, levels=unique(conversions_final$snapshot)),
               snapshot_label=factor(snapshot_label, levels=unique(conversions_final$snapshot_label)))
        
    return(conversions_final)
}

#' returns required sample size and estimated number of days based on recent traffic and historical
#'      conversion rates
#' @param experiment_data list of data-frames from load_data
#' @param historical_conversion_rates object returned by `get_historical_conversion_rates()`
#' @param experiment_path the path to filter on and simulate traffic numbers from
#' @param metrics the metrics to use for the sample size calcualtion
#' @param minimum_detectable_effect the percent increase you would like to detect
#' @param alpha the porbability of a false positive
#' @param power 1 minus the probability of a false negative
#' @param simulated_experiment_length the length of "simulated experiment" which is how the daily traffic is
#'      calculated
#' @param metric the metric to filter on
site__ab_test_calculator <- function(experiment_data,
                                     historical_conversion_rates,
                                     experiment_path,
                                     metrics,
                                     minimum_detectable_effect=0.05,
                                     alpha=0.05,
                                     power=0.8,
                                     simulated_experiment_length = 30) {

    # get expected traffic; returning users are included in experiment
    # simulate starting a 30-day experiment; must start the experiment 30 days + max attribution window
    # prior to the last date of the website traffic available
    attribution_windows <- experiment_data$attribution_windows %>%
        filter(metric_id %in% metrics) %>% 
        group_by(metric_id) %>%
        summarise(max_attribution_window = max(attribution_window))

    # relative to last date in website traffic (use rather than Sys.Date if data is not refreshed data and/or simulated data)
    experiment_start_date <- max(experiment_data$website_traffic$visit_date) - 
        days(simulated_experiment_length) - days(1) - 
        days(max(attribution_windows$max_attribution_window))

    simulated_experiment_traffic <- experiment_data$website_traffic %>%
        filter(visit_date >= experiment_start_date &
                   visit_date <= experiment_start_date + days(simulated_experiment_length),
               path == experiment_path) %>%
        group_by(user_id) %>%
        summarise(first_joined_experiment = min(visit_date))
    
    stopif(simulated_experiment_traffic$user_id %>% duplicated() %>% any())
    
    daily_experiment_traffic_count <- length(simulated_experiment_traffic$user_id) / simulated_experiment_length

    historical_conversion_rates <- historical_conversion_rates %>%
        filter(metric_id %in% metrics) %>%
        select(metric_id, historical_conversion_rate)

    conversion_rates <- historical_conversion_rates$historical_conversion_rate
    names(conversion_rates) <- historical_conversion_rates$metric_id

    calc_results <- ab_test_calculator(daily_traffic=daily_experiment_traffic_count,
                                        conversion_rates=conversion_rates,  # vector of conversion_rates
                                        percent_increase=minimum_detectable_effect,
                                        power=power,
                                        alpha=alpha)
    
    # make sure all the datasets have the metrics in the same order
    stopifnot(setequal(metrics, historical_conversion_rates$metric_id))
    stopifnot(setequal(metrics, names(calc_results$days_required)))
    stopifnot(setequal(metrics, names(calc_results$entities_required)))
    
    detectable_crs <- historical_conversion_rates$historical_conversion_rate +
        (historical_conversion_rates$historical_conversion_rate * minimum_detectable_effect)
    results_df <- data.frame(`Metric`=metrics,
               `Estimated Days Required`=as.numeric(calc_results$days_required),
               `Estimated Users Required`=as.numeric(calc_results$entities_required),
               `Historical Conversion Rate`=historical_conversion_rates$historical_conversion_rate,
               `Detectable Conversion Rate`=detectable_crs,
               check.names = FALSE)
    
    return(list(results=results_df, daily_traffic=daily_experiment_traffic_count))
}
