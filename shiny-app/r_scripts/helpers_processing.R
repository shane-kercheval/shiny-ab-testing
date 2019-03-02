source('helpers_stats.R')
source('helpers_misc.R')

#' loads the datasets, returns them as a named list
#'
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
    website_traffic <- website_traffic %>% arrange(user_id, visit_date)
    
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

        website_traffic <- website_traffic__get_user_first_visit(website_traffic)
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
#' @param website_traffic dataframe containing website traffic data in the expected format
#' @param top_n_paths if specified, count by the top (i.e. highest traffic) paths, grouping the remaining
#'      paths into an 'Other' category.
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to
#'      website)
website_traffic__to_cohort_num_users <- function(website_traffic,
                                                 cohort_format='%W',
                                                 top_n_paths=NULL,
                                                 only_first_time_visits=FALSE) {

    if(only_first_time_visits) {

        website_traffic <- website_traffic__get_user_first_visit(website_traffic)
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
#' @param experiment_traffic
#' @param attribution_windows
#' @param user_conversion_events
experiments__determine_conversions <- function(experiment_traffic, attribution_windows, user_conversion_events) {

    # dataset only contains users that converted
    # dataset is per user, per experment, per converted metric
    user_conversion_events <-  experiment_traffic %>% 
        # duplicates user-records when user has converted with multiple metrics
        inner_join(user_conversion_events, by='user_id') %>%
        # now we need to get the attribution time windows to figure out if they converted within the
        # time-frame
        inner_join(attribution_windows, by=c('experiment_id', 'metric_id')) %>%
        # but, we have to filter out anyone who first joined the experiment in the last x days, where x is
        # less than the attribution window for that metric, because they haven't been given the full amount
        # of time to convert. And even though (by definition of being in this dataset) they have already
        # converted, we don't want to count them yet because it will misrepresent the future conversion rate
        # (i.e. there are still people who are in this group that will convert)
        # so, when we calculate the "total" users (i.e. denominator) we will also filter out users that joined
        # the experiment in the last x days, to keep the numerator/denominator in the conversion rates in the 
        # same time period.
        filter(first_joined_experiment < Sys.Date() - attribution_window) %>%
        mutate(days_from_experiment_to_conversion = as.numeric(difftime(conversion_date,  # negative days means the conversion happened before the experiment started
                                                                        first_joined_experiment,
                                                                        units = 'days')),
               converted_within_window = days_from_experiment_to_conversion >= 0 & 
                   days_from_experiment_to_conversion <= attribution_window,
               metric_id = fct_reorder(metric_id, attribution_window)) %>%
        select(user_id, experiment_id, variation, metric_id, first_joined_experiment, conversion_date,
               days_from_experiment_to_conversion, attribution_window, converted_within_window)

        return (user_conversion_events)
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
#' @param experiment_info
#' @param experiment_traffic
#' @param attribution_windows
#' @param conversion_events
experiments__get_base_summary <- function(experiment_info,
                                          experiment_traffic,
                                          attribution_windows,
                                          conversion_events) {

    experiment_start_end_dates <- experiment_traffic %>%
        group_by(experiment_id) %>%
        summarise(start_date = min(first_joined_experiment),
                  end_date = max(first_joined_experiment))

    # filter the traffic by attribution windows;
    # this will duplicate each row in experiment_traffic for each metric; since attribution is based on metric
    filtered_experiment_traffic <- private__filter_experiment_traffic_via_attribution(experiment_info,
                                                                                      experiment_traffic,
                                                                                      attribution_windows)
    
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

    stopifnot(nrow(cache_experiment_variations) == length(unique(experiment_info$experiment_id)))
    
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
    # attribution windows like we did above with
    experiment_conversion_events <- experiments__determine_conversions(experiment_traffic,
                                                                       attribution_windows,
                                                                       conversion_events) %>%
        filter(converted_within_window) %>%
        count(experiment_id, variation, metric_id) %>%
        rename(successes=n) %>%
        inner_join(experiment_info, by=c('experiment_id', 'variation')) %>%
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
#' @param experiment_info
#' @param experiment_traffic
#' @param attribution_windows
#' @param conversion_events
experiments__get_summary <- function(experiment_info,
                                     experiment_traffic,
                                     website_traffic,
                                     attribution_windows,
                                     conversion_events,
                                     days_of_prior_data=15) {

    experiments_summary <- experiments__get_base_summary(experiment_info,
                                                         experiment_traffic,
                                                         attribution_windows,
                                                         conversion_events)
    
    ##########################################################################################################
    # Add P-Value Information
    ##########################################################################################################
    
    p_values <- pmap(list(experiments_summary$control_successes,
                          experiments_summary$control_trials,
                          experiments_summary$variant_successes,
                          experiments_summary$variant_trials),
                     function(bs, bt, vs, vt) get_p_values_info(bs, bt, vs, vt))
 
    experiments_summary_pvalues <- experiments_summary
    
    experiments_summary$p_value <- map_dbl(p_values, ~ .['p_value'])
    experiments_summary$cr_diff_estimate <- map_dbl(p_values, ~ .['cr_diff_estimate'])
    experiments_summary$p_value_conf_low <- map_dbl(p_values, ~ .['conf.low'])
    experiments_summary$p_value_conf_high <- map_dbl(p_values, ~ .['conf.high'])

    ##########################################################################################################
    # Add Bayesian Information
    ##########################################################################################################
    prior_data <- private__create_prior_experiment_traffic(website_traffic,
                                                           experiments_summary,
                                                           experiment_traffic,
                                                           experiment_info,
                                                           attribution_windows,
                                                           days_of_prior_data)
    # get the experiments summary, but based on the prior data (i.e. mocked to look like an experiment)
    prior_summary <- experiments__get_base_summary(experiment_info=experiment_info,
                                                   experiment_traffic=prior_data,
                                                   attribution_windows=attribution_windows,
                                                   conversion_events=conversion_events)

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
                         credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b)
                     })

    experiments_summary <- experiments_summary %>%                 
        mutate(prob_variant_is_better=map_dbl(cia_list, ~.['posterior']),
               bayesian_cr_diff_estimate=map_dbl(cia_list, ~.['cr_diff_estimate']),
               bayesian_conf.low=map_dbl(cia_list, ~.['conf.low']),
               bayesian_conf.high=map_dbl(cia_list, ~.['conf.high']))

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
               cr_diff_estimate,
               p_value_conf_low,
               p_value_conf_high,
               # bayesian
               prior_alpha,
               prior_beta,
               control_alpha,
               control_beta,
               variant_alpha,
               variant_beta,
               prob_variant_is_better,
               bayesian_cr_diff_estimate,
               bayesian_conf.low,
               bayesian_conf.high)

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
#' @param experiment_traffic
#' @param attribution_windows
private__filter_experiment_traffic_via_attribution <- function(experiment_info,
                                                               experiment_traffic,
                                                               attribution_windows){

    inner_join(experiment_traffic, attribution_windows, by='experiment_id') %>%
        # we only want the people who have had enough time to convert, given the attribution window for a
        # given metric (i.e. exclude people who join within the attribution window relative to today)
        filter(first_joined_experiment < Sys.Date() - attribution_window) %>%
        mutate(metric_id = fct_reorder(metric_id, attribution_window)) %>%
        inner_join(experiment_info,
                   by=c('experiment_id', 'variation'))
}

private__create_prior_experiment_traffic <- function(website_traffic,
                                                     experiments_summary,
                                                     experiment_traffic,
                                                     experiment_info,
                                                     attribution_windows,
                                                     days_of_prior_data=15) {

    # we need to modify website_traffic to mock experiment_traffic in order to genearate a PRIOR dataset for
    # bayesian calculations.
    # specifically, based on how many days of prior data we want, we'll transform/mock the website_traffic
    # to look like experiment_traffic, but based on the prior dates.
    # NOTE: Well only want to use the paths that the experiments where in
    experiment_prior_dates <- experiments_summary %>%
        select(experiment_id, start_date, metric_id) %>%
        inner_join(attribution_windows %>%
                       mutate(metric_id = factor(metric_id,
                                                 levels=levels(experiments_summary$metric_id))),
                   by = c('experiment_id', 'metric_id')) %>%
        group_by(experiment_id) %>%
        # well take the min calculated start date so we allow enough time for the largest attribution window
        summarise(prior_start_date = min(start_date - days(days_of_prior_data + attribution_window + 1)),
                  prior_end_date = prior_start_date + days(days_of_prior_data))

    experiment_prior_paths <- distinct(experiment_traffic %>% select(experiment_id, path))
    prior_data <- data.frame(user_id=NULL, first_joined_experiment=NULL, experiment_id=NULL, variation=NULL)

    for(experiment in unique(experiment_info$experiment_id)) {

        prior_start_date <- (experiment_prior_dates %>% filter(experiment_id == experiment))$prior_start_date
        prior_end_date <- (experiment_prior_dates %>% filter(experiment_id == experiment))$prior_end_date
        
        prior_paths <- experiment_prior_paths %>% filter(experiment_id == experiment)
        variation_name <- (experiment_info %>% 
            filter(experiment_id == experiment,
                   is_control))$variation
        
        prior_data <- rbind(prior_data,
                            website_traffic %>% 
                                filter(visit_date >= prior_start_date & visit_date <= prior_end_date,
                                       path %in% prior_paths$path) %>%
                                group_by(user_id) %>%
                                summarise(first_joined_experiment = min(visit_date)) %>%
                                mutate(experiment_id = experiment,
                                       variation = variation_name))
    }

    return (prior_data)
}
