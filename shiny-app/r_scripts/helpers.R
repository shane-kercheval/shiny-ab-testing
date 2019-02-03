##############################################################################################################
# CHECK DATASET ASSUMPTIONS
##############################################################################################################

#' Checks basic assumptions and data structure of website traffic dataset
#'
#' @param website_traffic dataframe containing website traffic data in the expected format
check_data__website_traffic <- function(website_traffic) {
    
    stopifnot(!any(is.na(website_traffic)))

    stopifnot(all(colnames(website_traffic) == c('user_id', 'visit_date', 'path')))
    # ensure there are no duplicated rows i.e. unique user/date/path
    stopifnot(nrow(website_traffic) == nrow(distinct(website_traffic)))
}

#' Checks basic assumptions and data structure of experiment traffic dataset
#'
#' @param experiment_traffic dataframe containing experiment traffic data in the expected format
check_data__experiment_traffic <- function(experiment_traffic, experiment_info) {
    
    stopifnot(!any(is.na(experiment_traffic)))

    stopifnot(all(sort(colnames(experiment_traffic)) == 
                      sort(c("user_id", "first_joined_experiment", "path", "experiment_id", "variation"))))
    
    # a user should only enter have one record for each experiment (which represents the date/path they
    # entered the experiment and the variation they were assigned)
    experiment_summary <- experiment_traffic %>%
        group_by(experiment_id) %>%
        summarise(duplicate_user_ids = any(duplicated(user_id)),
                  variation_count = length(unique(variation)))
    
    stopifnot(!any(experiment_summary$duplicate_user_ids))
    stopifnot(all(experiment_summary$variation_count == 2))

    # ensure that there is a 1-1 relationship between the experiment_id/variation in experiment_info and 
    # website_traffic
    unique_experiment_variation_traffic <- distinct(experiment_traffic %>% select(experiment_id, variation)) %>%
        arrange(experiment_id, variation)
    
    unique_experiment_variation_info <- experiment_info %>%
        select(-is_baseline) %>%
        arrange(experiment_id, variation)
    
    stopifnot(all(unique_experiment_variation_traffic == unique_experiment_variation_info))
}

#' Checks basic assumptions and data structure of experiment info dataset
#'
#' @param experiment_info dataframe containing experiment info data in the expected format
check_data__experiment_info <- function(experiment_info) {
    
    stopifnot(!any(is.na(experiment_info)))
    
    stopifnot(all(sort(colnames(experiment_info)) == 
                      sort(c("experiment_id", "variation", "is_baseline"))))
    
    info_summary <- experiment_info %>%
        group_by(experiment_id) %>%
        summarise(num_variations=n(),
                  baseline_count=sum(is_baseline),
                  variation_count=sum(!is_baseline))
    stopifnot(all(info_summary$num_variations == 2))
    stopifnot(all(info_summary$baseline_count == 1))
    stopifnot(all(info_summary$variation_count == 1))
}

#' Checks basic assumptions and data structure of attribution window dataset
#'
#' @param attribution_windows dataframe containing attribution window data in the expected format
check_data__attribution_windows <- function(attribution_windows, experiment_info) {
    
    stopifnot(!any(is.na(attribution_windows)))
    stopifnot(all(colnames(attribution_windows) == c('experiment_id', 'metric_id', 'attribution_window')))

    # ensure that there is a 1-1 relationship between the experiment_id in experiment_info and
    # attribution_windows
    stopifnot(all(sort(unique(experiment_info$experiment_id)) == sort(unique(attribution_windows$experiment_id))))
}

#' Checks basic assumptions and data structure of conversion rate dataset
#'
#' @param user_conversion_rates dataframe containing conversion rate data in the expected format
check_data__conversion_rates <- function(user_conversion_rates, attribution_windows) {
    
    stopifnot(!any(is.na(user_conversion_rates)))

    stopifnot(all(colnames(user_conversion_rates) == c('user_id', 'metric_id', 'conversion_date')))

    stopifnot(all(sort(unique(user_conversion_rates$metric_id)) == sort(unique(attribution_windows$metric_id))))

    conversion_summary <- user_conversion_rates %>%
        group_by(metric_id) %>%
        summarise(duplicate_user_ids = any(duplicated(user_id)))
    stopifnot(all(conversion_summary$duplicate_user_ids == FALSE))
}

##############################################################################################################
# WEBSITE TRAFFIC FUNCTIONS
##############################################################################################################

#' Gets the row of the first visit for each user-id
#'
#' @param website_traffic dataframe containing website traffic data in the expected format
website_traffic__get_user_first_visit <- function(website_traffic) {
    
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
#' @param website_traffic dataframe containing website traffic data in the expected format
#' @param top_n_paths if specified, count by the top (i.e. highest traffic) paths, grouping the remaining
#'      paths into an 'Other' category. 
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to website)
website_traffic__to_daily_num_users <- function(website_traffic,
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
#'      represented in >=1 rows for a single cohorted period when top_n_paths is not NULL, but will only be count
#'      once in a cohorted period when top_n_paths is NULL
#' 
#' @param website_traffic dataframe containing website traffic data in the expected format
#' @param top_n_paths if specified, count by the top (i.e. highest traffic) paths, grouping the remaining
#'      paths into an 'Other' category.
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to website)
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
# Sample Size Functions
##############################################################################################################

#' Calculates the total sample size required (assumes 2 variations) to run an A/B test
#' 
#' @param original_conversion_rate the baseline/original conversion rate
#' @param percent_increase the percent increase (relative to the `original_conversion_rate`) that you want to
#'      be able to detect. Also known as "minimum detectable effect".
#' @param power probability of a True Positive (i.e. you detect the effect if it exists)
#' @param alpha probability of a False Positive (i.e. an effect is detected when it does not actually exist)
calculate_total_sample_size <- function(original_conversion_rate,
                                        percent_increase,
                                        power=0.8,
                                        alpha=0.05) {
    if(original_conversion_rate == 0) {

        return (Inf)
    }

    new_conversion_rate <- original_conversion_rate + (original_conversion_rate * percent_increase)
    if(new_conversion_rate > 1) {

        new_conversion_rate <- 1
    }

    return (ceiling(power.prop.test(p1=original_conversion_rate,
                                    p2=new_conversion_rate,
                                    power=power,
                                    sig.level=alpha)$n*2))
}

#' Calculates the number of days required to run an A/B test
#' 
#' @param daily_traffic the epxected number of people that will enter into the experiment (e.g. see the test
#'      on your web-page) per day
#' @param conversion_rates a vector of baseline/original conversion rates
#' @param percent_increase the percent increase (relative to the `original_conversion_rate`) that you want to
#'      be able to detect. Also known as "minimum detectable effect".
#' @param power probability of a True Positive (i.e. you detect the effect if it exists)
#' @param alpha probability of a False Positive (i.e. an effect is detected when it does not actually exist)
calculate_days_required <- function(daily_traffic,
                                    conversion_rates,  # vector of conversion_rates
                                    percent_increase,
                                    power=0.8,
                                    alpha=0.05) {
    entities_required <- unlist(map(conversion_rates, ~ calculate_total_sample_size(
            original_conversion_rate=.,
            percent_increase=percent_increase,
            power=power,
            alpha=alpha)))

    days_required <- ceiling(entities_required / daily_traffic)

    return (ifelse(days_required > 365, Inf, days_required))
    
}

##############################################################################################################
# MISC FUNCTIONS
##############################################################################################################
#' Creates "cohorts" in the form of YYYY-xx where `xx` is based on the `cohort_format` supplied
#' 
#' @param date_vector a vector of dates from which the cohorts will be based on
#' @param cohort_format the string format of the cohort (e.g. %W for week number, %m for month number)
#'      defaults to `%W` (Week 00-53 with Monday as first day of the week)
create_cohort <- function(date_vector, cohort_format='%W') {

    return (format(date_vector, paste0('%Y-', cohort_format)))
}

#' Formats numeric values
#'
#' @param values a numeric vector
prettify_numerics <- function(values) {

    if(max(values) > 1000000) {

        values <- paste0(round(values / 1000000, 2), 'M')

    } else if(max(values) > 100000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values) > 10000) {

        values <- paste0(round(values / 1000, 1), 'K')

    } else if(max(values) > 1000) {

        values <- paste0(round(values / 1000, 2), 'K')

    } else if(max(values) > 100) {

        values <- round(values, 0)

    } else if(max(values) > 1) {

        values <- round(values, 1)

    } else if(max(values) > 0.1) {

        values <- round(values, 2)

    } else {

        values <- formatC(values, format = "e", digits = 2)
    }

    return (values)
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
#' @param user_conversion_rates
experiments__get_experiment_conversion_rates <- function(experiment_traffic, attribution_windows, user_conversion_rates) {

    # dataset only contains users that converted
    # dataset is per user, per experment, per converted metric
    user_conversion_rates <-  experiment_traffic %>% 
        # duplicates user-records when user has converted with multiple metrics
        inner_join(user_conversion_rates, by='user_id') %>%
        # now we need to get the attribution time windows to figure out if they converted within the time-frame
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
               converted_within_window = days_from_experiment_to_conversion >= 0 & days_from_experiment_to_conversion <= attribution_window,
               metric_id = fct_reorder(metric_id, attribution_window)) %>%
        select(user_id, experiment_id, variation, first_joined_experiment, metric_id, conversion_date, attribution_window, days_from_experiment_to_conversion, converted_within_window)
    
        return (user_conversion_rates)
}

get_p_values_info <- function(baseline_successes, baseline_trials, variant_successes, variant_trials) {

    test_results <- prop.test(x=c(variant_successes, baseline_successes),
                              n=c(variant_trials, baseline_trials))
    return (c(p_value = test_results$p.value,
              cr_diff_estimate = as.numeric(test_results$estimate[1] - test_results$estimate[2]),
              conf.low = test_results$conf.int[1],
              conf.high = test_results$conf.int[2]))
}