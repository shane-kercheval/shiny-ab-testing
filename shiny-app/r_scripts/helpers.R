##############################################################################################################
# CHECK DATASET ASSUMPTIONS
##############################################################################################################

#' Checks basic assumptions and data structure of website traffic dataset
#'
#' @param website_traffic dataframe containing website traffic data in the expected format
check_website_traffic <- function(website_traffic) {
    
    stopifnot(all(colnames(website_traffic) == c('user_id', 'visit_date', 'path')))
    # ensure there are no duplicated rows i.e. unique user/date/path
    stopifnot(nrow(website_traffic) == nrow(distinct(website_traffic)))
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
