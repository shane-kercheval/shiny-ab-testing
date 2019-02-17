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
    experiments_summary <- experiment_traffic %>%
        group_by(experiment_id) %>%
        summarise(duplicate_user_ids = any(duplicated(user_id)),
                  variation_count = length(unique(variation)))
    
    stopifnot(!any(experiments_summary$duplicate_user_ids))
    stopifnot(all(experiments_summary$variation_count == 2))

    # ensure that there is a 1-1 relationship between the experiment_id/variation in experiment_info and 
    # website_traffic
    unique_experiment_variation_traffic <- distinct(experiment_traffic %>%
        select(experiment_id, variation)) %>%
        arrange(experiment_id, variation)
    
    unique_experiment_variation_info <- experiment_info %>%
        select(-is_control) %>%
        arrange(experiment_id, variation)
    
    stopifnot(all(unique_experiment_variation_traffic == unique_experiment_variation_info))
}

#' Checks basic assumptions and data structure of experiment info dataset
#'
#' @param experiment_info dataframe containing experiment info data in the expected format
check_data__experiment_info <- function(experiment_info) {
    
    stopifnot(!any(is.na(experiment_info)))
    
    stopifnot(all(sort(colnames(experiment_info)) == 
                      sort(c("experiment_id", "variation", "is_control"))))
    
    info_summary <- experiment_info %>%
        group_by(experiment_id) %>%
        summarise(num_variations=n(),
                  control_count=sum(is_control),
                  variation_count=sum(!is_control))
    stopifnot(all(info_summary$num_variations == 2))
    stopifnot(all(info_summary$control_count == 1))
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
    stopifnot(all(sort(unique(experiment_info$experiment_id)) == 
                      sort(unique(attribution_windows$experiment_id))))
}

#' Checks basic assumptions and data structure of conversion rate dataset
#'
#' @param user_conversion_events dataframe containing conversion rate data in the expected format
check_data__conversion_events <- function(user_conversion_events, attribution_windows) {
    
    stopifnot(!any(is.na(user_conversion_events)))

    stopifnot(all(colnames(user_conversion_events) == c('user_id', 'metric_id', 'conversion_date')))

    stopifnot(all(sort(unique(user_conversion_events$metric_id)) == 
                      sort(unique(attribution_windows$metric_id))))

    conversion_summary <- user_conversion_events %>%
        group_by(metric_id) %>%
        summarise(duplicate_user_ids = any(duplicated(user_id)))
    stopifnot(all(conversion_summary$duplicate_user_ids == FALSE))
}
