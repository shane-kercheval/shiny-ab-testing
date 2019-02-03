#' Plots Website Traffic over time
#' 
#' @param website_traffic dataframe containing website traffic data in the expected format
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to website)
#' @param is_weekly if TRUE, groups/cohorts data by week, if FALSE then groups by month
#' @param filter_year_end_beginning_weeks if TRUE (and if is_weekly is TRUE) then it excludes weeks 0 (first
#'      week number of the year) and 53 (last week number of the year) because both are only partial weeks, and 
#'      will make the traffic appear to drop during those weeks.
#' @param top_n_paths if specified, the graph color the lines by path, and count by the top (i.e. highest 
#'      traffic) paths, grouping the remaining paths into an 'Other' category.
website_traffic__plot_traffic <- function(website_traffic,
                                          only_first_time_visits = FALSE,
                                          is_weekly = TRUE,
                                          filter_year_end_beginning_weeks = TRUE,
                                          top_n_paths = NULL) {
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
                                                           cohort_format = cohort_format,
                                                           top_n_paths = top_n_paths,
                                                           only_first_time_visits = only_first_time_visits)
        
    if(is_weekly && filter_year_end_beginning_weeks) {
        
        num_users_data <- num_users_data %>% filter(!str_detect(string=cohort, pattern='-00') & !str_detect(string=cohort, pattern='-53'))
        caption <- "\nPartial weeks at the end and beginning of the year are excluded."
    }
    
    if(is.null(top_n_paths)) {
    
        plot_object <- num_users_data %>% ggplot(aes(x=cohort, y=num_users, group = 1))
    
    } else {
        
        plot_object <- num_users_data %>% 
            rename(Path=path) %>%
            ggplot(aes(x=cohort, y=num_users, group=Path, color=Path))
    }

    plot_object +
        geom_line() +
        geom_point() +
        expand_limits(y = 0) +
        geom_text(aes(label = prettify_numerics(num_users)), check_overlap=TRUE, vjust = -0.5) +
        scale_y_continuous(labels = comma_format()) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        labs(title = title,
             subtitle = subtitle,
             x = cohort_name,
             y = y_label,
             caption = caption)
}
