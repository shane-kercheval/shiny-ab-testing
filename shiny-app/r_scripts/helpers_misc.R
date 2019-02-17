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
