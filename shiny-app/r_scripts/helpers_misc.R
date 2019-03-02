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

#' returns a dataframe's column as a vector
#'
#' @param df a data.frame
#' @param column the column to return as a vector
#' @param return_unique if TRUE, return unique values
get_vector <- function(df, column, return_unique=FALSE) {

    if(return_unique) {

        return (unique(df[[column]]))
    
    } else {

        return (df[[column]])
    }
}

#' returns the vector (`vec`) without the specified value (`val`). If `val` doesn't exist in `vec`, `vec` is
#' returned unchanged.
#'
#' @param vec the vector
#' @param val the value to remove
remove_val <- function(vec, val) {

    return (vec[!vec %in% val])
}

#' Returnes the ceiling of the **absolute value** of `y`, rounded to the nearest_x.
#'
#' @param y the value
#' @param nearest_x the decimal value to round the ceiling to
ceiling_nearest_x <- function(y, nearest_x) {

    y_trans <- ceiling(abs(y)/nearest_x)*nearest_x
    if(y < 0) {
        y_trans <- y_trans * -1
    }
    # round to nearest 10 because computers can't handle decimals
    return (round(y_trans, 10))
}

#' Like `stopifnot`, but stop `stopifnot` stops if the expression is not true, and `stopif` stops if the
#' expression is true. Avoids the unintuitive double-negative e.g. (`stopifnot(!espression)`) which becomes
#' `stopif(espression)`.
#'
#' @param exprs the expression
stopif <- function(exprs) {

    stopifnot(!exprs)
}


#' compares two dataframes and returns TRUE if they are both equal
#'
#' @param dataframe1 a dataframe to compare
#' @param dataframe2 a dataframe to compare
are_dataframes_equal <- function(dataframe1, dataframe2) {
    return (all(rownames(dataframe1) == rownames(dataframe2)) &&
                all(colnames(dataframe1) == colnames(dataframe2)) &&
                # if either df1 or df2 is NA, then both should be NA
                all(ifelse(is.na(dataframe1) | is.na(dataframe2),
                       is.na(dataframe1) & is.na(dataframe2),
                       dataframe1 == dataframe2)))
}
