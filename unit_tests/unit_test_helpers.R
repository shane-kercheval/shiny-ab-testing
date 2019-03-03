##############################################################################################################
# File Paths
##############################################################################################################
data_path__experiment_info <- '../shiny-app/simulated_data/experiment_info.csv'
data_path__experiment_traffic <- '../shiny-app/simulated_data/experiment_traffic.csv'
data_path__attribution_windows <- '../shiny-app/simulated_data/attribution_windows.csv'
data_path__website_traffic <- '../shiny-app/simulated_data/website_traffic.csv'
data_path__conversion_events <- '../shiny-app/simulated_data/conversion_events.csv'

# need to scale the output of the log(exponential) distribution to the number of days we are simulating
scale_a_b <- function(x, a, b) {

    (b - a) * ((x - min(x)) / (max(x) - min(x))) + a 
}

get_random_variation <- function(variation_names, user_id) {
    set.seed(user_id)
    return (variation_names[rbinom(1, 1, 0.5) + 1])
}

# generate a new date based on the gamma distribution; note setting the seed based on the user-id,
# which i wanted to do to recreate the same data, causes the distribution to become strange
generate_new_date <- function(visit_index, original_date, user_id) {
    
    # we don't wanto modify the first_visit_date 
    if(visit_index == 1) {

        return (original_date)
    }
    
    #set.seed(user_id)
    offset <- floor(rgamma(1, shape=visit_index + 3))
    return (original_date + max(offset, 1))
}

test_save_plot <- function(plot, file_name, size_inches=c(5, 8)) {

    stopifnot(!is.null(plot))

    if (file.exists(file_name)) file.remove(file_name)

    ggsave(filename=file_name, plot=plot, height=size_inches[1], width=size_inches[2], units='in')
    stopifnot(file.exists(file_name))
}

expect_dataframes_equal <- function(dataframe1, dataframe2){

    expect_true(are_dataframes_equal(dataframe1, dataframe2))
}
