##############################################################################################################
# File Paths
##############################################################################################################
simulated_data_path__attribution_windows <- 'simulated_data/attribution_windows.csv'
simulated_data_path__website_traffic <- 'simulated_data/website_traffic.csv'
simulated_data_path__experiment_traffic <- 'simulated_data/experiment_traffic.csv'
simulated_data_path__conversion_rate_data <- 'simulated_data/conversion_rate_data.csv'

##############################################################################################################
# Helper Functions
##############################################################################################################
create_cohort <- function(date_object, cohort_method=month, pad_width=2) {

    return (
        paste0(year(date_object),
               '-',
               str_pad(cohort_method(date_object), width=pad_width, side='left', pad = '0'))
    )
}