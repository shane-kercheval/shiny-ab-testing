library('testthat')
source('../shiny-app/r_scripts/helpers.R', chdir=TRUE)

# to run from command line, use:
# library('testthat')
# test_file("test_helpers.R")


    # attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv', row.names = FALSE))
    # website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv', row.names = FALSE))
    # experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv', row.names = FALSE))
    # conversion_rate_data <- as.data.frame(read_csv('../shiny-app/simulated_data/conversion_rate_data.csv', row.names = FALSE))

test_that("test_helpers: create", {



     <- read.csv('./data/apriori_sequence_dataset_start.csv')
    ordered_data <- helper_order(dataset=unordered_data, id_column_name='customer_id', column_to_order_by='event_sequence')
    comparison_data <- read.csv('./data/apriori_sequence_dataset_end.csv')
    expect_true(all(ordered_data == comparison_data))
})



