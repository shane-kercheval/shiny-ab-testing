library('testthat')
source('../shiny-app/r_scripts/helpers.R', chdir=TRUE)

# to run from command line, use:
# library('testthat')
# test_file("test_helpers.R")


    # experiment_info <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_info.csv', row.names = FALSE))
    # attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv', row.names = FALSE))
    # website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv', row.names = FALSE))
    # experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv', row.names = FALSE))
    # conversion_rate_data <- as.data.frame(read_csv('../shiny-app/simulated_data/conversion_rate_data.csv', row.names = FALSE))



##############################################################################################################
# Misc Helpers
##############################################################################################################
test_that("calculate_total_sample_size", {
    
    original_cr <- .95
    percent_increase <- (1-.95)/.95
    original_cr + (original_cr * percent_increase)
    
    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.1,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 7525)

    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 14265)

    result <- calculate_total_sample_size(original_conversion_rate=0.20,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 14265)

    result <- calculate_total_sample_size(original_conversion_rate=0.10,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 14265)
    
    
    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.2,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 1926)

    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.1,
                                          power=0.9,
                                          alpha=0.05)
    expect_true(result == 10073)
    
    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.1,
                                          power=0.9,
                                          alpha=0.01)
    expect_true(result == 14265)
    
    ##########################################################################################################
    # goes from .95 to 1.0
    ##########################################################################################################
    result <- calculate_total_sample_size(original_conversion_rate=0.95,
                                          percent_increase=(1-.95)/.95,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 304)
    ##########################################################################################################
    # this will give a value larger than 1.0, so 1.0 will be used, so it should be the same as the previous
    # results
    ##########################################################################################################
    new_result <- calculate_total_sample_size(original_conversion_rate=0.95,
                                              percent_increase=0.30,
                                              power=0.8,
                                              alpha=0.05)
    expect_true(result == new_result)
    
    ##########################################################################################################
    # test with `0` conversion rate
    ##########################################################################################################
    result <- calculate_total_sample_size(original_conversion_rate=0,
                                          percent_increase=0.10,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(is.infinite(result))
})

test_that("test_helpers: calculate_days_required", {

    calculate_days_required(daily_traffic,
                                    conversion_rates,  # vector of conversion_rates
                                    percent_increase,
                                    power=0.8,
                                    alpha=0.05)
})







##############################################################################################################
# Historical Traffic & Conversion Rates
##############################################################################################################
test_that("test_helpers: create", {

    website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv', row.names = FALSE))



})



