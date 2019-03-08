library('testthat')

source('../shiny-app/r_scripts/helpers_stats.R', chdir=TRUE)
source('unit_test_helpers.R')

# to run from command line, use:
# library('testthat')
# test_file("test_helpers_stats.R")

test_that("calculate_total_sample_size", {
    context("helpers_stats::calculate_total_sample_size")
    # test pre-calculated examples
    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.1,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 7525)

    result <- calculate_total_sample_size(original_conversion_rate=0.30,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 29711)

    result <- calculate_total_sample_size(original_conversion_rate=0.20,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 51165)

    result <- calculate_total_sample_size(original_conversion_rate=0.10,
                                          percent_increase=0.05,
                                          power=0.8,
                                          alpha=0.05)
    expect_true(result == 115526)
    
    
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

test_that("calculate_days_required", {
    context("helpers_stats::calculate_days_required")

    daily_traffic <- 10000
    total_sample_size_required <- c(29711, 51165, 115526)  # from calculate_total_sample_size unit test
    expected_days_required <- ceiling(total_sample_size_required / daily_traffic)
    days_required <- calculate_days_required(daily_traffic=daily_traffic,
                                             conversion_rates=c(0.3, 0.2, 0.1),
                                             percent_increase=0.05,
                                             power=0.8,
                                             alpha=0.05)

    expect_true(all(expected_days_required == days_required))
})

test_that("credible_interval_approx", {
    context("helpers_stats::credible_interval_approx")

    # this is an alternative way of calculating the probability that b > a, simulation of random draws
    simulate_b_wins <- function(alpha_a, beta_a, alpha_b, beta_b, number_of_draws=1e6) {
        
        a_cr_simulation <- rbeta(number_of_draws, alpha_a, beta_a)
        b_cr_simulation <- rbeta(number_of_draws, alpha_b, beta_b)
        
        return ( mean(b_cr_simulation > a_cr_simulation) )
    }

    ##########################################################################################################
    # confidence_level=0.95
    ##########################################################################################################
    
    alpha_a <- 3872
    beta_a <- 8880
    alpha_b <- 2228
    beta_b <- 5071
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(temp_cred[["conf.low_level"]], 0.025)
    expect_equal(temp_cred[["conf.high_level"]], 0.975)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 2) , round(prob_b_better_sim, 2))
    
    alpha_a <- 5111
    beta_a <- 272055
    alpha_b <- 5253
    beta_b <- 272251
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(temp_cred[['conf.low_level']], 0.025)
    expect_equal(temp_cred[['conf.high_level']], 0.975)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 2) , round(prob_b_better_sim, 2))
    
    alpha_a <- 2724
    beta_a <- 144083
    alpha_b <- 2733
    beta_b <- 144247
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(temp_cred[['conf.low_level']], 0.025)
    expect_equal(temp_cred[['conf.high_level']], 0.975)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 2) , round(prob_b_better_sim, 2))
    
    alpha_a <- 22973
    beta_a <- 341979
    alpha_b <- 22206
    beta_b <- 342860
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(temp_cred[['conf.low_level']], 0.025)
    expect_equal(temp_cred[['conf.high_level']], 0.975)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 4) , round(prob_b_better_sim, 4))
    
    ##########################################################################################################
    # confidence_level=0.90
    ##########################################################################################################
    alpha_a <- 3872
    beta_a <- 8880
    alpha_b <- 2228
    beta_b <- 5071
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b, confidence_level=0.90)
    expect_equal(temp_cred[["conf.low_level"]], 0.05)
    expect_equal(temp_cred[["conf.high_level"]], 0.95)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 2) , round(prob_b_better_sim, 2))
    
    alpha_a <- 5111
    beta_a <- 272055
    alpha_b <- 5253
    beta_b <- 272251
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b, confidence_level=0.90)
    expect_equal(temp_cred[['conf.low_level']], 0.05)
    expect_equal(temp_cred[['conf.high_level']], 0.95)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 2) , round(prob_b_better_sim, 2))
    
    alpha_a <- 2724
    beta_a <- 144083
    alpha_b <- 2733
    beta_b <- 144247
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b, confidence_level=0.90)
    expect_equal(temp_cred[['conf.low_level']], 0.05)
    expect_equal(temp_cred[['conf.high_level']], 0.95)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 2) , round(prob_b_better_sim, 2))
    
    alpha_a <- 22973
    beta_a <- 341979
    alpha_b <- 22206
    beta_b <- 342860
    temp_cred <- credible_interval_approx(alpha_a, beta_a, alpha_b, beta_b, confidence_level=0.90)
    expect_equal(temp_cred[['conf.low_level']], 0.05)
    expect_equal(temp_cred[['conf.high_level']], 0.95)
    expect_equal(temp_cred[['cr_diff_estimate']], (alpha_b / (alpha_b + beta_b)) - (alpha_a / (alpha_a + beta_a)))
    prob_b_better_cia <- temp_cred[['posterior']]
    prob_b_better_sim <- simulate_b_wins(alpha_a, beta_a, alpha_b, beta_b)
    expect_equal(round(prob_b_better_cia, 4) , round(prob_b_better_sim, 4))
})

test_that("get_p_values_info", {
    context("helpers_stats::get_p_values_info")
    
    ##########################################################################################################
    # confidence_level=0.95
    ##########################################################################################################
    control_successes <- 4612
    control_trials <- 131189
    variant_successes <- 4140
    variant_trials <- 131116
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.95)

    control_successes <- 3303
    control_trials <- 123077
    variant_successes <- 3526
    variant_trials <- 123100
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.95)

    control_successes <- 2299
    control_trials <- 106879
    variant_successes <- 2373
    variant_trials <- 106899
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.95)

    control_successes <- 1319
    control_trials <- 90865
    variant_successes <- 1370
    variant_trials <- 91064
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.95)

    ##########################################################################################################
    # confidence_level=0.90
    ##########################################################################################################
    control_successes <- 4612
    control_trials <- 131189
    variant_successes <- 4140
    variant_trials <- 131116
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials,
                                   confidence_level=0.90)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.90)

    control_successes <- 3303
    control_trials <- 123077
    variant_successes <- 3526
    variant_trials <- 123100
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials,
                                   confidence_level=0.90)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.90)

    control_successes <- 2299
    control_trials <- 106879
    variant_successes <- 2373
    variant_trials <- 106899
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials,
                                   confidence_level=0.90)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.90)

    control_successes <- 1319
    control_trials <- 90865
    variant_successes <- 1370
    variant_trials <- 91064
    expected_diff <- (variant_successes / variant_trials) - (control_successes / control_trials)
    temp_info <- get_p_values_info(control_successes, control_trials, variant_successes, variant_trials,
                                   confidence_level=0.90)
    expect_equal(temp_info[['cr_diff_estimate']], expected_diff)
    expect_equal(temp_info[['conf.level']], 0.90)
})