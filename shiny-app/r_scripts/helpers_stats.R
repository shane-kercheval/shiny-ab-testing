library(purrr)

##############################################################################################################
# Sample Size Functions
##############################################################################################################

#' Calculates the total sample size required (assumes 2 variations) to run an A/B test
#' 
#' @param original_conversion_rate the control/original conversion rate
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
#' @param conversion_rates a vector of control/original conversion rates
#' @param percent_increase the percent increase (relative to the `original_conversion_rate`) that you want to
#'      be able to detect. Also known as "minimum detectable effect".
#' @param power probability of a True Positive (i.e. you detect the effect if it exists)
#' @param alpha probability of a False Positive (i.e. an effect is detected when it does not actually exist)
ab_test_calculator <- function(daily_traffic,
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

    return (list(days_required=ifelse(days_required > 365, Inf, days_required),
                 entities_required=entities_required))
}

#' Returns a credible interval approximations using the normal distribution, according to the methods in the
#' link below.
#' 
#' code modified from https://github.com/dgrtwo/empirical-bayes-book/blob/master/bayesian-ab.Rmd
#' 
#' @param alpha_a the alpha value for the "A" group
#' @param beta_a the beta value for the "A" group
#' @param alpha_b the alpha value for the "B" group
#' @param beta_b the beta value for the "B" group
credible_interval_approx <- function(alpha_a, beta_a, alpha_b, beta_b, confidence_level=0.95) {
    # https://github.com/dgrtwo/empirical-bayes-book/blob/master/bayesian-ab.Rmd
    u1 <- alpha_a / (alpha_a + beta_a)
    u2 <- alpha_b / (alpha_b + beta_b)
    var1 <- as.double(alpha_a) * beta_a / ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
    var2 <- as.double(alpha_b) * beta_b / ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
    mu_diff <- u2 - u1
    sd_diff <- sqrt(var1 + var2)
    
    confidence_difference <- 1 - confidence_level
    two_sided_difference <- confidence_difference / 2

    # in D.R.'s code, the first player had a higher probability but a negative estimate (i.e. negative
    # difference in conversion rate, mu_diff). This doesn't make sense, so we'll 1) use the first player as
    # the A group and the second as the B group), so B-A which gives the expected intervals (but flips the
    # posterior probability), and 2) use 1-pnorm(...) to get the correct posterior probability
    return (c(posterior = 1 - pnorm(0, mu_diff, sd_diff),
              cr_diff_estimate = mu_diff,
              conf.low = qnorm(two_sided_difference, mu_diff, sd_diff),
              conf.high = qnorm(1 - two_sided_difference, mu_diff, sd_diff),
              conf.low_level = two_sided_difference,
              conf.high_level=1 - two_sided_difference))
}

#' get p-values and corresponding confidence intervals
#' 
#' @param control_successes
#' @param control_trials
#' @param variant_successes
#' @param variant_trials
get_p_values_info <- function(control_successes,
                              control_trials,
                              variant_successes,
                              variant_trials,
                              confidence_level=0.95) {

    test_results <- prop.test(x=c(variant_successes, control_successes),
                              n=c(variant_trials, control_trials),
                              conf.level = confidence_level)

    return (c(p_value = test_results$p.value,
              cr_diff_estimate = as.numeric(test_results$estimate[1] - test_results$estimate[2]),
              conf.low = test_results$conf.int[1],
              conf.high = test_results$conf.int[2],
              conf.level = attr(test_results$conf.int, 'conf.level')))
}
