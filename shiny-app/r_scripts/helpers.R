calculate_total_sample_size <- function(original_conversion_rate,
                                        percent_increase,
                                        power=0.8,
                                        alpha=0.05) {
    ##########################################################################################################
    # returns the total sample size (i.e. for both A&B groups)
    ##########################################################################################################
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

calculate_days_required <- function(daily_traffic,
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
    
    return (ifelse(days_required > 365, Inf, days_required))
    
}






test_save_plot <- function(plot, file_name, size_inches=c(5, 8)) {

    stopifnot(!is.null(plot))

    if (file.exists(file_name)) file.remove(file_name)

    ggsave(filename=file_name, plot=plot, height=size_inches[1], width=size_inches[2], units='in')
    expect_true(file.exists(file_name))
}
