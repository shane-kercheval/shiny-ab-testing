##############################################################################################################
# Check Dataset Assumptions
##############################################################################################################
check_website_traffic <- function(website_traffic) {
    
    stopifnot(all(colnames(website_traffic) == c('user_id', 'visit_date', 'path')))
    # ensure there are no duplicated rows i.e. unique user/date/path
    stopifnot(nrow(website_traffic) == nrow(distinct(website_traffic)))
}



##############################################################################################################
# Sample Size Functions
##############################################################################################################
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

create_cohort <- function(date_vector, cohort_format='%W') {
    # defaults to %W (Week 00-53 with Monday as first day of the week)
    return (format(date_vector, paste0('%Y-', cohort_format)))
}
