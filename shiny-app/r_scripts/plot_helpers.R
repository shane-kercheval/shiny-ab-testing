#' Plots Website Traffic over time
#' 
#' @param website_traffic dataframe containing website traffic data in the expected format
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to website)
#' @param is_weekly if TRUE, groups/cohorts data by week, if FALSE then groups by month
#' @param filter_year_end_beginning_weeks if TRUE (and if is_weekly is TRUE) then it excludes weeks 0 (first
#'      week number of the year) and 53 (last week number of the year) because both are only partial weeks, and 
#'      will make the traffic appear to drop during those weeks.
#' @param top_n_paths if specified, the graph color the lines by path, and count by the top (i.e. highest 
#'      traffic) paths, grouping the remaining paths into an 'Other' category.
website_traffic__plot_traffic <- function(website_traffic,
                                          only_first_time_visits = FALSE,
                                          is_weekly = TRUE,
                                          filter_year_end_beginning_weeks = TRUE,
                                          top_n_paths = NULL) {
    if(is_weekly) {
        
        cohort_format <- '%W'
        cohort_name <- "Week"
        
    } else {
        
        cohort_format <- '%m'
        cohort_name <- "Month"
    }
    
    caption <- ""
    if(only_first_time_visits) {
        
        title <- paste("First-Time User Visits Per", cohort_name)
        subtitle <- paste0("Represents the number of new users to the website for a given ", tolower(cohort_name),".")
        y_label <- paste0("First-Time User Visits (per ", cohort_name, ")")
        
    } else {
        title <- paste("Unique User Visits Per", cohort_name)
        subtitle <- paste0("Users are only counted once in the given ",
                           tolower(cohort_name),
                           "but same user may be represented in multiple ",
                           tolower(cohort_name), ".")
        y_label <- paste0("Unique User Visits (per ", cohort_name, ")")
    }
    
    num_users_data <- website_traffic__to_cohort_num_users(website_traffic,
                                                           cohort_format = cohort_format,
                                                           top_n_paths = top_n_paths,
                                                           only_first_time_visits = only_first_time_visits)
        
    if(is_weekly && filter_year_end_beginning_weeks) {
        
        num_users_data <- num_users_data %>% filter(!str_detect(string=cohort, pattern='-00') & !str_detect(string=cohort, pattern='-53'))
        caption <- "\nPartial weeks at the end and beginning of the year are excluded."
    }
    
    if(is.null(top_n_paths)) {
    
        plot_object <- num_users_data %>% ggplot(aes(x=cohort, y=num_users, group = 1))
    
    } else {
        
        plot_object <- num_users_data %>% 
            rename(Path=path) %>%
            ggplot(aes(x=cohort, y=num_users, group=Path, color=Path))
    }

    plot_object +
        geom_line() +
        geom_point() +
        expand_limits(y = 0) +
        geom_text(aes(label = prettify_numerics(num_users)), check_overlap=TRUE, vjust = -0.5) +
        scale_y_continuous(labels = comma_format()) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        labs(title = title,
             subtitle = subtitle,
             x = cohort_name,
             y = y_label,
             caption = caption)
}

experiments_summary__plot_bayesian <- function(experiments_summary,
                                               experiment,
                                               metric,
                                               show_prior_distribution=TRUE) {
    
    local_experiment <-  experiments_summary %>%
        filter(experiment_id  == experiment & metric_id == metric)
    
    prior_alpha <- local_experiment$prior_alpha
    prior_beta <- local_experiment$prior_beta
    control_alpha <- local_experiment$control_alpha
    control_beta <- local_experiment$control_beta
    variant_alpha <- local_experiment$variant_alpha
    variant_beta <- local_experiment$variant_beta
    prob_variant_is_better <- local_experiment$prob_variant_is_better
    control_name <- local_experiment$control_name
    variant_name <- local_experiment$variant_name

    alpha_vector <- c(control_alpha, variant_alpha, prior_alpha)
    beta_vector <-  c(control_beta, variant_beta, prior_beta)

    if(show_prior_distribution) {

        x_min <- min(qbeta(0.001, alpha_vector, beta_vector))
        x_max <- max(qbeta(0.999, alpha_vector, beta_vector))
    
    } else {
    
        x_min <- min(qbeta(0.001, alpha_vector[1:2], beta_vector[1:2]))
        x_max <- max(qbeta(0.999, alpha_vector[1:2], beta_vector[1:2]))
    }

    x_axis_spread <- x_max - x_min

    # depending on the where we want to graph and how spread out the values are, we will want to get more/less granualar with our plot

    distro_names <- c("Control", "Variant", "Prior")  # don't change order, controlled from above
    distros <- data_frame(alpha = alpha_vector,
                          beta = beta_vector,
                          group = distro_names) %>%
        group_by(alpha, beta, group) %>%
        do(tibble(x = seq(x_min, x_max, x_axis_spread / 1000))) %>%
        ungroup() %>%
        mutate(y = dbeta(x, alpha, beta),
               Parameters = factor(paste0(group, ": alpha= ", comma_format()(alpha), ", beta= ", comma_format()(beta))))

    x_axis_break_steps <- 0.05
    if(x_axis_spread <= 0.02) {

        x_axis_break_steps <- 0.001

    } else if(x_axis_spread <= 0.05) {

        x_axis_break_steps <- 0.005

    } else if(x_axis_spread <= 0.15) {

        x_axis_break_steps <- 0.01

    } else if(x_axis_spread <= 0.5) {

        x_axis_break_steps <- 0.02
    }

    #custom_colors <- rev(hue_pal()(3))
    custom_colors <- c('#19A6FF', '#FF9500', '#C396E8')
    
    if(!show_prior_distribution) {

        distros <- distros %>%
            filter(!str_detect(Parameters, "Prior"))

        custom_colors <- custom_colors[1:2]
    }

    control_cred_low <- qbeta(0.025, control_alpha, control_beta)
    control_cred_high <- qbeta(0.975, control_alpha, control_beta)

    variant_cred_low <- qbeta(0.025, variant_alpha, variant_beta)
    variant_cred_high <- qbeta(0.975, variant_alpha, variant_beta)


    cia <- credible_interval_approx(alpha_a=control_alpha,
                                    beta_a=control_beta,
                                    alpha_b=variant_alpha,
                                    beta_b=variant_beta)
    percent_of_time_b_wins <- cia['posterior']

    # a_cr_simulation <- rbeta(1e6, control_alpha, control_beta)
    # b_cr_simulation <- rbeta(1e6, variant_alpha, variant_beta)
    # percent_of_time_b_wins <- mean(b_cr_simulation > a_cr_simulation)

    max_distros_20th <- max(distros$y) / 20
    
    # re-level (re-order) the levels of Params so the order is Control->Variant->Prior
    param_levels <- levels(distros$Parameters)
    distros <- distros %>%
        mutate(Parameters = factor(distros$Parameters, levels = c(param_levels[3], param_levels[1], param_levels[2])))
    
    plot_object <- ggplot(data=distros, aes(x, y, color = Parameters)) +
        geom_line() +
        geom_area(aes(fill=Parameters, group=Parameters), alpha=0.3, position = 'identity') +
        geom_errorbarh(aes(xmin = control_cred_low, xmax = control_cred_high, y = max_distros_20th * -1), height = max_distros_20th * 0.75, color = custom_colors[1], alpha=0.3) + 
        geom_errorbarh(aes(xmin = variant_cred_low, xmax = variant_cred_high, y = max_distros_20th * -2), height = max_distros_20th * 0.75, color = custom_colors[2], alpha=0.3) + 
        scale_x_continuous(breaks = seq(0, 1, x_axis_break_steps),
                           labels = percent_format()) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              legend.text=element_text(size=rel(0.5)),
              plot.subtitle=element_text(size=rel(0.7))) +
        coord_cartesian(xlim=c(x_min, x_max)) +
        scale_fill_manual(values=custom_colors) +
        scale_color_manual(values=custom_colors) +
        labs(title='Posterior Probability Distributions of Control & Variant',
             subtitle=paste0(paste0('The probability the Variant is better is ', percent(prob_variant_is_better), ".\n"),
                             paste0('\nExperiment: "', experiment, '"'),
                             paste0('\nMetric: "', metric, '"'),
                             paste0('\nControl Name: "', control_name, '"'),
                             paste0('\nVariant Name: "', variant_name, '"')),
             x="Conversion Rates",
             y="Density of beta")

    return (plot_object)
}
