library(scales)

source('definitions.R')
source('helpers_misc.R')


#' Plots Website Traffic over time
#' 
#' @param experiment_data list of data-frames from load_data
#' @param only_first_time_visits only count the first time the user appears in the dataset (i.e. first time to website)
#' @param is_weekly if TRUE, groups/cohorts data by week, if FALSE then groups by month
#' @param filter_year_end_beginning_weeks if TRUE (and if is_weekly is TRUE) then it excludes weeks 0 (first
#'      week number of the year) and 53 (last week number of the year) because both are only partial weeks, and 
#'      will make the traffic appear to drop during those weeks.
#' @param top_n_paths if specified, the graph color the lines by path, and count by the top (i.e. highest 
#'      traffic) paths, grouping the remaining paths into an 'Other' category.
plot__website_traffic <- function(experiment_data,
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
        subtitle <- paste0("Represents the number of new users to the website for a given ",
                           tolower(cohort_name),".")
        y_label <- paste0("First-Time User Visits (per ", cohort_name, ")")
        
    } else {
        title <- paste("Unique User Visits Per", cohort_name)
        subtitle <- paste0("Users are only counted once in the given ",
                           tolower(cohort_name),
                           "but same user may be represented in multiple ",
                           tolower(cohort_name), ".")
        y_label <- paste0("Unique User Visits (per ", cohort_name, ")")
    }

    num_users_data <- website_traffic__to_cohort_num_users(experiment_data,
                                                           cohort_format = cohort_format,
                                                           top_n_paths = top_n_paths,
                                                           only_first_time_visits = only_first_time_visits)
        
    if(is_weekly && filter_year_end_beginning_weeks) {
        
        num_users_data <- num_users_data %>%
            filter(!str_detect(string=cohort, pattern='-00') & !str_detect(string=cohort, pattern='-53'))
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

#' Plots the bayesian posterior control/variant/prior distributions of a particular experiment/metric.
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
#' @param metric
#' @param show_prior_distribution
plot__bayesian_posterior <- function(experiments_summary,
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
    bayesian_prob_variant_gt_control <- local_experiment$bayesian_prob_variant_gt_control
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

    # depending on the where we want to graph and how spread out the values are, we will want to get more/less
    # granualar with our plot

    distro_names <- c("Control", "Variant", "Prior")  # don't change order, controlled from above
    distros <- data.frame(alpha = alpha_vector,
                          beta = beta_vector,
                          group = distro_names) %>%
        group_by(alpha, beta, group) %>%
        do(tibble(x = seq(x_min, x_max, x_axis_spread / 1000))) %>%
        ungroup() %>%
        mutate(y = dbeta(x, alpha, beta),
               Parameters = factor(paste0(group, ": alpha= ",
                                          comma_format()(alpha),
                                          ", beta= ",
                                          comma_format()(beta))))

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
    custom_colors <- global__colors_bayesian
    
    if(!show_prior_distribution) {

        distros <- distros %>% filter(!str_detect(Parameters, "Prior"))

        custom_colors <- custom_colors %>% remove_val(global__colors_prior)
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
        mutate(Parameters = factor(distros$Parameters,
               levels = c(param_levels[3], param_levels[1], param_levels[2])))
    
    plot_object <- ggplot(data=distros, aes(x, y, color = Parameters)) +
        geom_line() +
        geom_area(aes(fill=Parameters, group=Parameters), alpha=0.3, position = 'identity') +
        geom_errorbarh(aes(xmin = control_cred_low, xmax = control_cred_high, y = max_distros_20th * -1),
                       height = max_distros_20th * 0.75, color = global__colors_control, alpha=0.3) +
        geom_errorbarh(aes(xmin = variant_cred_low, xmax = variant_cred_high, y = max_distros_20th * -2),
                       height = max_distros_20th * 0.75, color = global__colors_variant, alpha=0.3) +
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
             subtitle=paste0(paste0('The probability the Variant is better is ',
                                    percent(bayesian_prob_variant_gt_control), ".\n"),
                             paste0('\nExperiment: "', experiment, '"'),
                             paste0('\nMetric: "', metric, '"'),
                             paste0('\nControl Name: "', control_name, '"'),
                             paste0('\nVariant Name: "', variant_name, '"')),
             x="Conversion Rates",
             y="Density of beta")

    return (plot_object)
}

#' Plots the control vs variant conversion rates for all 
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
plot__conversion_rates <- function(experiments_summary, experiment) {
    get_type <- function(type) {
        
        if(type == 'p_value') {
            return (type)
        }
        
        return (str_split(type, '_', simplify=TRUE)[2])
    }

    modified_summary <- experiments_summary %>%
        filter(experiment_id == experiment) %>%
        select(metric_id, 
               control_successes, control_trials, control_conversion_rate, 
               variant_successes, variant_trials, variant_conversion_rate,
               p_value) %>%
        gather(type, value, -metric_id) %>%
        mutate(variation=ifelse(type != 'p_value' & str_detect(type, 'control'), 'Control', 'Variant')) %>%
        mutate(actual_type=map_chr(type, ~ get_type(.))) %>%
        mutate(actual_type=ifelse(actual_type == 'conversion', 'conversion_rate', actual_type)) %>%
        select(-type) %>%
        spread(actual_type, value) %>%
        group_by(metric_id) %>%
        mutate(p_value=min(p_value, na.rm=TRUE)) %>%
        ungroup()

    modified_summary %>%
        ggplot(aes(x=metric_id, y=conversion_rate, group=variation, fill=variation)) +
            #geom_col(position='dodge') +
            geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.75) +
            scale_y_continuous(labels=percent_format()) +
            coord_cartesian(ylim=c(0, max(modified_summary$conversion_rate) + 0.05)) +
            geom_text(aes(label= percent_format()(conversion_rate)),
                      position=position_dodge(width=.8), check_overlap=TRUE, vjust=-3, 
                      size=rel(global__text_size)) + 
            geom_text(aes(label= comma_format()(successes)),
                      position=position_dodge(width=.8), check_overlap=TRUE, vjust=-1.75,
                      size=rel(global__text_size)) + 
            geom_text(aes(label= comma_format()(trials)),
                      position=position_dodge(width=.8), check_overlap=TRUE, vjust=-0.5,
                      size=rel(global__text_size)) + 
            #facet_wrap(~ track_field_name, ncol=1, scales='free_x') +
            scale_fill_manual(values=c(global__colors_control, global__colors_variant)) +
            theme_light(base_size=global__theme_base_size) +
            theme(axis.text.x=element_text(angle=35, hjust=1)) +
            labs(caption="",
                 x="Metric",
                 y="Conversion Rate",
                 fill="Variation")
}

#' Plots the percent change from the Control to the Variant along with p-values
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
plot__percent_change_frequentist <- function(experiments_summary, experiment) {

    current_experiments <- experiments_summary %>%
        filter(experiment_id == experiment)
    
    nearest_x <- 0.05
    min_perc_change <- min(0,
                           ceiling_nearest_x(min(current_experiments$percent_change_from_control),
                                             nearest_x))
    max_perc_change <- ceiling_nearest_x(max(current_experiments$percent_change_from_control),
                                         nearest_x)
    
    plot_object <- current_experiments %>%
        ggplot(aes(x=metric_id, y=percent_change_from_control, fill=p_value <= global__p_value_threshold)) +
        geom_col()
    
    # if there are any experiments where the percent change is greater than 0, add text
    # if i don't do the check, i get an error
    t <- current_experiments %>% filter(percent_change_from_control >= 0)
    if(nrow(t) > 0) {
        plot_object <- plot_object +
            geom_text(data=t,
                      aes(label=paste(percent(percent_change_from_control), 'Change')),
                      vjust=-2, size=rel(global__text_size), check_overlap=TRUE) +
            geom_text(data=t,
                      aes(label=paste(round(p_value, 3), 'p-value')),
                      vjust=-0.5, size=rel(global__text_size), check_overlap=TRUE)    
    }
    
    # if there are any experiments where the percent change is less than 0, add text
    # if i don't do the check, i get an error
    t <- current_experiments %>% filter(percent_change_from_control < 0)
    if(nrow(t) > 0) {
        plot_object <- plot_object +
            geom_text(data=t,
                      aes(label=paste(percent(percent_change_from_control), 'Change')),
                      vjust=2.7, size=rel(global__text_size), check_overlap=TRUE) +
            geom_text(data=t,
                      aes(label=paste(round(p_value, 3), 'p-value')),
                      vjust=1.2, size=rel(global__text_size), check_overlap=TRUE)   
    }
    
    # in the case that that all the p-values are statistically significant, the colors will be wrong
    if(all(current_experiments$p_value <= global__p_value_threshold)) {

        fill_colors <- global__colors_good

    } else {

        fill_colors <- c(global__colors_bad, global__colors_good)
    }

    plot_object +
        coord_cartesian(ylim=c(min_perc_change - 0.02, max_perc_change + 0.02)) +
        scale_y_continuous(labels=percent_format(),
                           breaks=seq(min_perc_change, max_perc_change, nearest_x)) +
        scale_fill_manual(values=fill_colors) +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x=element_text(angle=35, hjust=1)) +
        labs(caption=paste0("\nThe decimal value next to the bar represents the p-value.",
                            paste("\nThe p-value threshold for statistical significance is",
                                  global__p_value_threshold)),
             x="Metric",
             y="Percent Change from Control to Variant",
             fill="Statistically Significant")
}

#' Plots the percent change from the Control to the Variant along with the probability that the Variant is
#' better than the control.
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
plot__percent_change_bayesian <- function(experiments_summary, experiment) {
    
    current_experiments <- experiments_summary %>%
        filter(experiment_id == experiment) %>%
        select(-percent_change_from_control) %>%
        mutate(percent_change_from_control = bayesian_cr_difference / bayesian_control_cr)
    
    nearest_x <- 0.05
    min_perc_change <- ceiling_nearest_x(min(current_experiments$percent_change_from_control),
                                         nearest_x)
    max_perc_change <- ceiling_nearest_x(max(current_experiments$percent_change_from_control),
                                         nearest_x)

    plot_object <- current_experiments %>%
        ggplot(aes(x=metric_id, y=percent_change_from_control, fill=bayesian_prob_variant_gt_control)) +
        geom_col()

    # if there are any experiments where the percent change is greater than 0, add text
    # if i don't do the check, i get an error
    t <- current_experiments %>% filter(percent_change_from_control >= 0)
    if(nrow(t) > 0) {
        plot_object <- plot_object +
            geom_text(data=t,
                      aes(label=paste(percent(percent_change_from_control), 'Change')),
                      vjust=-2, size=rel(global__text_size), check_overlap=TRUE) +
            geom_text(data=t,
                      aes(label=paste(percent(bayesian_prob_variant_gt_control), 'Probability')),
                      vjust=-0.5, size=rel(global__text_size), check_overlap=TRUE)
    }

    # if there are any experiments where the percent change is less than 0, add text
    # if i don't do the check, i get an error
    t <- current_experiments %>% filter(percent_change_from_control < 0)
    if(nrow(t) > 0) {
        plot_object <- plot_object +
            geom_text(data=t,
                      aes(label=paste(percent(percent_change_from_control), 'Change')),
                      vjust=2.7, size=rel(global__text_size), check_overlap=TRUE) +
            geom_text(data=t,
                      aes(label=paste(percent(bayesian_prob_variant_gt_control), 'Probability')),
                      vjust=1.2, size=rel(global__text_size), check_overlap=TRUE)
    }

    plot_object +
        coord_cartesian(ylim=c(min_perc_change - 0.02, max_perc_change + 0.02)) +
        scale_y_continuous(labels=percent_format(),
                           breaks=seq(min_perc_change, max_perc_change, nearest_x)) +
        #scale_fill_manual(values=c(global__colors_bad, global__colors_good)) +
        scale_fill_gradient2(low = global__colors_bad, mid = "gray",
                             high = global__colors_good, midpoint = 0.5, limits=c(0,1)) +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x=element_text(angle=35, hjust=1)) +
        labs(caption="\nThe decimal value next to the bar represents the Probability that the Variant is Better.",
             x="Metric",
             y="Percent Change from Control to Variant",
             fill="Probability Variant is Better")
}

#' Plots the percent change (with confidence intervals) from the Control to the Variant along with p-values
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
plot__percent_change_conf_frequentist <- function(experiments_summary, experiment) {

    current_experiments <- experiments_summary %>%
        filter(experiment_id == experiment)
    
    current_experiments <- current_experiments %>%
        select(metric_id,
               control_conversion_rate,
               p_value,
               percent_change_from_control,
               contains('frequentist')) %>%
        mutate(percent_change_conf_low=frequentist_conf_low / control_conversion_rate,
               percent_change_conf_high=frequentist_conf_high / control_conversion_rate,
               p_value_sig = p_value <= global__p_value_threshold) %>%
        select(metric_id, contains('percent_change'), p_value, p_value_sig)
    
    y_expand <- 0.10
    min_y <- ceiling_nearest_x(min(current_experiments$percent_change_conf_low), y_expand)
    max_y <- ceiling_nearest_x(max(current_experiments$percent_change_conf_high), y_expand)
    
    # in the case that that all the p-values are statistically significant, the colors will be wrong
    if(all(current_experiments$p_value_sig)) {

        fill_colors <- global__colors_good

    } else {

        fill_colors <- c(global__colors_bad, global__colors_good)
    }

    current_experiments %>%
        ggplot(aes(x=metric_id, y=percent_change_from_control, color=p_value_sig)) +
        geom_point(size=2) +
        geom_errorbar(aes(ymin=percent_change_conf_low, ymax=percent_change_conf_high), size=0.8) +
        geom_text(aes(label=percent(percent_change_from_control)),
                  hjust=1.2, size=rel(global__text_size), check_overlap=TRUE, color='black') +
        geom_text(aes(y=max_y + y_expand, label=paste('p-value:', round(p_value, 3))),
                  vjust=1, size=rel(global__text_size), check_overlap=TRUE, color='black') +
        geom_hline(yintercept=0, color='#EB5424', size=1.2, alpha=0.5) +
        coord_cartesian(ylim=c(min_y - y_expand, max_y + y_expand)) +
        scale_color_manual(values=fill_colors) +
        scale_y_continuous(labels=percent_format(), breaks=seq(min_y - y_expand, max_y + y_expand, y_expand)) +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x=element_text(angle=35, hjust=1)) +
        labs(caption=paste0("\nThe decimals at the top of the graph are the corresponding p-values.",
                            paste("\nThe p-value threshold for statistical significance is",
                                  global__p_value_threshold)),
             x="Metric",
             y="Percent Change from Control to Variant",
             color="Statistically Significant")
}

#' Plots the percent change (with confidence intervals) from the Control to the Variant along with the
#' probability that the Variant is better than the control.
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
plot__percent_change_conf_bayesian <- function(experiments_summary, experiment) {

    current_experiments <- experiments_summary %>%
        filter(experiment_id == experiment)
    
    current_experiments <- current_experiments %>%
        select(metric_id,
               contains('bayesian')) %>%
        mutate(percent_change_from_control=bayesian_cr_difference / bayesian_control_cr,
               percent_change_conf_low=bayesian_conf_low / bayesian_control_cr,
               percent_change_conf_high=bayesian_conf_high / bayesian_control_cr) %>%
        select(metric_id, contains('percent_change'), bayesian_prob_variant_gt_control)
    
    y_expand <- 0.10
    min_y <- ceiling_nearest_x(min(current_experiments$percent_change_conf_low), y_expand)
    max_y <- ceiling_nearest_x(max(current_experiments$percent_change_conf_high), y_expand)
    
    current_experiments %>%
        ggplot(aes(x=metric_id, y=percent_change_from_control, color=bayesian_prob_variant_gt_control)) +
        geom_point(size=2) +
        geom_errorbar(aes(ymin=percent_change_conf_low, ymax=percent_change_conf_high), size=0.8) +
        geom_text(aes(label=percent(percent_change_from_control)),
                  hjust=1.2, size=rel(global__text_size), check_overlap=TRUE, color='black') +
        geom_text(aes(y=max_y + y_expand, label=paste(percent(bayesian_prob_variant_gt_control), "Probability")),
                  vjust=1, size=rel(global__text_size), check_overlap=TRUE, color='black') +
        geom_hline(yintercept=0, color='#EB5424', size=1.2, alpha=0.5) +
        coord_cartesian(ylim=c(min_y - y_expand, max_y + y_expand)) +
        scale_color_gradient2(low = global__colors_bad, mid = "gray",
                              high = global__colors_good, midpoint = 0.5, limits=c(0,1)) +
        #scale_color_manual(values=c(global__colors_bad, global__colors_good)) +
        scale_y_continuous(labels=percent_format(), breaks=seq(min_y - y_expand, max_y + y_expand, y_expand)) +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x=element_text(angle=35, hjust=1)) +
        labs(caption="\nThe decimals at the top of the graph is the probability the Variant is better than the Control.",
             x="Metric",
             y="Percent Change from Control to Variant",
             color="Probability Variant is Better")
}
