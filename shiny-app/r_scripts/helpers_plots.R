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
                           ", but the same user may be represented in multiple ",
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
        geom_text(aes(label = prettify_numerics(num_users)),
                  check_overlap=TRUE, vjust = -0.5, size=rel(global__text_size)) +
        scale_y_continuous(labels = comma_format()) +
        theme_light(base_size=global__theme_base_size) +
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
#' @param confidence_level the confidence level passed to `experiments__get_summary()` which generates
#'    `experiment_data`
#' @param show_prior_distribution
plot__bayesian_posterior <- function(experiments_summary,
                                     experiment,
                                     metric,
                                     confidence_level,
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

    confidence_difference <- 1 - confidence_level
    two_sided_difference <- confidence_difference / 2

    control_cred_low <- qbeta(two_sided_difference, control_alpha, control_beta)
    control_cred_high <- qbeta(1 - two_sided_difference, control_alpha, control_beta)

    variant_cred_low <- qbeta(two_sided_difference, variant_alpha, variant_beta)
    variant_cred_high <- qbeta(1 - two_sided_difference, variant_alpha, variant_beta)

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
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              legend.text=element_text(size=rel(0.9)),
              plot.subtitle=element_text(size=rel(0.9))) +
        coord_cartesian(xlim=c(x_min, x_max)) +
        scale_fill_manual(values=custom_colors) +
        scale_color_manual(values=custom_colors) +
        labs(#title='Posterior Probability Distributions of Control & Variant',
             subtitle=paste0(paste0('The probability the Variant is better is ',
                                    percent(bayesian_prob_variant_gt_control), ".\n"),
                             #paste0('\nExperiment: "', experiment, '"'),
                             #paste0('\nMetric: "', metric, '"'),
                             paste0('\nControl Name: "', control_name, '"'),
                             paste0('\nVariant Name: "', variant_name, '"')),
             x="Conversion Rates",
             y="Density of beta",
             caption=paste("\nThe lines under each distribution show the corresponding",
                           percent(confidence_level),
                           "confidence interval."))

    return (plot_object)
}

#' Plots the control vs variant conversion rates for all metrics
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
        mutate(p_value=min(p_value, na.rm=TRUE),
               highest_conversion_rate = max(conversion_rate)) %>%
        ungroup()

    modified_summary %>%
        ggplot(aes(x=metric_id, y=conversion_rate, group=variation, fill=variation)) +
            #geom_col(position='dodge') +
            geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.75) +
            scale_y_continuous(labels=percent_format()) +
            coord_cartesian(ylim=c(0, max(modified_summary$conversion_rate) + 0.05)) +
            geom_text(aes(y=highest_conversion_rate, label= percent_format()(conversion_rate)),
                      position=position_dodge(width=.8), check_overlap=TRUE, vjust=-4, 
                      size=rel(global__text_size + 0.5)) + 
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

#' Plots the control vs variant conversion rates based on the bayesian methodology, for all metrics
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
plot__conversion_rates_bayesian <- function(experiments_summary, experiment) {
    get_type <- function(type) {
        
        if(type == 'bayesian_prob_variant_gt_control') {
            return (type)
        }
        
        return (str_split(type, '_', simplify=TRUE)[2])
    }

    modified_summary <- experiments_summary %>%
        filter(experiment_id == experiment) %>%
        select(metric_id, 
               control_alpha, control_beta, bayesian_control_cr, 
               variant_alpha, variant_beta, bayesian_variant_cr,
               bayesian_prob_variant_gt_control) %>%
        gather(type, value, -metric_id) %>%
        mutate(variation=ifelse(type != 'bayesian_prob_variant_gt_control' & str_detect(type, 'control'), 'Control', 'Variant')) %>%
        mutate(actual_type=map_chr(type, ~ get_type(.))) %>%
        mutate(actual_type=ifelse(actual_type == 'control' | actual_type == 'variant',
                                  'conversion_rate',
                                  actual_type)) %>%
        select(-type) %>%
        spread(actual_type, value) %>%
        group_by(metric_id) %>%
        mutate(bayesian_prob_variant_gt_control=min(bayesian_prob_variant_gt_control, na.rm=TRUE),
               highest_conversion_rate = max(conversion_rate)) %>%
        ungroup()

    modified_summary %>%
        ggplot(aes(x=metric_id, y=conversion_rate, group=variation, fill=variation)) +
            #geom_col(position='dodge') +
            geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.75) +
            scale_y_continuous(labels=percent_format()) +
            coord_cartesian(ylim=c(0, max(modified_summary$conversion_rate) + 0.05)) +
            geom_text(aes(y=highest_conversion_rate, label= percent_format()(conversion_rate)),
                      position=position_dodge(width=.8), check_overlap=TRUE, vjust=-4, 
                      size=rel(global__text_size + 0.5)) + 
            geom_text(aes(label=paste('a:', comma_format()(alpha))),
                      position=position_dodge(width=.8), check_overlap=TRUE, vjust=-1.75,
                      size=rel(global__text_size)) + 
            geom_text(aes(label=paste('b:', comma_format()(beta))),
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
plot__percent_change_frequentist <- function(experiments_summary, experiment, p_value_threshold=0.05) {

    current_experiments <- experiments_summary %>%
        filter(experiment_id == experiment)
    
    nearest_x <- 0.05
    min_perc_change <- min(0,
                           ceiling_nearest_x(min(current_experiments$percent_change_from_control),
                                             nearest_x))
    max_perc_change <- ceiling_nearest_x(max(current_experiments$percent_change_from_control),
                                         nearest_x)
    
    plot_object <- current_experiments %>%
        ggplot(aes(x=metric_id, y=percent_change_from_control, fill=p_value <= p_value_threshold)) +
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
    if(all(current_experiments$p_value <= p_value_threshold)) {

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
        labs(caption=paste("\nThe p-value threshold for statistical significance is",
                           p_value_threshold),
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
        labs(#caption="\nThe decimal value next to the bar represents the Probability that the Variant is Better.",
             x="Metric",
             y="Percent Change from Control to Variant",
             fill="Probability Variant is Better")
}

#' Plots the percent change (with confidence intervals) from the Control to the Variant along with p-values
#' 
#' @param experiment_data list of data-frames from load_data
#' @param experiment
plot__percent_change_conf_frequentist <- function(experiments_summary, experiment, p_value_threshold=0.05) {

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
               p_value_sig = p_value <= p_value_threshold) %>%
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
        labs(caption=paste(paste("\nThe p-value threshold for statistical significance is",
                                  p_value_threshold),
                            "\nThe error bars show the", percent(1 - p_value_threshold), "confidence interval."),
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
        labs(caption=paste("\nThe error bars show the", percent(global__confidence_level), "confidence interval."),
             x="Metric",
             y="Percent Change from Control to Variant",
             color="Probability Variant is Better")
}

plot__daily_p_value <- function(experiments_daily_summary,
                                experiment,
                                metric,
                                p_value_threshold=0.05) {

    current_daily_summary <- experiments_daily_summary %>%
        filter(experiment_id == experiment,
               metric_id == metric)

    error_bar_height <- (max(current_daily_summary$p_value, na.rm=TRUE) - min(current_daily_summary$p_value, na.rm=TRUE)) / 25
    median_p_value <- median(current_daily_summary$p_value, na.rm = TRUE)
    missing_dates <- current_daily_summary %>%
        filter(is.na(control_conversion_rate)) %>%
        select(day_expired_attribution) %>%
        mutate(y_axis_location = p_value_threshold + error_bar_height) #median_p_value)

    missing_dates <- missing_dates %>%
        mutate(message = ifelse(as.character(missing_dates$day_expired_attribution) == as.character(min(missing_dates$day_expired_attribution)),
                              'Lag from\nAttribution\nWindow',
                              NA))

    current_daily_summary %>%
        mutate(day_expired_attribution = as.Date(day_expired_attribution)) %>%
        ggplot(aes(x=day_expired_attribution, y=p_value)) +
        geom_line(na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        geom_text(aes(label=round(p_value, 3)),
                  vjust=-0.5, check_overlap = TRUE, na.rm = TRUE, size=rel(global__text_size)) +
        geom_hline(yintercept = p_value_threshold, color ='red', alpha=0.5, size=1.5) +
        geom_errorbarh(data=missing_dates, aes(y = y_axis_location,
                                               xmin = min(day_expired_attribution),
                                               xmax = max(day_expired_attribution)),
                       color='#828282', height=rel(error_bar_height), size=rel(0.45)) +
        geom_text(data=missing_dates, aes(y=y_axis_location, label=message),
                  vjust=-0.5,
                  hjust='left',#0.325,
                  check_overlap = TRUE, na.rm=TRUE, size=rel(global__text_size)) +
        scale_x_date(date_breaks = '1 days') + 
        #scale_y_continuous(breaks = seq(0, 1, 0.05)) +
        expand_limits(y=0) +
        labs(title='P-value over time',
             y='P-Value',
             x='Day of Experiment (and days after)') +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

plot__daily_percent_change_frequentist <- function(experiments_daily_summary,
                                                   experiment,
                                                   metric,
                                                   p_value_threshold=0.05) {

    current_daily_summary <- experiments_daily_summary %>%
        filter(experiment_id == experiment,
               metric_id == metric) %>%
        mutate(day_expired_attribution = as.Date(day_expired_attribution)) %>%
        mutate(perc_change = frequentist_cr_difference / control_conversion_rate,
               perc_change_conf_low = frequentist_conf_low / control_conversion_rate,
               perc_change_conf_high = frequentist_conf_high / control_conversion_rate)
    
    error_bar_height <- (max(current_daily_summary$perc_change_conf_high, na.rm=TRUE) - min(current_daily_summary$perc_change_conf_low, na.rm=TRUE)) / 25
    median_percent_change <- median(current_daily_summary$perc_change, na.rm = TRUE)
    missing_dates <- current_daily_summary %>%
        filter(is.na(control_conversion_rate)) %>%
        select(day_expired_attribution) %>%
        mutate(y_axis_location = error_bar_height) #median_percent_change)

    missing_dates <- missing_dates %>%
        mutate(message = ifelse(as.character(missing_dates$day_expired_attribution) == as.character(min(missing_dates$day_expired_attribution)),
                              'Lag from\nAttribution\nWindow',
                              NA))

    plot_object <- current_daily_summary %>%
        ggplot(aes(x=day_expired_attribution, y=perc_change)) +
        geom_line(na.rm = TRUE) +
        #coord_cartesian(ylim=c(-0.10, 0.3)) +
        scale_y_continuous(labels = percent_format()) +
        scale_x_date(date_breaks = '1 days') + 
        geom_ribbon(aes(ymin = perc_change_conf_low, ymax = perc_change_conf_high), fill = 'green', alpha=0.15)

    if(any(current_daily_summary$p_value > p_value_threshold, na.rm=TRUE)) {
    
        plot_object <- plot_object +
            geom_ribbon(aes(ymin = ifelse(p_value > p_value_threshold, perc_change_conf_low, NA),
                            ymax = ifelse(p_value > p_value_threshold, perc_change_conf_high, NA)),
                        fill = 'red', alpha=0.45)
    }

    if(any(current_daily_summary$p_value <= p_value_threshold, na.rm=TRUE)) {
    
        plot_object <- plot_object +
            geom_ribbon(aes(ymin = ifelse(p_value <= p_value_threshold, perc_change_conf_low, NA),
                            ymax = ifelse(p_value <= p_value_threshold, perc_change_conf_high, NA)),
                        fill = 'green', alpha=0.2)

    }
    
    plot_object +
        geom_hline(yintercept = 0, color='red', alpha=0.5, size=1.5) +
        geom_text(aes(label=percent(perc_change)),
                  vjust=-1, check_overlap = TRUE, na.rm=TRUE, size=rel(global__text_size)) +
        geom_errorbarh(data=missing_dates, aes(y = y_axis_location,
                                               xmin = min(day_expired_attribution),
                                               xmax = max(day_expired_attribution)),
                       color='#828282', height=rel(error_bar_height), size=rel(0.45)) +
        geom_text(data=missing_dates, aes(y=y_axis_location, label=message),
                  vjust=-0.5,
                  hjust='left',#0.325,
                  check_overlap = TRUE, na.rm=TRUE, size=rel(global__text_size)) +
        labs(#title='Difference in Conversion Rate of `B` - `A`, with Frequentist Confidence Interval - \nWith Attribution Window',
             caption=paste("\n", percent(global__confidence_level), "confidence interval"),
             y='Lift (i.e. Percent change from Control to Variant)',
             x='Day of Experiment') +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

plot__daily_prob_variant_gt_control <- function(experiments_daily_summary, experiment, metric) {
    current_daily_summary <- experiments_daily_summary %>%
            filter(experiment_id == experiment,
                   metric_id == metric)

    error_bar_height <- 0.04  # we can hard code this value because the y-axis is always between 0-1
    median_probability <- median(current_daily_summary$bayesian_prob_variant_gt_control, na.rm = TRUE)
    missing_dates <- current_daily_summary %>%
        filter(is.na(control_conversion_rate)) %>%
        select(day_expired_attribution) %>%
        mutate(y_axis_location = 0.5 + error_bar_height) #median_probability)

    missing_dates <- missing_dates %>%
        mutate(message = ifelse(as.character(missing_dates$day_expired_attribution) == as.character(min(missing_dates$day_expired_attribution)),
                              'Lag from\nAttribution\nWindow',
                              NA))

    current_daily_summary %>%
            mutate(day_expired_attribution = as.Date(day_expired_attribution)) %>%
            ggplot(aes(x=day_expired_attribution, y=bayesian_prob_variant_gt_control)) +
            geom_line(na.rm=TRUE) +
            geom_point(na.rm=TRUE) +
            geom_text(aes(label=percent(bayesian_prob_variant_gt_control)),
                      vjust=-0.5, check_overlap = TRUE, na.rm=TRUE, size=rel(global__text_size)) +
            geom_hline(yintercept = 0.5, color ='red', alpha=0.5, size=1.5) +
            geom_errorbarh(data=missing_dates, aes(y = y_axis_location,
                                               xmin = min(day_expired_attribution),
                                               xmax = max(day_expired_attribution)),
                           color='#828282', height=rel(error_bar_height), size=rel(0.45)) +
            geom_text(data=missing_dates, aes(y=y_axis_location, label=message),
                      vjust=-0.5,
                      hjust='left',#0.325,
                      check_overlap = TRUE, na.rm=TRUE, size=rel(global__text_size)) +
            scale_x_date(date_breaks = '1 days') + 
            #scale_y_continuous(breaks = seq(0, 1, 0.05)) +
            expand_limits(y=c(0, 1)) +
            labs(title='Probability Variant is Better',
                 y='Probability Variant is Better',
                 x='Day of Experiment (and days after)') +
            theme_light(base_size=global__theme_base_size) +
            theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

plot__daily_percent_change_bayesian <- function(experiments_daily_summary, experiment, metric) {

    current_daily_summary <- experiments_daily_summary %>%
        filter(experiment_id == experiment,
               metric_id == metric) %>%
        mutate(day_expired_attribution = as.Date(day_expired_attribution)) %>%
        mutate(perc_change = bayesian_percent_change,
               perc_change_conf_low = bayesian_conf_low / bayesian_control_cr,
               perc_change_conf_high = bayesian_conf_high / bayesian_control_cr,
               # it is "statistically significant" if the confidence interval is completely above or below 0
               is_stat_sig=(perc_change_conf_low < 0 & perc_change_conf_high < 0) | (perc_change_conf_low > 0 & perc_change_conf_high > 0))

    error_bar_height <- (max(current_daily_summary$perc_change_conf_high, na.rm=TRUE) - min(current_daily_summary$perc_change_conf_low, na.rm=TRUE)) / 25
    median_percent_change <- median(current_daily_summary$perc_change, na.rm = TRUE)
    missing_dates <- current_daily_summary %>%
        filter(is.na(control_conversion_rate)) %>%
        select(day_expired_attribution) %>%
        mutate(y_axis_location = error_bar_height) #median_percent_change)

    missing_dates <- missing_dates %>%
        mutate(message = ifelse(as.character(missing_dates$day_expired_attribution) == as.character(min(missing_dates$day_expired_attribution)),
                              'Lag from\nAttribution\nWindow',
                              NA))

    plot_object <- current_daily_summary %>%
        ggplot(aes(x=day_expired_attribution, y=perc_change), na.rm=TRUE) +
        geom_line(na.rm=TRUE) +
        #coord_cartesian(ylim=c(-0.10, 0.3)) +
        scale_y_continuous(labels = percent_format()) +
        scale_x_date(date_breaks = '1 days') + 
        geom_ribbon(aes(ymin = perc_change_conf_low, ymax = perc_change_conf_high), fill = 'green', alpha=0.15)

    if(any(!current_daily_summary$is_stat_sig, na.rm=TRUE)) {
    
        plot_object <- plot_object +
            geom_ribbon(aes(ymin = ifelse(!is_stat_sig, perc_change_conf_low, NA), ymax = ifelse(!is_stat_sig, perc_change_conf_high, NA)), fill = 'red', alpha=0.45)
    }

    if(any(current_daily_summary$is_stat_sig, na.rm=TRUE)) {
    
        plot_object <- plot_object +
            geom_ribbon(aes(ymin = ifelse(is_stat_sig, perc_change_conf_low, NA), ymax = ifelse(is_stat_sig, perc_change_conf_high, NA)), fill = 'green', alpha=0.2)

    }

    plot_object +
        geom_hline(yintercept = 0, color='red', alpha=0.5, size=1.5) +
        geom_text(aes(label=percent(perc_change)),
                  vjust=-1, check_overlap = TRUE, na.rm=TRUE, size=rel(global__text_size)) +
        geom_errorbarh(data=missing_dates, aes(y = y_axis_location,
                                               xmin = min(day_expired_attribution),
                                               xmax = max(day_expired_attribution)),
                       color='#828282', height=rel(error_bar_height), size=rel(0.45)) +
        geom_text(data=missing_dates, aes(y=y_axis_location, label=message),
                  vjust=-0.5,
                  hjust='left',#0.325,
                  check_overlap = TRUE, na.rm=TRUE, size=rel(global__text_size)) +
        labs(#title='Difference in Conversion Rate of `B` - `A`, with Frequentist Confidence Interval - \nWith Attribution Window',
             caption=paste("\n", percent(global__confidence_level), "confidence interval"),
             y='Lift (i.e. Percent change from Control to Variant)',
             x='Day of Experiment') +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Returns Traffic Data Left Joined with Conversion Event Data, for a specific metric
create_traffic_convesions <- function(experiment_data,
                                      metric,
                                      cohort_format) {
    left_join(experiment_data$website_traffic %>%
                  group_by(user_id) %>%
                  summarise(first_visit = min(visit_date)) %>%
                  ungroup() %>%
                  mutate(cohort = create_cohort(first_visit, cohort_format)),
              experiment_data$conversion_events %>%
                  filter(metric_id == metric_name),
              by='user_id')
    
}

#' returns conversion rate data over time for the cohorts defined in traffic_conversions;
#' The user defines several snapshots (i.e. number of days after each person's first visit) to view the
#' overall conversion rates for each cohort
plot__conversion_rates_snapshot_absolute <- function(traffic_conversions,
                                                     snapshot_1_days,
                                                     snapshot_2_days,
                                                     snapshot_3_days,
                                                     cohort_label) {

    # per snapshot - need to filter out any cohort that has people in it where they haven't been given
    # enough time
    snapshot_1_day_threshold <- Sys.Date() - days(snapshot_1_days)
    snapshot_2_day_threshold <- Sys.Date() - days(snapshot_2_days)
    snapshot_3_day_threshold <- Sys.Date() - days(snapshot_3_days)
    
    color_label_lookup <- paste(c(snapshot_1_days, snapshot_2_days, snapshot_3_days), "Days")
    names(color_label_lookup) <- c("Snapshot 1", "Snapshot 2", "Snapshot 3")
    
    if(cohort_label == "Week") {

        cohort_format <- "%W"

    } else if (cohort_label == "Month") {

        cohort_format <- "%m"

    }else {

        stopifnot(FALSE)
    }

    temp <- traffic_conversions %>%
        mutate(days_to_convert=as.numeric(difftime(conversion_date, first_visit, units = 'days')),
               converted_1=!is.na(conversion_date) & conversion_date > first_visit & days_to_convert <= snapshot_1_days,
               converted_2=!is.na(conversion_date) & conversion_date > first_visit & days_to_convert <= snapshot_2_days,
               converted_3=!is.na(conversion_date) & conversion_date > first_visit & days_to_convert <= snapshot_3_days) %>%
        group_by(cohort) %>%
        summarise(max_first_visit=max(first_visit),
                  num_users=n_distinct(user_id),
                  snapshot_1_conversions=sum(converted_1),
                  snapshot_2_conversions=sum(converted_2),
                  snapshot_3_conversions=sum(converted_3)) %>%
        ungroup() %>%
        filter(# filter out current cohort
               cohort != create_cohort(Sys.Date(), cohort_format = cohort_format)
               ) %>%
        # filter out (i.e. set to NA) any cohort where the last person to visit the website in that cohort  hasn't had enough time to convert to the corresponding snapshot
        mutate(
            snapshot_1_conversions = ifelse(max_first_visit < snapshot_1_day_threshold, snapshot_1_conversions, NA),
            snapshot_2_conversions = ifelse(max_first_visit < snapshot_2_day_threshold, snapshot_2_conversions, NA),
            snapshot_3_conversions = ifelse(max_first_visit < snapshot_3_day_threshold, snapshot_3_conversions, NA)
        ) %>%
        mutate(`Snapshot 1` = snapshot_1_conversions / num_users,
               `Snapshot 2` = snapshot_2_conversions / num_users,
               `Snapshot 3` = snapshot_3_conversions / num_users) %>%
        select(cohort, contains('Snapshot ')) %>%
        gather(snapshot, conversion_rate, -cohort) %>%
        mutate(snapshot = factor(map_chr(snapshot, ~ color_label_lookup[.]), levels = color_label_lookup))

    temp %>%
    ggplot(aes(x=cohort, y=conversion_rate, group=snapshot, color=snapshot)) +
        geom_line(na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        geom_text(aes(label=percent(conversion_rate)),
                  vjust=-0.5, check_overlap = TRUE, na.rm = TRUE, size=rel(global__text_size)) +
        expand_limits(y=0) +
        scale_y_continuous(labels = percent_format()) +
        labs(#title='P-value over time',
             y='Conversion Rate',
             x=cohort_label,
             color='Snapshot') +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# like plot__conversion_rates_snapshot_absolute, but gives the percent of total conversions
# Generate new graph but instead of absolute conversion rate
# calcualte the % of conversions (have to actually have another setting, Max days Number of Days allowed to Convert)
plot__conversion_rates_snapshot_percent <- function(traffic_conversions,
                                                    snapshot_1_days,
                                                    snapshot_2_days,
                                                    snapshot_3_days,
                                                    snapshot_max_days,
                                                    cohort_label) {

    color_label_lookup <- paste(c(snapshot_1_days, snapshot_2_days, snapshot_3_days), "Days")
    names(color_label_lookup) <- c("Snapshot 1", "Snapshot 2", "Snapshot 3")

    snapshot_max_day_threshold <- Sys.Date() - days(snapshot_max_days)

    if(cohort_label == "Week") {

        cohort_format <- "%W"

    } else if (cohort_label == "Month") {

        cohort_format <- "%m"

    }else {

        stopifnot(FALSE)
    }

    # per snapshot - need to filter out any cohort that has people in it where they haven't been given
    # enough time
    snapshot_1_day_threshold <- Sys.Date() - days(snapshot_1_days)
    snapshot_2_day_threshold <- Sys.Date() - days(snapshot_2_days)
    snapshot_3_day_threshold <- Sys.Date() - days(snapshot_3_days)

    traffic_conversions %>%
        mutate(days_to_convert=as.numeric(difftime(conversion_date, first_visit, units = 'days')),
               converted_1=!is.na(conversion_date) & conversion_date > first_visit & days_to_convert <= snapshot_1_days,
               converted_2=!is.na(conversion_date) & conversion_date > first_visit & days_to_convert <= snapshot_2_days,
               converted_3=!is.na(conversion_date) & conversion_date > first_visit & days_to_convert <= snapshot_3_days,
               converted_max=!is.na(conversion_date) & conversion_date > first_visit & days_to_convert <= snapshot_max_days) %>%
        # this time, we only want to look at those who have converted within the max time allowed
        # and, we only want to keep the cohorts where the last person to join the cohort has had enough to convert >= snapshot_max_days
        filter(converted_max) %>%
        group_by(cohort) %>%
        summarise(max_first_visit=max(first_visit),
                  num_users_converted=n_distinct(user_id),
                  snapshot_1_conversions=sum(converted_1),
                  snapshot_2_conversions=sum(converted_2),
                  snapshot_3_conversions=sum(converted_3)) %>%
        ungroup() %>%
        filter(# filter out current cohort
            cohort != create_cohort(Sys.Date(), cohort_format = cohort_format),
            # we only want to keep the cohorts where the last person to join the cohort has had enough to convert >= snapshot_max_days
            max_first_visit < snapshot_max_day_threshold
        ) %>%
        # filter out (i.e. set to NA) any cohort where the last person to visit the website in that cohort  hasn't had enough time to convert to the corresponding snapshot
        mutate(
            snapshot_1_conversions = ifelse(max_first_visit < snapshot_1_day_threshold, snapshot_1_conversions, NA),
            snapshot_2_conversions = ifelse(max_first_visit < snapshot_2_day_threshold, snapshot_2_conversions, NA),
            snapshot_3_conversions = ifelse(max_first_visit < snapshot_3_day_threshold, snapshot_3_conversions, NA)
        ) %>%
        mutate(`Snapshot 1` = snapshot_1_conversions / num_users_converted,
               `Snapshot 2` = snapshot_2_conversions / num_users_converted,
               `Snapshot 3` = snapshot_3_conversions / num_users_converted) %>%
        
        select(cohort, contains('Snapshot ')) %>%
        gather(snapshot, conversion_rate, -cohort) %>%
        mutate(snapshot = factor(map_chr(snapshot, ~ color_label_lookup[.]), levels = color_label_lookup)) %>%
    ggplot(aes(x=cohort, y=conversion_rate, group=snapshot, color=snapshot)) +
        geom_line(na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        geom_text(aes(label=percent(conversion_rate)),
                  vjust=-0.5, check_overlap = TRUE, na.rm = TRUE, size=rel(global__text_size)) +
        expand_limits(y=0) +
        scale_y_continuous(labels = percent_format()) +
        labs(#title='P-value over time',
            y='Percent of Total Conversions',
            x=cohort_label,
            color='Snapshot') +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

plot__conversion_rates_historical <- function(experiment_data,
                                              exclude_last_n_days=30) {

    # Historical Conversion Rates
    traffic_conversions <- left_join(experiment_data$website_traffic %>%
                       group_by(user_id) %>%
                       summarise(first_visit = min(visit_date)) %>%
                       ungroup(),
                   experiment_data$conversion_events,
                   by='user_id') %>%
    filter(first_visit < Sys.Date() - days(exclude_last_n_days)) %>%
        select(user_id, metric_id)
    
    total_users <- length(unique(traffic_conversions$user_id))
    
    temp <- traffic_conversions %>%
        filter(!is.na(metric_id)) %>%
        group_by(metric_id) %>%
        summarise(conversion_rate=n_distinct(user_id) / total_users) %>%
        arrange(desc(conversion_rate))
    temp <- temp %>%
        mutate(metric_id = factor(metric_id, temp$metric_id))
    
    temp %>%
    ggplot(aes(x=metric_id, y=conversion_rate, fill=metric_id)) +
        geom_col() +
        scale_y_continuous(labels = percent_format()) +
        labs(#title='P-value over time',
            y='Historical Conversion Rate',
            x='Metric') +
        geom_text(aes(label=percent(conversion_rate)), vjust=-0.5) +
        scale_fill_manual(values=global__metric_colors) +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              legend.position='none')
}

#' gives historical conversion rates, but considering the *median* attribution rates for each metric over
#' all experiments
plot__conversion_rates_attribution <- function(experiment_data,
                                               exclude_last_n_days=30) {

    
    attr_windows <- experiment_data$attribution_windows %>%
        group_by(metric_id) %>%
        summarise(median_attr_window = median(attribution_window)) %>%
        ungroup()
    
    # Historical Conversion Rates
    traffic_conversions <- left_join(experiment_data$website_traffic %>%
                                        group_by(user_id) %>%
                                        summarise(first_visit = min(visit_date)) %>%
                                        ungroup(),
                                     experiment_data$conversion_events,
                                     by='user_id') %>%
        filter(first_visit < Sys.Date() - days(exclude_last_n_days)) %>%
        left_join(attr_windows, by='metric_id') %>%
        mutate(converted_within_window = first_visit + days(median_attr_window) < conversion_date) %>%
        mutate(converted_within_window = ifelse(is.na(converted_within_window), FALSE, converted_within_window))
    
    total_users <- length(unique(traffic_conversions$user_id))
    
    temp <- traffic_conversions %>%
        filter(!is.na(metric_id)) %>%
        group_by(metric_id) %>%
        summarise(conversion_rate=n_distinct(user_id) / total_users,
                  conversion_rate_within_window=sum(converted_within_window) / total_users,
                  percent_cr_window_realized=conversion_rate_within_window / conversion_rate) %>%
        arrange(desc(conversion_rate))
    temp <- temp %>%
        mutate(metric_id = factor(metric_id, temp$metric_id))
    
    temp %>%
    ggplot(aes(x=metric_id, y=conversion_rate_within_window, fill=metric_id)) +
        geom_col() +
        geom_text(aes(label=paste(percent(conversion_rate_within_window), "(within Attribution)")), vjust=-0.5) +
        geom_text(aes(label=paste(percent(percent_cr_window_realized), "of total")), vjust=1.5) +
        geom_col(aes(y=conversion_rate), fill='black', alpha=0.2) +
        geom_text(aes(y=conversion_rate, label=paste(percent(conversion_rate), "(historical)")), vjust=-0.5) +
        scale_y_continuous(labels = percent_format()) +
        labs(#title='P-value over time',
            y='Conversion Rates within Attribution Window',
            x='Metric') +
        scale_fill_manual(values=global__metric_colors) +
        theme_light(base_size=global__theme_base_size) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              legend.position='none')
}
