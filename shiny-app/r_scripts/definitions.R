should_log_message <- TRUE
##############################################################################################################
# File Paths
##############################################################################################################
data_path__experiment_info <- 'simulated_data/experiment_info.csv'
data_path__experiment_traffic <- 'simulated_data/experiment_traffic.csv'
data_path__attribution_windows <- 'simulated_data/attribution_windows.csv'
data_path__website_traffic <- 'simulated_data/website_traffic.csv'
data_path__conversion_events <- 'simulated_data/conversion_events.csv'

global__colors_variant <- '#19A6FF'  # blue
global__colors_control <- '#FF9500'  # orange
global__colors_prior <- '#C396E8'  # purple
global__colors_bayesian <-  c(global__colors_variant, global__colors_control, global__colors_prior)

global__colors_bad <- '#E15759'  # red
global__colors_good <- '#37B57F'  # green

# color_event_type_join <- '#EBB13E'

global__metric_colors <- c('#40C6EE', '#EBB13E', '#2AADB7', '#F28E2B', '#C396E8')
# color_event_type_initial <- '#40C6EE'
# color_event_type_click <- '#11499C'
# color_event_type_backclick <- '#7A7A7A'
# color_event_type_link <- '#FC641F'
# color_event_type_generic <- '#C396E8'
# color_event_type_final <- '#5FECA6'
# color_event_type_success <- color_good

global__text_size <- 4
global__theme_base_size <- 16

global__plot_height_percent_of_width <- 0.55

global__experiment__tab_names__percent_change <- "Percent Change"
global__experiment__tab_names__percent_change_conf <- "Percent Change Confidence"
global__experiment__tab_names__conversion_rates <- "Conversion Rates"
global__experiment__tab_names__trends <- "Trends"
global__experiment__tab_names__bayesian <- "Bayesian Posteriors"
global__experiment__tab_names__raw_data <- "Raw Data"

#global__select_path <- "<Select Path>"

graph_options__raw_data__choices <- c(
    "Date Info",
    "Variation Names",
    "Raw Counts",
    "Frequentist",
    "Frequentist Conf. Int.",
    "Bayesian",
    "Bayesian Conf. Int.",
    "Bayesian Alpha/Beta"
)

##############################################################################################################
# Statistical Variables
##############################################################################################################
global__confidence_level <- 0.95
global__p_value_threshold <- 1 - global__confidence_level

global__prior_number_of_days <- 5
