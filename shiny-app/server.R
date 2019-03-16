library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)

source('r_scripts/helpers_check_data_integrity.R', chdir=TRUE)
source('r_scripts/helpers_plots.R', chdir=TRUE)
source('r_scripts/helpers_processing.R', chdir=TRUE)
source('r_scripts/definitions.R')
source('r_scripts/helpers_logging.R', chdir=TRUE)

theme_set(theme_light())
options(scipen=999)

shinyServer(function(session, input, output) {

    ##########################################################################################################
    # LOAD DATA
    ##########################################################################################################
    reactive__experiment_data <- reactive({

        #withProgress(value=1/2, message="Loading Data", {
            
            log_message_block_start("Loading Data")
            load_data()
        #})
    })

    reactive__experiments_summary <- reactive({

        #withProgress(value=1/2, message="Loading Experiments Summary Data", {

            log_message_block_start("Loading Experiments Summary Data")
            readRDS('processed_data/experiments_summary.RDS')
        #})
    })

    reactive__experiments_daily_summary <- reactive({

        #withProgress(value=1/2, message="Loading Daily Summary Data", {

            log_message_block_start("Loading Daily Summary Data")
            readRDS('processed_data/experiments_daily_summary.RDS')
        #})
    })

    create_traffic_convesions <- function(experiment_data,
                                          metric,
                                          cohort_format) {
        left_join(experiment_data$website_traffic %>%
                      group_by(user_id) %>%
                      summarise(first_visit = min(visit_date)) %>%
                      ungroup() %>%
                      mutate(cohort = create_cohort(first_visit, cohort_format)),
                  experiment_data$conversion_events %>%
                      filter(metric_id == metric),
                  by='user_id')
        
    }
    reactive__traffic_conversions_metric <- reactive({

        req(reactive__experiment_data())
        req(input$conversion_rates__metric)
        req(input$conversion_rates__cohort_type)

        log_message_block_start("Building Traffic Conversions Metric")
        log_message_variable("conversion_rates__metric", input$conversion_rates__metric)
        log_message_variable("conversion_rates__cohort_type", input$conversion_rates__cohort_type)

        if(input$conversion_rates__cohort_type == "Week") {

            cohort_format <- "%W"

        } else if (input$conversion_rates__cohort_type == "Month") {

            cohort_format <- "%m"

        }else {

            stopifnot(FALSE)
        }

        log_message_variable("cohort_format", cohort_format)

        #withProgress(value=1/2, message="Loading Daily Summary Data", {
        
        create_traffic_convesions(experiment_data=reactive__experiment_data(),
                                  metric=input$conversion_rates__metric,
                                  cohort_format=cohort_format)
        #})
    })

    ##########################################################################################################
    # UI
    ##########################################################################################################
    output$experiment__select__UI <- renderUI({

        req(reactive__experiments_summary())

        experiments <- reactive__experiments_summary() %>%
            arrange(-as.numeric(end_date), -as.numeric(start_date)) %>%
            get_vector('experiment_id', return_unique=TRUE)

        selectInput(inputId='experiment__select',
                    label="Choose Experiment",
                    choices=experiments,
                    selected=experiments[1],
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })

    ##########################################################################################################
    # DYNAMIC GRAPH OPTIONS
    ##########################################################################################################
    output$graph_options__percent_change__UI <- renderUI({
        NULL
    })
    output$graph_options__percent_change_conidence__UI <- renderUI({
        NULL
    })
    output$graph_options__conversion_rates__UI <- renderUI({
        NULL
    })

    #' defualt placement is 'bottom', but I want the default to be 'top'
    add_tooltip <- function(element, tooltip_text, placement='top', trigger='hover') {
        return ( tipify(element, title=tooltip_text, placement=placement, trigger=trigger) )
    }

    ui_list_append <- function(l, ui_item, div_class=NULL) {

        if(is.null(div_class)) {

            return (c(l, list(ui_item)))

        } else {

            return (c(l, list(tags$div(class=div_class, ui_item))))
        }
    }

    output$graph_options__trends__UI <- renderUI({

        req(reactive__experiments_summary())
        req(input$experiment__select)

        #withProgress(value=1/2, message="Generating Graph Options", {

            log_message_variable('experiment__select', input$experiment__select)

            ui_graph_type <- radioButtons(
                inputId='experiment__trend_graph_type',
                label="Trend Type",
                choices=c("Percent Change", "Statistic"),
                selected="Percent Change",
                inline=TRUE,
                width=NULL, choiceNames=NULL,
                choiceValues=NULL) %>%
            #For the Frequentist graph, the 'Statistic' options shows the p-value over time.\n\nFor the Bayesian graph, the 'Statistic' options shows the `probability that the Variant is better than the Control`, over time.
            add_tooltip("For the Frequentist graph, the `Statistic` option shows the p-value over time. For the Bayesian graph, the `Statistic` option shows the `probability that the Variant is better than the Control`, over time.")

            metrics <- reactive__experiments_summary() %>%
                filter(experiment_id == input$experiment__select) %>%
                arrange(desc(control_conversion_rate)) %>%
                get_vector('metric_id', return_unique=TRUE)

            log_message_variable('metrics', paste(metrics, collapse='; '))

            ui_metric <- selectInput(inputId='experiment__trends__metric_select',
                    label="Choose Metric",
                    choices=metrics,
                    selected=metrics[1],
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL) %>%
            add_tooltip("Choose the metric to analyze.")


            ui_list <- list()
            ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_graph_type)
            ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_metric)

            return (tagList(list=ui_list))
        #})
    })

    output$graph_options__bayesian_posteriors__UI <- renderUI({

        log_message_block_start("Creating Bayesian Posterior Graph")
        req(reactive__experiments_summary())
        req(input$experiment__select)

        #withProgress(value=1/2, message="Generating Graph Options", {

            log_message_variable('experiment__select', input$experiment__select)

            ui_show_prior <- checkboxInput(
                inputId='experiment__bayesian_posterior__show_prior',
                label="Show Prior Distribution",
                value=TRUE)
            # ui_show_prior <- radioButtons(
            #         inputId='experiment__bayesian_posterior__show_prior',
            #         label="Show Prior Distribution",
            #         choices=c("Yes", "No"),
            #         selected="Yes",
            #         inline=TRUE,
            #         width=NULL, choiceNames=NULL,
            #         choiceValues=NULL)

            metrics <- reactive__experiments_summary() %>%
                filter(experiment_id == input$experiment__select) %>%
                arrange(desc(control_conversion_rate)) %>%
                get_vector('metric_id', return_unique=TRUE)

            log_message_variable('metrics', paste(metrics, collapse='; '))

            ui_metric <- selectInput(inputId='experiment__bayesian_posterior__metric_select',
                    label="Choose Metric",
                    choices=metrics,
                    selected=metrics[1],
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL) %>%
            add_tooltip("Choose the metric to analyze.")


            ui_list <- list()
            ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_show_prior)
            ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_metric)

            return (tagList(list=ui_list))
        #})
    })

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

    output$graph_options__raw_data__UI <- renderUI({
        selected <- c(
            "Frequentist",
            "Bayesian"
        )
        ui_show_in_table <- checkboxGroupInput(
            inputId='experiment__raw_data__show_options',
            label='Show in Table',
            choices = graph_options__raw_data__choices, 
            selected = selected,
            inline = FALSE,
            width = NULL, 
            choiceNames = NULL,
            choiceValues = NULL)

        ui_list <- list()
        ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_show_in_table)
        return (tagList(list=ui_list))
    })

    output$conversion_rates__metric__UI <- renderUI({

        req(reactive__experiments_summary())

        metric_names <- reactive__experiments_summary() %>%
            get_vector('metric_id', return_unique = TRUE) %>%
            as.character()

        ui_metric <- selectInput(
            inputId='conversion_rates__metric',
            label="Metric",
            choices=metric_names,
            selected=metric_names[1])

        ui_list <- list()
        ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_metric)
        return (tagList(list=ui_list))
    })

    output$duration_calculator__url__UI <- renderUI({
    
        req(reactive__experiment_data())

        paths <- reactive__experiment_data()$website_traffic %>%
            count(path) %>%
            arrange(desc(n)) %>%
            get_vector('path')

        ui_paths <- selectInput(
            inputId='duration_calculator__url',
            label="Path",
            choices=c("<Select Path>", paths),
            selected="<Select Path>")

        ui_list <- list()
        ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_paths)
        return (tagList(list=ui_list))
    })

    output$duration_calculator__metrics__UI <- renderUI({

        req(reactive__experiments_summary())

        metric_names <- reactive__experiments_summary() %>%
            get_vector('metric_id', return_unique = TRUE) %>%
            as.character()

        ui_metric <- selectInput(
            inputId='duration_calculator__metrics',
            label="Metrics",
            choices=metric_names,
            selected=metric_names,
            multiple=TRUE)

        ui_list <- list()
        ui_list <- ui_list_append(ui_list, div_class='dynamic_filter', ui_metric)
        return (tagList(list=ui_list))
    })


    ##########################################################################################################
    # Update Dynamic Graph Options based on the Selected Tab
    ##########################################################################################################
    observeEvent(input$experiment_tabs, {

        req(input$experiment_tabs)
        log_message_variable('Selected New Tab', input$experiment_tabs)

        if(input$experiment_tabs == global__experiment__tab_names__percent_change) {

            shinyjs::show('experiment__stat_type_select')

            #updateCollapse(session, 'main__bscollapse', close="Graph Options")
            shinyjs::show('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_conidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            
        } else if(input$experiment_tabs == global__experiment__tab_names__percent_change_conf) {

            shinyjs::show('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::show('graph_options__percent_change_conidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")

        } else if(input$experiment_tabs == global__experiment__tab_names__conversion_rates) {
            
            shinyjs::show('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_conidence__UI')
            shinyjs::show('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")

        } else if(input$experiment_tabs == global__experiment__tab_names__trends) {
            
            shinyjs::show('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_conidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::show('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")

        } else if(input$experiment_tabs == global__experiment__tab_names__bayesian) {
            
            shinyjs::hide('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_conidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::show('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")
            
        } else if(input$experiment_tabs == global__experiment__tab_names__raw_data) {
            
            shinyjs::hide('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_conidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::show('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")

        } else {

            stopifnot(FALSE)
        }
    })

    observe({

        req(input$conversion_rates__graph_type)

        log_message_block_start('Hide/Show Conversion Rate Options')
        log_message_variable('conversion_rates__graph_type', input$conversion_rates__graph_type)

        if(input$conversion_rates__graph_type == "Cohort") {

            shinyjs::show('conversion_rates__metric__UI')
            shinyjs::show('conversion_rates__metric')
            shinyjs::hide('conversion_rates__cohort_type')
            shinyjs::show('conversion_rates__cr_type')
            shinyjs::show('conversion_rates__snapshot_1_days')
            shinyjs::show('conversion_rates__snapshot_2_days')
            shinyjs::show('conversion_rates__snapshot_3_days')

            if(input$conversion_rates__cr_type == "Absolute") {

                shinyjs::hide('conversion_rates__max_days_to_convert')
            
            } else {

                shinyjs::show('conversion_rates__max_days_to_convert')    
            }

            shinyjs::hide('conversion_rates__exclude_last_n_days')

        } else if(input$conversion_rates__graph_type == "Historical") {

            shinyjs::hide('conversion_rates__metric')
            shinyjs::hide('conversion_rates__cohort_type')
            shinyjs::hide('conversion_rates__cr_type')
            shinyjs::hide('conversion_rates__snapshot_1_days')
            shinyjs::hide('conversion_rates__snapshot_2_days')
            shinyjs::hide('conversion_rates__snapshot_3_days')
            shinyjs::hide('conversion_rates__max_days_to_convert')
            shinyjs::show('conversion_rates__exclude_last_n_days')

        } else if(input$conversion_rates__graph_type == "Attribution") {

            shinyjs::hide('conversion_rates__metric')
            shinyjs::hide('conversion_rates__cohort_type')
            shinyjs::hide('conversion_rates__cr_type')
            shinyjs::hide('conversion_rates__snapshot_1_days')
            shinyjs::hide('conversion_rates__snapshot_2_days')
            shinyjs::hide('conversion_rates__snapshot_3_days')
            shinyjs::hide('conversion_rates__max_days_to_convert')
            shinyjs::show('conversion_rates__exclude_last_n_days')

        } else {

            stopifnot(FALSE)
        }
    })

    ##########################################################################################################
    # PLOTS & TABLES
    ##########################################################################################################
    output$plot__percent_change <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__stat_type_select)

        #withProgress(value=1/2, message="Creating Percent Change Graph", {

            log_message_block_start("Creating Percent Change Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

            if(input$experiment__stat_type_select == "Frequentist") {

                plot__percent_change_frequentist(reactive__experiments_summary(),
                                                 input$experiment__select,
                                                 p_value_threshold=global__p_value_threshold)
            } else {

                plot__percent_change_bayesian(reactive__experiments_summary(), input$experiment__select)
            }
        #})
    }, height=function() {

        session$clientData$output_plot__percent_change_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__percent_change', 
        title="Percent Change",
        content=HTML("Shows the percent change from the Control to the Variant.<br><br>The Frequentist graph gives the p-value, while the Bayesian graph gives the probability that the Variant's conversion rate is higher than the Control's conversion rate.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates."),
        placement="left", trigger="hover", options=NULL)

    output$plot__percent_change_confidence <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__stat_type_select)

        #withProgress(value=1/2, message="Creating Percent Change Conf. Graph", {

            log_message_block_start("Creating Percent Change Conf. Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

            if(input$experiment__stat_type_select == "Frequentist") {

                plot__percent_change_conf_frequentist(reactive__experiments_summary(),
                                                      input$experiment__select,
                                                      p_value_threshold=global__p_value_threshold)
            } else {

                plot__percent_change_conf_bayesian(reactive__experiments_summary(), input$experiment__select)
            }
        #})
    }, height=function() {

        session$clientData$output_plot__percent_change_confidence_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__percent_change_confidence', 
        title="Percent Change Confidence Level",
        content=HTML("Shows the percent change between the Control and Variant, with confidence intervals.<br><br>The frequentist graph shows the p-value at the top of the graph, while the Bayesian graph gives the probability that the Variant's conversion rate is higher than the Control's conversion rate.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates."),
        placement="left", trigger="hover", options=NULL)

    output$plot__conversion_rates <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__stat_type_select)

        #withProgress(value=1/2, message="Creating Conversion Rates Graph", {

            log_message_block_start("Creating Conversion Rates Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

            if(input$experiment__stat_type_select == "Frequentist") {

                plot__conversion_rates(reactive__experiments_summary(), input$experiment__select)
            } else {

                plot__conversion_rates_bayesian(reactive__experiments_summary(), input$experiment__select)
            }
        #})
    }, height=function() {

        session$clientData$output_plot__conversion_rates_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__conversion_rates', 
        title="Conversion Rates",
        content=HTML("Shows the conversion rates of the Control and Variant groups.<br><br>The frequentist graph shows the conversions and trials above the bar, while the bayesian graph shows the alpha and beta values above the bar.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates."),
        placement="left", trigger="hover", options=NULL)

    output$plot__trends <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__trends__metric_select)
        req(input$experiment__stat_type_select)
        req(input$experiment__trend_graph_type)

        #withProgress(value=1/2, message="Creating Trends Graph", {

            log_message_block_start("Creating Trends Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__trends__metric_select', input$experiment__trends__metric_select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)
            log_message_variable('experiment__trend_graph_type', input$experiment__trend_graph_type)

            if(input$experiment__stat_type_select == "Frequentist") {

                if(input$experiment__trend_graph_type == "Percent Change") {
                    
                    plot__daily_percent_change_frequentist(reactive__experiments_daily_summary(),
                                                           input$experiment__select,
                                                           input$experiment__trends__metric_select,
                                                           p_value_threshold=global__p_value_threshold)
                } else {

                    plot__daily_p_value(reactive__experiments_daily_summary(),
                                        input$experiment__select,
                                        input$experiment__trends__metric_select,
                                        p_value_threshold=global__p_value_threshold)
                }

            } else {

                if(input$experiment__trend_graph_type == "Percent Change") {
                    plot__daily_percent_change_bayesian(reactive__experiments_daily_summary(),
                                                        input$experiment__select,
                                                        input$experiment__trends__metric_select)
                } else {

                    plot__daily_prob_variant_gt_control(reactive__experiments_daily_summary(),
                                                        input$experiment__select,
                                                        input$experiment__trends__metric_select)
                }
            }
        #})
    }, height=function() {

        session$clientData$output_plot__trends_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__trends', 
        title="Percent Change Over time",
        content=HTML("Shows the percent change (i.e. Lift) from the Control to the Variant over the lifetime of the experiment.<br><br>There will be a lag between the start of the experiment and when data will start appearing, due to the attribution window.<br><br>The colored area is the confidence interval where red indicates that '0% Lift' is within the interval, and green indicates '0% Lift' is outside the interval.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates."),
        placement="left", trigger="hover", options=NULL)



    output$plot__bayesian_posteriors <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__bayesian_posterior__metric_select)

        #withProgress(value=1/2, message="Creating Bayesian Posterior Graph", {

            log_message_block_start("Creating Bayesian Posterior Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__bayesian_posterior__metric_select', input$experiment__bayesian_posterior__metric_select)
            log_message_variable('experiment__bayesian_posterior__show_prior', input$experiment__bayesian_posterior__show_prior)

            plot__bayesian_posterior(reactive__experiments_summary(),
                                     input$experiment__select,
                                     input$experiment__bayesian_posterior__metric_select,
                                     confidence_level=global__confidence_level,
                                     show_prior_distribution=input$experiment__bayesian_posterior__show_prior)
        #})
    }, height=function() {

        session$clientData$output_plot__bayesian_posteriors_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__bayesian_posteriors', 
        title="Posterior Probability Distributions of the Control & Variant",
        content=HTML("This graph shows the posterior probability distributions of the Control, Variant, and if selected, the Priors. It also shows a confidence intervals for the Control and Variant distributions below."),
        placement="left", trigger="hover", options=NULL)

    output$plot__website_traffic <- renderPlot({

        req(reactive__experiment_data())

        top_n_paths <- NULL

        if(input$website_traffic__show_path_breakdown) {
            top_n_paths <- input$website_traffic__top_n_paths
        }

        plot__website_traffic(
            experiment_data=reactive__experiment_data(),
            only_first_time_visits=input$website_traffic__show_first_time_visits,
            is_weekly=input$website_traffic__cohort_type == "Week",
            filter_year_end_beginning_weeks=input$website_traffic__cohort_type == "Week",
            top_n_paths=top_n_paths)

    }, height=function() {

        session$clientData$output_plot__website_traffic_width * 0.65  # set height to % of width
    })

    output$conversion_rates__plot <- renderPlot({

        log_message_block_start("Building Conversion Rate Plots2")
        log_message_variable('conversion_rates__metric', input$conversion_rates__metric)
        log_message_variable('conversion_rates__graph_type', input$conversion_rates__graph_type)
        log_message_variable('conversion_rates__cr_type', input$conversion_rates__cr_type)

        req(input$conversion_rates__graph_type)
        # req(input$conversion_rates__metric)
        # #req(reactive__traffic_conversions_metric())
        # req(input$conversion_rates__cr_type)

        # req(input$conversion_rates__cohort_type)
        # req(input$conversion_rates__snapshot_1_days)
        # req(input$conversion_rates__snapshot_2_days)
        # req(input$conversion_rates__snapshot_3_days)
        # req(input$conversion_rates__max_days_to_convert)

        log_message_block_start("Building Conversion Rate Plots")
        log_message_variable('conversion_rates__metric', input$conversion_rates__metric)

        snapshot_1_days <- input$conversion_rates__snapshot_1_days
        snapshot_2_days <- input$conversion_rates__snapshot_2_days
        snapshot_3_days <- input$conversion_rates__snapshot_3_days

        if(input$conversion_rates__graph_type == "Cohort") {
            
            req(input$conversion_rates__metric)

            if(input$conversion_rates__cr_type == "Absolute") {

                return (plot__conversion_rates_snapshot_absolute(traffic_conversions=reactive__traffic_conversions_metric(),
                                                         snapshot_1_days,
                                                         snapshot_2_days,
                                                         snapshot_3_days,
                                                         cohort_label=input$conversion_rates__cohort_type))


            } else {
                plot__conversion_rates_snapshot_percent(traffic_conversions=reactive__traffic_conversions_metric(),
                                                        snapshot_1_days,
                                                        snapshot_2_days,
                                                        snapshot_3_days,
                                                        snapshot_max_days=input$conversion_rates__max_days_to_convert,
                                                        cohort_label=input$conversion_rates__cohort_type)
            }


        } else if(input$conversion_rates__graph_type == "Historical") { 

            plot__conversion_rates_historical(experiment_data=reactive__experiment_data(),
                                              exclude_last_n_days=input$conversion_rates__exclude_last_n_days)

        } else if(input$conversion_rates__graph_type == "Attribution") {

            plot__conversion_rates_attribution(experiment_data=reactive__experiment_data(),
                                               exclude_last_n_days=input$conversion_rates__exclude_last_n_days) 

        } else {

            stopifnot(FALSE)
        }

    }, height=function() {

        session$clientData$output_conversion_rates__plot_width * 0.65  # set height to % of width
    })


    output$experiment__raw_data_table <- renderDataTable({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__raw_data__show_options)

        log_message_block_start("Showing Raw Data of Experiment")

        log_message_variable('experiment__raw_data__show_options',
            paste(input$experiment__raw_data__show_options, collapse=', '))

        summary_columns <- list(
            #'experiment_id',
            `Date Info`=c(
                'start_date',
                'end_date',
                'last_join_date'
            ),
            'metric_id',
            `Variation Names`=c(
                'control_name',
                'variant_name'
            ),
            'control_conversion_rate',
            'variant_conversion_rate',
            'percent_change_from_control',
            `Raw Counts`=c(
                'control_successes',
                'control_trials',
                'variant_successes',
                'variant_trials'
            ),
            `Frequentist`=c(
                'p_value',
                'frequentist_cr_difference'
            ),
            `Frequentist Conf. Int.`=c(
                'frequentist_conf_low',
                'frequentist_conf_high'
            ),
            `Bayesian`=c(
                'bayesian_control_cr',
                'bayesian_variant_cr',
                'bayesian_prob_variant_gt_control'
            ),
            `Bayesian Conf. Int.`=c(
                'bayesian_cr_difference',
                'bayesian_conf_low',
                'bayesian_conf_high'
            ),
            `Bayesian Alpha/Beta`=c(
                'prior_alpha',
                'prior_beta',
                'control_alpha',
                'control_beta',
                'variant_alpha',
                'variant_beta'
            )
        )

        # Lets remove the items that are in graph_options__raw_data__choices but were not selected
        items_to_remove <- graph_options__raw_data__choices[!graph_options__raw_data__choices %in% input$experiment__raw_data__show_options]
        remaining_columns <- summary_columns
        for(item_to_remove in items_to_remove) {

            remaining_columns[[item_to_remove]] <- NULL 
        }

        return (
            reactive__experiments_summary() %>%
                filter(experiment_id == input$experiment__select) %>%
                select_(.dots=unlist(remaining_columns, use.names = FALSE))
        )
    })

    output$more__settings__table <- renderDataTable({

        return (
            tribble(
                ~Name, ~Value,
                #--|--
                "Confidence Level", global__confidence_level,
                "P-Value Threshold", global__p_value_threshold,
                "Days of Prior Information", global__prior_number_of_days
            )
        )
    },
    options = list(paging = FALSE, info = TRUE)
    )

    output$raw_data__table <- renderDataTable({

        req(input$raw_data__select_dataset)

        df <- NULL

        if(input$raw_data__select_dataset == "Attribution Windows") {

            df <- reactive__experiment_data()$attribution_windows

        } else if(input$raw_data__select_dataset == "Experiment Info") {

            df <- reactive__experiment_data()$experiment_info

        } else if(input$raw_data__select_dataset == "Experiment Traffic") {

            df <- reactive__experiment_data()$experiment_traffic

        } else if(input$raw_data__select_dataset == "Website Traffic") {

            df <- reactive__experiment_data()$website_traffic

        } else if(input$raw_data__select_dataset == "Conversion Events") {

            df <- reactive__experiment_data()$conversion_events

        } else if(input$raw_data__select_dataset == "Experiments Summary") {

            df <- reactive__experiments_summary()

        } else if(input$raw_data__select_dataset == "Daily Summary") {

            df <- reactive__experiments_daily_summary()

        } else {

            stopifnot(FALSE)
        }

        return (df %>% head(1000))
    })
})
