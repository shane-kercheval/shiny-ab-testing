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

        withProgress(value=1/2, message="Loading Experiment Data", {
            
            log_message_block_start("Loading Experiment Data")
            load_data()
        })
    })

    reactive__experiments_summary <- reactive({

        log_message_block_start("Loading Experiments Summary Data")
        readRDS('processed_data/experiments_summary.RDS')
    })

    reactive__experiments_daily_summary <- reactive({

        log_message_block_start("Loading Daily Summary Data")
        readRDS('processed_data/experiments_daily_summary.RDS')
    })

    reactive__latest_traffic_datetime <- reactive({

        log_message_block_start("Loading Current Date/Time")
        readRDS('processed_data/latest_website_traffic_datetime.RDS')
    })

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

        get__cohorted_traffic_conversions(experiment_data=reactive__experiment_data(),
                                          metric=input$conversion_rates__metric,
                                          cohort_format=cohort_format)
    })

    reactive__historical_conversion_rates <- reactive({

        log_message_block_start("Loading Historical Conversion Rate Data")
        readRDS('processed_data/historical_conversion_rates.RDS')
    })

    reactive__ab_test_calculator_results <- reactive({

        req(input$duration_calculator__url)
        req(input$duration_calculator__mde)
        req(input$duration_calculator__alpha)
        req(input$duration_calculator__beta)

        withProgress(value=1/2, message="Calculating AB Duration & Sample Size", {

            log_message_variable("duration_calculator__url", input$duration_calculator__url)

            minimum_detectable_effect <- input$duration_calculator__mde / 100
            alpha <- input$duration_calculator__alpha / 100
            power <- 1 - (input$duration_calculator__beta / 100)

            # do the calculation for all metrics, then the renderTable can filter as needed
            metric_names <- reactive__experiments_summary() %>%
                get_vector('metric_id', return_unique = TRUE) %>%
                as.character()

            log_message_variable("metric_names", metric_names)
            log_message_variable("minimum_detectable_effect", minimum_detectable_effect)
            log_message_variable("alpha", alpha)
            log_message_variable("power", power)

            calc_results <- site__ab_test_calculator(reactive__experiment_data(),
                                                     reactive__historical_conversion_rates(),
                                                     experiment_path=input$duration_calculator__url,
                                                     metrics=metric_names,
                                                     minimum_detectable_effect=minimum_detectable_effect,
                                                     alpha=alpha,
                                                     power=power,
                                                     simulated_experiment_length = 30)
        })
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

    output$experiment_info__start_date__UI <- renderText({

        req(reactive__experiments_summary())
        req(input$experiment__select)

        reactive__experiments_summary() %>%
            filter(experiment_id == input$experiment__select) %>%
            distinct() %>%
            get_vector('start_date', return_unique = TRUE) %>%
            as.Date() %>%
            as.character()
    })

    output$experiment_info__end_date__UI <- renderText({

        req(reactive__experiments_summary())
        req(input$experiment__select)

        end_date <- reactive__experiments_summary() %>%
            filter(experiment_id == input$experiment__select) %>%
            distinct() %>%
            get_vector('end_date', return_unique = TRUE) %>%
            as.Date()

        # still running: end-date within a day of "current" date
        if(end_date >= reactive__latest_traffic_datetime() - days(1)) {

            return ("<font color=\"#FF0000\"><b>Running...</b></font>")

        } else {

            return (end_date %>% as.character())
        }
    })

    output$experiment_info__data_collection_metrics__UI <- renderText({

        req(input$experiment__select)

        metric_names <- reactive__experiments_summary() %>%
        filter(experiment_id == input$experiment__select) %>%
            get_vector('metric_id', return_unique = TRUE) %>%
            as.character()


        return(paste0(paste0(metric_names, collapse=':<br>'), ':'))
    })

    output$experiment_info__data_collection_lags__UI <- renderText({

        req(input$experiment__select)

        lag_info <- reactive__experiments_summary() %>%
            group_by(experiment_id, metric_id) %>%
            summarise(collection_lag = as.numeric(difftime(end_date, last_join_date, units='days'))) %>%
            ungroup() %>%
            mutate(has_lag=ifelse(collection_lag > 0, TRUE, FALSE),
                   label=ifelse(collection_lag > 0, paste(round(collection_lag), "day lag"), "All data included."),
                   label=ifelse(label == "0 day lag", "<1 day lag", label)) %>%
            mutate(label=ifelse(has_lag, paste0("<font color=\"#FF0000\"><b>", label, "</b></font>"), label)) %>%
            filter(experiment_id == input$experiment__select)

        return (paste0("", paste0(lag_info$label, collapse = "<br>")))
    })

    ##########################################################################################################
    # DYNAMIC GRAPH OPTIONS
    ##########################################################################################################
    output$graph_options__percent_change__UI <- renderUI({
        NULL
    })
    output$graph_options__percent_change_confidence__UI <- renderUI({
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
    })

    output$graph_options__bayesian_posteriors__UI <- renderUI({

        req(reactive__experiments_summary())
        req(input$experiment__select)

        log_message_block_start("Creating Bayesian Posterior Graph Options")
        log_message_variable('experiment__select', input$experiment__select)

        ui_show_prior <- checkboxInput(
            inputId='experiment__bayesian_posterior__show_prior',
            label="Show Prior Distribution",
            value=TRUE)

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
            choices=paths,  #c(global__select_path, paths),
            selected=paths[1])  #global__select_path)

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


    insert_custom_progress_bar <- function(message="Loading/Processing Data") {
        # this function is a hack so that the progress bar works for ggplots that take a long time to render
        # because ggplot objects don't attempt to render under they are displayed
        global__progress_bar_html <- HTML(paste0('"<div id="shiny-notification-panel"><div id="shiny-notification-42ec5661c2722d29" class="shiny-notification"><div class="shiny-notification-close">×</div><div class="shiny-notification-content"><div class="shiny-notification-content-text"><div id="shiny-progress-42ec5661c2722d29" class="shiny-progress-notification"><div class="progress progress-striped active" style=""><div class="progress-bar" style="width: 50%;"></div></div><div class="progress-text"><span class="progress-message">', message,'</span> <span class="progress-detail"></span></div></div></div><div class="shiny-notification-content-action"></div></div></div></div>"'))
        progress_html <- conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                          global__progress_bar_html,
                                          id='custom_progress_bar')
        remove_custom_progress_bar()
        insertUI(selector="#custom_progress_bar_placeholder", where = c("afterEnd"), progress_html, immediate=TRUE)
    }

    remove_custom_progress_bar <- function() {

        # remove the custom progress bar that works with long ggplot 
        removeUI(selector="#custom_progress_bar", immediate = TRUE)
    }

    ##########################################################################################################
    # Update Dynamic Graph Options based on the Selected Tab
    ##########################################################################################################
    observeEvent(input$nav_bar_page, {

        # remove the custom progress bar when navigating to a different tab
        remove_custom_progress_bar()
    })

    observeEvent(input$experiment_tabs, {

        req(input$experiment_tabs)
        log_message_variable('Selected New Tab', input$experiment_tabs)

        # remove the custom progress bar when navigating to a different tab
        remove_custom_progress_bar()

        if(input$experiment_tabs == global__experiment__tab_names__percent_change) {

            shinyjs::show('experiment__stat_type_select')

            #updateCollapse(session, 'main__bscollapse', close="Graph Options")
            shinyjs::show('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_confidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            
        } else if(input$experiment_tabs == global__experiment__tab_names__percent_change_conf) {

            shinyjs::show('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::show('graph_options__percent_change_confidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")

        } else if(input$experiment_tabs == global__experiment__tab_names__conversion_rates) {
            
            shinyjs::show('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_confidence__UI')
            shinyjs::show('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")

        } else if(input$experiment_tabs == global__experiment__tab_names__trends) {
            
            shinyjs::show('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_confidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::show('graph_options__trends__UI')
            shinyjs::hide('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")

        } else if(input$experiment_tabs == global__experiment__tab_names__bayesian) {
            
            shinyjs::hide('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_confidence__UI')
            shinyjs::hide('graph_options__conversion_rates__UI')
            shinyjs::hide('graph_options__trends__UI')
            shinyjs::show('graph_options__bayesian_posteriors__UI')
            shinyjs::hide('graph_options__raw_data__UI')
            #updateCollapse(session, 'main__bscollapse', open="Graph Options")
            
        } else if(input$experiment_tabs == global__experiment__tab_names__raw_data) {
            
            shinyjs::hide('experiment__stat_type_select')

            shinyjs::hide('graph_options__percent_change__UI')
            shinyjs::hide('graph_options__percent_change_confidence__UI')
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


            updateCollapse(session, 'conversion_rates__bscollapse', open="Graph Options")

        } else if(input$conversion_rates__graph_type == "Historical") {

            updateCollapse(session, 'conversion_rates__bscollapse', close="Graph Options")

            shinyjs::hide('conversion_rates__metric')
            shinyjs::hide('conversion_rates__cohort_type')
            shinyjs::hide('conversion_rates__cr_type')
            shinyjs::hide('conversion_rates__snapshot_1_days')
            shinyjs::hide('conversion_rates__snapshot_2_days')
            shinyjs::hide('conversion_rates__snapshot_3_days')
            shinyjs::hide('conversion_rates__max_days_to_convert')

        } else if(input$conversion_rates__graph_type == "Attribution") {

            updateCollapse(session, 'conversion_rates__bscollapse', close="Graph Options")

            shinyjs::hide('conversion_rates__metric')
            shinyjs::hide('conversion_rates__cohort_type')
            shinyjs::hide('conversion_rates__cr_type')
            shinyjs::hide('conversion_rates__snapshot_1_days')
            shinyjs::hide('conversion_rates__snapshot_2_days')
            shinyjs::hide('conversion_rates__snapshot_3_days')
            shinyjs::hide('conversion_rates__max_days_to_convert')

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

        insert_custom_progress_bar("Rendering Percent Change Graph")
        log_message_block_start("Rendering Percent Change Graph")

        log_message_variable('experiment__select', input$experiment__select)
        log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

        if(input$experiment__stat_type_select == "Frequentist") {

            plot__percent_change_frequentist(reactive__experiments_summary(),
                                             input$experiment__select,
                                             p_value_threshold=global__p_value_threshold)
        } else {

            plot__percent_change_bayesian(reactive__experiments_summary(), input$experiment__select)
        }
    }, height=function() {

        session$clientData$output_plot__percent_change_width * 0.55  # set height to % of width
    })
    addPopover(
        session,
        'plot__percent_change', 
        title="Percent Change",
        content=HTML("Shows the percent change from the Control to the Variant.<br><br>The Frequentist graph gives the p-value, while the Bayesian graph gives the probability that the Variant's conversion rate is higher than the Control's conversion rate.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates.<br><br>The conversion rates will be lower than the historical values because of the attribution windows."),
        placement="left", trigger="hover", options=NULL)

    output$plot__percent_change_confidence <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__stat_type_select)

        insert_custom_progress_bar("Rendering Percent Change Conf. Graph")
        log_message_block_start("Rendering Percent Change Conf. Graph")

        log_message_variable('experiment__select', input$experiment__select)
        log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

        if(input$experiment__stat_type_select == "Frequentist") {

            plot__percent_change_conf_frequentist(reactive__experiments_summary(),
                                                  input$experiment__select,
                                                  p_value_threshold=global__p_value_threshold)
        } else {

            plot__percent_change_conf_bayesian(reactive__experiments_summary(), input$experiment__select)
        }
    }, height=function() {

        session$clientData$output_plot__percent_change_confidence_width * 0.55  # set height to % of width
    })
    addPopover(
        session,
        'plot__percent_change_confidence', 
        title="Percent Change Confidence Level",
        content=HTML("Shows the percent change between the Control and Variant, with confidence intervals.<br><br>The frequentist graph shows the p-value at the top of the graph, while the Bayesian graph gives the probability that the Variant's conversion rate is higher than the Control's conversion rate.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates.<br><br>The conversion rates will be lower than the historical values because of the attribution windows."),
        placement="left", trigger="hover", options=NULL)

    output$plot__conversion_rates <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__stat_type_select)

        insert_custom_progress_bar("Rendering Conversion Rates Graph")
        log_message_block_start("Rendering Conversion Rates Graph")

        log_message_variable('experiment__select', input$experiment__select)
        log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

        if(input$experiment__stat_type_select == "Frequentist") {

            plot__conversion_rates(reactive__experiments_summary(), input$experiment__select)
        } else {

            plot__conversion_rates_bayesian(reactive__experiments_summary(), input$experiment__select)
        }
    }, height=function() {

        session$clientData$output_plot__conversion_rates_width * 0.55  # set height to % of width
    })
    addPopover(
        session,
        'plot__conversion_rates', 
        title="Conversion Rates",
        content=HTML("Shows the conversion rates of the Control and Variant groups.<br><br>The frequentist graph shows the conversions and trials above the bar, while the bayesian graph shows the alpha and beta values above the bar.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates.<br><br>The conversion rates will be lower than the historical values because of the attribution windows."),
        placement="left", trigger="hover", options=NULL)

    output$plot__trends <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__trends__metric_select)
        req(input$experiment__stat_type_select)
        req(input$experiment__trend_graph_type)

        insert_custom_progress_bar("Rendering Trends Graphs")
        log_message_block_start("Rendering Trends Graph")

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
    }, height=function() {

        session$clientData$output_plot__trends_width * 0.55  # set height to % of width
    })
    addPopover(
        session,
        'plot__trends', 
        title="Percent Change Over time",
        content=HTML("Shows the percent change (i.e. Lift) from the Control to the Variant over the lifetime of the experiment.<br><br>There will be a lag between the start of the experiment and when data will start appearing, due to the attribution window.<br><br>The colored area is the confidence interval where red indicates that '0% Lift' is within the interval, and green indicates '0% Lift' is outside the interval.<br><br>The conversion rates will differ between the Frequentist and Bayesian graphs because the Bayesian methodology will anchor the conversion rates toward the prior rates.<br><br>The conversion rates will be lower than the historical values because of the attribution windows."),
        placement="left", trigger="hover", options=NULL)

    output$plot__bayesian_posteriors <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__bayesian_posterior__metric_select)

        insert_custom_progress_bar("Rendering Bayesian Posterior Graph")
        log_message_block_start("Rendering Bayesian Posterior Graph")

        log_message_variable('experiment__select', input$experiment__select)
        log_message_variable('experiment__bayesian_posterior__metric_select', input$experiment__bayesian_posterior__metric_select)
        log_message_variable('experiment__bayesian_posterior__show_prior', input$experiment__bayesian_posterior__show_prior)

        plot__bayesian_posterior(reactive__experiments_summary(),
                                 input$experiment__select,
                                 input$experiment__bayesian_posterior__metric_select,
                                 confidence_level=global__confidence_level,
                                 show_prior_distribution=input$experiment__bayesian_posterior__show_prior)
    }, height=function() {

        session$clientData$output_plot__bayesian_posteriors_width * 0.55  # set height to % of width
    })
    addPopover(
        session,
        'plot__bayesian_posteriors', 
        title="Posterior Probability Distributions of the Control & Variant",
        content=HTML("This graph shows the posterior probability distributions of the Control, Variant, and if selected, the Priors. It also shows a confidence intervals for the Control and Variant distributions below."),
        placement="left", trigger="hover", options=NULL)

    output$plot__website_traffic <- renderPlot({

        insert_custom_progress_bar("Rendering Website Traffic Graph")
        log_message_block_start("Rendering Website Traffic Graph")

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

        session$clientData$output_plot__website_traffic_width * 0.55  # set height to % of width
    })

    reactive__cohorted_snapshots <- reactive({

        withProgress(value=1/2, message="Creating Cohorted Snapshots", {

            log_message_block_start("Building Cohorted Conversion Rates")
            log_message_variable('conversion_rates__cohort_type', input$conversion_rates__cohort_type)
            log_message_variable('conversion_rates__max_days_to_convert', input$conversion_rates__max_days_to_convert)

            snapshots <- c(input$conversion_rates__snapshot_1_days,
                           input$conversion_rates__snapshot_2_days,
                           input$conversion_rates__snapshot_3_days)

            log_message_variable('snapshots', paste(snapshots, collapse=", "))

            cohorted_snapshots <- get__cohorted_conversions_snapshot(reactive__traffic_conversions_metric(),
                                                                     cohort_label=input$conversion_rates__cohort_type,
                                                                     snapshots=snapshots,
                                                                     snapshot_max_days=input$conversion_rates__max_days_to_convert)
        })
    })

    output$conversion_rates__plot <- renderPlot({

        req(input$conversion_rates__graph_type)

        insert_custom_progress_bar("Rendering Conversion Rate Plots")
        log_message_block_start("Rendering Conversion Rate Plots")
        log_message_variable('conversion_rates__graph_type', input$conversion_rates__graph_type)
        log_message_variable('conversion_rates__cr_type', input$conversion_rates__cr_type)
        log_message_variable('conversion_rates__metric', input$conversion_rates__metric)

        if(input$conversion_rates__graph_type == "Cohort") {
            
            req(input$conversion_rates__metric)

            if(input$conversion_rates__cr_type == "Absolute") {

                return (plot__conversion_rates_snapshot_absolute(reactive__cohorted_snapshots(),
                                                                 cohort_label=input$conversion_rates__cohort_type))


            } else {
                plot__conversion_rates_snapshot_percent(reactive__cohorted_snapshots(),
                                                        snapshot_max_days=input$conversion_rates__max_days_to_convert,
                                                        cohort_label=input$conversion_rates__cohort_type)
            }


        } else if(input$conversion_rates__graph_type == "Historical") { 

            plot__conversion_rates_historical(reactive__historical_conversion_rates())

        } else if(input$conversion_rates__graph_type == "Attribution") {

            plot__conversion_rates_attribution(reactive__historical_conversion_rates())

        } else {

            stopifnot(FALSE)
        }

    }, height=function() {

        session$clientData$output_conversion_rates__plot_width * 0.55  # set height to % of width
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

    output$duration_calculator__results_table <- renderTable({

        calc_results <- reactive__ab_test_calculator_results()

        if(is.null(calc_results)) {

            return (NULL)
        }

        log_message_variable("duration_calculator__metrics", paste(input$duration_calculator__metrics,
                                                                   collapse=', '))

        results_table <- calc_results$results %>%
            filter(Metric %in% input$duration_calculator__metrics) %>%
            mutate(`Estimated Days Required` = as.character(`Estimated Days Required`),
                   `Estimated Users Required` = comma_format()(`Estimated Users Required`),
                   `Historical Conversion Rate` = percent(`Historical Conversion Rate`),
                   `Detectable Conversion Rate` = percent(`Detectable Conversion Rate`))

        return (results_table)
    })

    output$duration_calculator__average_daily_traffic_header <- renderText("Average Daily Traffic")
    output$duration_calculator__average_daily_traffic_text <- renderText({
        

        calc_results <- reactive__ab_test_calculator_results()

        if(is.null(calc_results)) {

            return (NULL)
        }

        return (paste(comma_format()(round(calc_results$daily_traffic)), "Users"))
    })
})
