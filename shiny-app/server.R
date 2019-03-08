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

shinyServer(function(session, input, output) {

    reactive__experiment_data <- reactive({

        withProgress(value=1/2, message="Loading Data", {

            log_message_block_start("Loading Data")
            load_data()
        })
    })

    reactive__experiments_summary <- reactive({

        withProgress(value=1/2, message="Loading Experiments Summary Data", {

            log_message_block_start("Loading Experiments Summary Data")
            readRDS('processed_data/experiments_summary.RDS')
        })
    })

    reactive__experiments_daily_summary <- reactive({

        withProgress(value=1/2, message="Loading Daily Summary Data", {

            log_message_block_start("Loading Daily Summary Data")
            readRDS('processed_data/experiments_daily_summary.RDS')
        })
    })

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

    output$experiment__metric_select__UI <- renderUI({

        req(reactive__experiments_summary())
        req(input$experiment__select)

        log_message_variable('experiment__select', input$experiment__select)
        
        metrics <- reactive__experiments_summary() %>%
            filter(experiment_id == input$experiment__select) %>%
            arrange(desc(control_conversion_rate)) %>%
            get_vector('metric_id', return_unique=TRUE)

        log_message_variable('metrics', paste(metrics, collapse='; '))

        selectInput(inputId='experiment__metric_select',
                    label="Choose Experiment",
                    choices=metrics,
                    selected=metrics[1],
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })

    output$plot__percent_change <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__metric_select)
        req(input$experiment__stat_type_select)

        withProgress(value=1/2, message="Creating Percent Change Graph", {

            log_message_block_start("Creating Percent Change Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__metric_select', input$experiment__metric_select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

            if(input$experiment__stat_type_select == "Frequentist") {

                plot__percent_change_frequentist(reactive__experiments_summary(),
                                                 input$experiment__select,
                                                 p_value_threshold=global__p_value_threshold)
            } else {

                plot__percent_change_bayesian(reactive__experiments_summary(), input$experiment__select)
            }
        })
    }, height=function() {

        session$clientData$output_plot__percent_change_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__percent_change', 
        title="Percent Change Graph",
        content="TBD",
        placement="left", trigger="hover", options=NULL)


    output$plot__percent_change_confidence <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__metric_select)
        req(input$experiment__stat_type_select)

        withProgress(value=1/2, message="Creating Percent Change Graph", {

            log_message_block_start("Creating Percent Change Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__metric_select', input$experiment__metric_select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

            if(input$experiment__stat_type_select == "Frequentist") {

                plot__percent_change_conf_frequentist(reactive__experiments_summary(),
                                                      input$experiment__select,
                                                      p_value_threshold=global__p_value_threshold)
            } else {

                plot__percent_change_conf_bayesian(reactive__experiments_summary(), input$experiment__select)
            }

            
        })
    }, height=function() {

        session$clientData$output_plot__percent_change_confidence_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__percent_change_confidence', 
        title="Percent Change Graph",
        content="TBD",
        placement="left", trigger="hover", options=NULL)


    output$plot__conversion_rates <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__metric_select)
        req(input$experiment__stat_type_select)

        withProgress(value=1/2, message="Creating Percent Change Graph", {

            log_message_block_start("Creating Percent Change Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__metric_select', input$experiment__metric_select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

            if(input$experiment__stat_type_select == "Frequentist") {

                plot__conversion_rates(reactive__experiments_summary(), input$experiment__select)
            } else {

                plot__conversion_rates_bayesian(reactive__experiments_summary(), input$experiment__select)
            }

            
        })
    }, height=function() {

        session$clientData$output_plot__conversion_rates_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__conversion_rates', 
        title="Percent Change Graph",
        content="TBD",
        placement="left", trigger="hover", options=NULL)


    output$plot__trends <- renderPlot({

        req(reactive__experiments_summary())
        req(input$experiment__select)
        req(input$experiment__metric_select)
        req(input$experiment__stat_type_select)

        withProgress(value=1/2, message="Creating Percent Change Graph", {

            log_message_block_start("Creating Percent Change Graph")

            log_message_variable('experiment__select', input$experiment__select)
            log_message_variable('experiment__metric_select', input$experiment__metric_select)
            log_message_variable('experiment__stat_type_select', input$experiment__stat_type_select)

            if(input$experiment__stat_type_select == "Frequentist") {

                plot__daily_percent_change_frequentist(reactive__experiments_daily_summary(),
                                                       input$experiment__select,
                                                       input$experiment__metric_select)
            } else {

                plot__daily_percent_change_bayesian(reactive__experiments_daily_summary(),
                                                    input$experiment__select,
                                                    input$experiment__metric_select)
            }

            
        })
    }, height=function() {

        session$clientData$output_plot__trends_width * 0.65  # set height to % of width
    })
    addPopover(
        session,
        'plot__trends', 
        title="Percent Change Graph",
        content="TBD",
        placement="left", trigger="hover", options=NULL)






})

