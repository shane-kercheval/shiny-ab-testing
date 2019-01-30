library(shiny)
library(tidyverse)
library(stringr)
library(scales)

theme_set(theme_light())

source('r_scripts/definitions.R')

shinyServer(function(session, input, output) {

   
   data__load <- function(input_uploadFile, default_dataset_path) {
        reactive({

            upload_file_path <- input_uploadFile$datapath
            
            withProgress(value=1/2, message="Loading Data",{

                dataset <- NULL
                if(is.null(upload_file_path)) {

                    dataset <- read_csv(default_dataset_path)

                } else {

                    dataset <- read_csv(upload_file_path)
                }

                return (as.data.frame(dataset))
            })
        })
    }

    renderDataTable__head <- function(dataset, num_records_to_show) {

        renderDataTable({

            return (head(dataset(), num_records_to_show))
        })
    }

    renderDataTable__types <- function(dataset) {

        renderDataTable({

            withProgress(value=1/2, message='Loading Types',{

                local_dataset <- dataset()
                types <- map_chr(colnames(local_dataset), ~ class(local_dataset[, .])[1])
                return (data.frame(variable=colnames(local_dataset), type=types))
            })
        })
    }

    ##########################################################################################################
    # Load Attribution Windows
    ##########################################################################################################
    data__attribution_windows <- data__load(input$attribution_windows__uploadFile, simulated_data_path__attribution_windows)
    output$attribution_windows__head_table <- renderDataTable__head(data__attribution_windows, 500)
    output$attribution_windows__types_table <- renderDataTable__types(data__attribution_windows)

    ##########################################################################################################
    # Load Website Traffic
    ##########################################################################################################
    data__website_traffic <- data__load(input$website_traffic__uploadFile, simulated_data_path__website_traffic)
    output$website_traffic__head_table <- renderDataTable__head(data__website_traffic, 500)
    output$website_traffic__types_table <- renderDataTable__types(data__website_traffic)

    ##########################################################################################################
    # Experiment Traffic
    ##########################################################################################################
    data__experiment_traffic <- data__load(input$experiment_traffic__uploadFile, simulated_data_path__experiment_traffic)
    output$experiment_traffic__head_table <- renderDataTable__head(data__experiment_traffic, 500)
    output$experiment_traffic__types_table <- renderDataTable__types(data__experiment_traffic)

    ##########################################################################################################
    # Load Conversion Rate Data
    ##########################################################################################################
    data__conversion_rate_data <- data__load(input$conversion_rate_data__uploadFile, simulated_data_path__conversion_rate_data)
    output$conversion_rate_data__head_table <- renderDataTable__head(data__conversion_rate_data, 500)
    output$conversion_rate_data__types_table <- renderDataTable__types(data__conversion_rate_data)



    ##########################################################################################################
    # Derived Datasets
    ##########################################################################################################


    # experiment start/end dates
    data__experiment_start_stop <- reactive({

        withProgress(value=1/2, message='Processing Data', {

            data__experiment_traffic() %>%
                group_by(experiment_id) %>%
                summarise(start=min(first_joined_experiment),
                          end=max(first_joined_experiment)) %>%
                arrange(desc(start))
        })
    })

    # experiment paths & counts
    data__experiment_paths <- reactive({

        withProgress(value=1/2, message='Processing Data', {

            data__experiment_traffic() %>%
                count(experiment_id, path) %>%
                arrange(experiment_id, desc(n)) %>%
                rename(visits = n)
        })
    })

    ##########################################################################################################
    # Experiment Results & Analysis
    ##########################################################################################################

    output$experiment_results__experiment_selected__UI <- renderUI({

        withProgress(value=1/2, message='Generating Filters', {
            
            req(data__experiment_start_stop())

            experiments <- unique(data__experiment_start_stop()$experiment_id)
            selectInput(inputId='experiment_results__experiment_selected',
                        label = 'Experiment',
                        choices = experiments,
                        selected = experiments[1],
                        multiple = FALSE,
                        selectize = TRUE,
                        width = 500,
                        size = NULL)
        })
    })

    output$experiment_results__metrics_selected__UI <- renderUI({
            
        withProgress(value=1/2, message='Generating Filters', {

            req(data__attribution_windows())
            req(input$experiment_results__experiment_selected)

            metrics <- (data__attribution_windows() %>% 
                filter(experiment_id == input$experiment_results__experiment_selected))$metric_id

            checkboxGroupInput(inputId='experiment_results__metrics_selected',
                               label="Metrics",
                               choices=metrics,
                               selected=metrics,
                               inline=FALSE,
                               width=NULL)
        })
    })

    output$experiment_results__paths_selected__UI <- renderUI({
            
        withProgress(value=1/2, message='Generating Filters', {

            req(data__experiment_paths())
            req(input$experiment_results__experiment_selected)

            paths <- (data__experiment_paths() %>% 
                filter(experiment_id == input$experiment_results__experiment_selected))$path

            checkboxGroupInput(inputId='experiment_results__paths_selected',
                               label="Paths",
                               choices=paths,
                               selected=paths,
                               inline=FALSE,
                               width=NULL)
        })
    })

    ##########################################################################################################
    # Website Traffic
    ##########################################################################################################
    output$website_traffic__plot <- renderPlot({

        show_path <- TRUE
        top_x_paths <- 2
        start_date <- Sys.Date() - 270
        end_date <- Sys.Date() - 30

        visitors_per_day_per_path <- data__website_traffic() %>%
            filter(visit_date >= start_date & visit_date <= end_date) %>%
            group_by(user_id) %>%
            mutate(visit_index=rank(visit_date, ties.method = "first")) %>%
            ungroup() %>%
            mutate(date = factor(create_cohort(visit_date)),
                   path= fct_lump(path,  n=top_x_paths),
                   visit_type = ifelse(visit_index == 1, 'New', 'Returning'))
            
        if(show_path) {

            visitors_per_day_per_path %>%
                count(date, path, visit_type) %>%
                ggplot(aes(x=date, y=n, color=path, group=path)) +
                    geom_line() +
                    geom_point() + 
                    facet_grid(visit_type ~ .) + 
                    scale_y_continuous(labels = comma_format()) +
                    geom_text(aes(label=n), vjust=-1, size=3.5, check_overlap=TRUE) +    
                    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
                    labs(title='Website Visitors',
                         y='Number of Visits',
                         x='Month',
                         color='Path')
        } else {

            visitors_per_day_per_path %>%
                count(date, visit_type) %>%
                ggplot(aes(x=date, y=n, color=visit_type, group=visit_type)) +
                    geom_line() +
                    geom_point() + 
                    expand_limits(y=0) +
                    scale_y_continuous(labels = comma_format()) +
                    geom_text(aes(label=n), vjust=-1, size=3.5, check_overlap=TRUE) +
                    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
                    labs(title='Website Visitors',
                         y='Number of Visits',
                         x='Month',
                         color='Visitor Type')
        }

    }, height = function() {

        session$clientData$output_website_traffic__plot_width * 0.66  # set height to % of width
    })
})
