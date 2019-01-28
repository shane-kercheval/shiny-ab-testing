library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

shinyUI(fluidPage(theme="custom.css",
                  
    useShinyjs(),

    titlePanel("AB Testing"),
    navlistPanel(
        tabPanel(
            'Dashboard',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='dashboard__bscollapse', 
                           open=c('TBD'),
                           multiple=TRUE,
                    bsCollapsePanel(
                        'TBD',
                        style='default'
                    )
                )
            ),
            column(9,
                tags$h1("TBD")
            )
        ),
        tabPanel(
            'Load Dataset',
            tabsetPanel(type='tabs',
                tabPanel(
                    'Prior Data',
                    tags$br(),
                    tags$p("A simulated dataset is loaded, but you may load your own, in the same format, via csv."),
                    tags$br(),
                    fileInput(inputId='prior_data__uploadFile', "Load .csv:"),
                    tags$br(),tags$br(),
                    tabsetPanel(type='tabs',
                        tabPanel(
                            "First 500 Records of Prior Dataset",
                            tags$div(class='results-table', dataTableOutput(outputId='prior_data__head_table'))
                        ),
                        tabPanel(
                            'Variable Types',
                            tags$div(class='results-table', dataTableOutput(outputId='prior_data__types_table'))
                        )
                    )
                ),
                tabPanel(
                    'Experiment Results',
                    tags$br(),
                    tags$p("A simulated dataset is loaded, but you may load your own, in the same format, via csv."),
                    tags$br(),
                    fileInput(inputId='experiment_data__uploadFile', "Load .csv:"),
                    tags$br(),tags$br(),
                    tabsetPanel(type='tabs',
                        tabPanel(
                            "First 500 Records of Experiment Results",
                            tags$div(class='results-table', dataTableOutput(outputId='experiment_data__head_table'))
                        ),
                        tabPanel(
                            'Variable Types',
                            tags$div(class='results-table', dataTableOutput(outputId='experiment_data__types_table'))
                        )
                    )
                )
            )
        ),
        widths=c(2,10)
    )

))
