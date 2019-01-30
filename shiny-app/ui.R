library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

shinyUI(fluidPage(theme="custom.css",
                  
    useShinyjs(),

    titlePanel("AB Testing"),
    navlistPanel(
        tabPanel(
            'Experiment Results & Analysis',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='dashboard__bscollapse', 
                           open=c("Experiment", "Metrics", "Paths"),
                           multiple=TRUE,
                    bsCollapsePanel(
                        "Experiment",
                        style='default',
                        uiOutput('experiment_results__experiment_selected__UI'),
                        uiOutput('experiment_results__metrics_selected__UI')
                    ),
                    bsCollapsePanel(
                        "Paths",
                        style='default',
                        uiOutput('experiment_results__paths_selected__UI')
                    )
                )
            ),
            column(9,
                tabsetPanel(type='tabs',
                    tabPanel(
                        'Overview',
                        tags$p('TBD')
                    ),
                    tabPanel(
                        'Bayesian',
                        tags$p('TBD')
                    ),
                    tabPanel(
                        'p-value oscillation',
                        tags$p('TBD')
                    ),
                    tabPanel(
                        'Misc',
                        tags$p('TBD')
                    )
                )
            )
        ),
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
            'Duration Calculator',
            column(3,
                class='column-input-control-style'
                # bsCollapse(id='dashboard__bscollapse', 
                #            open=c('TBD'),
                #            multiple=TRUE,
                #     bsCollapsePanel(
                #         'TBD',
                #         style='default'
                #     )
                # )

            ),
            column(9,
                tags$p('TBD')
            )
        ),
        tabPanel(
            'Website Traffic',
            column(3,
                class='column-input-control-style'
                # bsCollapse(id='dashboard__bscollapse', 
                #            open=c('TBD'),
                #            multiple=TRUE,
                #     bsCollapsePanel(
                #         'TBD',
                #         style='default'
                #     )
                # )

            ),
            column(9,
                plotOutput(outputId='website_traffic__plot')
            )
        ),
        tabPanel(
            'Load Dataset',
            tabsetPanel(type='tabs',
                tabPanel(
                    'Attribution Windows',
                    tags$br(),
                    tags$p("A simulated dataset is loaded, but you may load your own, in the same format, via csv."),
                    tags$br(),
                    tags$p("This dataset contains, for each experiment and metric, the maximum number of days allowed between when a user first joins the experiment and when their conversion to the metric will be attributed to the associated experiment/variation."),
                    tags$br(),
                    fileInput(inputId='attribution_windows__uploadFile', "Load .csv:"),
                    tags$br(),tags$br(),
                    tabsetPanel(type='tabs',
                        tabPanel(
                            "First 500 Records of Attribution Windows",
                            tags$div(class='results-table', dataTableOutput(outputId='attribution_windows__head_table'))
                        ),
                        tabPanel(
                            'Variable Types',
                            tags$div(class='results-table', dataTableOutput(outputId='attribution_windows__types_table'))
                        )
                    )
                ),
                tabPanel(
                    'Website Traffic',
                    tags$br(),
                    tags$p("A simulated dataset is loaded, but you may load your own, in the same format, via csv."),
                    tags$br(),
                    tags$p("This dataset contains per-day website traffic. A single row represents one or more visits to a specific path (i.e. url) for a given user_id, on a given day."),
                    tags$br(),
                    fileInput(inputId='website_traffic__uploadFile', "Load .csv:"),
                    tags$br(),tags$br(),
                    tabsetPanel(type='tabs',
                        tabPanel(
                            "First 500 Records of Website Traffic",
                            tags$div(class='results-table', dataTableOutput(outputId='website_traffic__head_table'))
                        ),
                        tabPanel(
                            'Variable Types',
                            tags$div(class='results-table', dataTableOutput(outputId='website_traffic__types_table'))
                        )
                    )
                ),
                tabPanel(
                    'Experiment Traffic',
                    tags$br(),
                    tags$p("A simulated dataset is loaded, but you may load your own, in the same format, via csv."),
                    tags$br(),
                    tags$p("This dataset contains gives a list of user_ids and the experiments they have been part of, including the variation, the date they first joined the experiment, and the path/url they were on."),
                    tags$br(),
                    fileInput(inputId='experiment_traffic__uploadFile', "Load .csv:"),
                    tags$br(),tags$br(),
                    tabsetPanel(type='tabs',
                        tabPanel(
                            "First 500 Records of Experiment Traffic",
                            tags$div(class='results-table', dataTableOutput(outputId='experiment_traffic__head_table'))
                        ),
                        tabPanel(
                            'Variable Types',
                            tags$div(class='results-table', dataTableOutput(outputId='experiment_traffic__types_table'))
                        )
                    )
                ),
                tabPanel(
                    'User Conversion Rates',
                    tags$br(),
                    tags$p("A simulated dataset is loaded, but you may load your own, in the same format, via csv."),
                    tags$br(),
                    tags$p("This dataset contains gives a list of user_ids and their conversion rate dates. If a user did not convert for a particular metric, the user_id/metric_id is not included in the dataset."),
                    tags$br(),
                    fileInput(inputId='conversion_rate_data__uploadFile', "Load .csv:"),
                    tags$br(),tags$br(),
                    tabsetPanel(type='tabs',
                        tabPanel(
                            "First 500 Records of User Conversion Rates",
                            tags$div(class='results-table', dataTableOutput(outputId='conversion_rate_data__head_table'))
                        ),
                        tabPanel(
                            'Variable Types',
                            tags$div(class='results-table', dataTableOutput(outputId='conversion_rate_data__types_table'))
                        )
                    )
                )
            )
        ),
        widths=c(2,10)
    )
))
