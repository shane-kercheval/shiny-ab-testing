library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('r_scripts/definitions.R')

global__progress_bar_html <- HTML('"<div id="shiny-notification-panel"><div id="shiny-notification-42ec5661c2722d29" class="shiny-notification"><div class="shiny-notification-close">×</div><div class="shiny-notification-content"><div class="shiny-notification-content-text"><div id="shiny-progress-42ec5661c2722d29" class="shiny-progress-notification"><div class="progress progress-striped active" style=""><div class="progress-bar" style="width: 50%;"></div></div><div class="progress-text"><span class="progress-message">Processing Data</span> <span class="progress-detail"></span></div></div></div><div class="shiny-notification-content-action"></div></div></div></div>"')

shinyUI(tagList(
    useShinyjs(),
    navbarPage("A/B Test Dashboard", theme="custom.css",
        tabPanel(
            "Experiment Analysis",
            column(3,
                class='column-input-control-style',
                bsCollapse(id='main__bscollapse', open=c("Experiment", "Graph Options"), multiple=TRUE,
                    bsCollapsePanel(
                        "Experiment",
                        uiOutput('experiment__select__UI')
                    ),
                    bsCollapsePanel(
                        "Graph Options",
                        tags$div(class='dynamic_filter',
                            radioButtons(
                                inputId='experiment__stat_type_select',
                                label="Statistical Methodology",
                                choices=c("Frequentist", "Bayesian"),
                                selected="Frequentist",
                                inline=TRUE,
                                width=NULL, choiceNames=NULL,
                                choiceValues=NULL)
                        ),
                        bsTooltip(id='experiment__stat_type_select',
                                  title="Choose to display the Frequentist or Bayesian methodology.",
                                  placement='top', trigger='hover'),
                        uiOutput('graph_options__percent_change__UI'),
                        uiOutput('graph_options__percent_change_conidence__UI'),
                        uiOutput('graph_options__conversion_rates__UI'),
                        uiOutput('graph_options__trends__UI'),
                        uiOutput('graph_options__bayesian_posteriors__UI'),
                        uiOutput('graph_options__raw_data__UI')
                    )
                )
            ),
            column(9,
                tabsetPanel(
                    id='experiment_tabs',
                    type='tabs',
                    tabPanel(
                        global__experiment__tab_names__percent_change,
                        tags$br(),
                        plotOutput(outputId='plot__percent_change')
                    ),
                    tabPanel(
                        global__experiment__tab_names__percent_change_conf,
                        tags$br(),
                        plotOutput(outputId='plot__percent_change_confidence')
                    ),
                    tabPanel(
                        global__experiment__tab_names__conversion_rates,
                        tags$br(),
                        plotOutput(outputId='plot__conversion_rates')
                    ),
                    tabPanel(
                        global__experiment__tab_names__trends,
                        tags$br(),
                        plotOutput(outputId='plot__trends')
                    ),
                    tabPanel(
                        global__experiment__tab_names__bayesian,
                        tags$br(),
                        plotOutput(outputId='plot__bayesian_posteriors')
                    ),
                    tabPanel(
                        global__experiment__tab_names__raw_data,
                        tags$br(),
                        tags$div(class='results-table', dataTableOutput(outputId='experiment__raw_data_table'))
                    )

                )
            )
        ),
        navbarMenu("Planning",
            tabPanel("Test Duration Estimator",
                fluidRow(
                    tags$br()
                )
            ),
            tabPanel("Conversion Rates",
                column(3,
                    class='column-input-control-style',
                    bsCollapse(id='conversion_rates__bscollapse', open=c("Options", "Graph Options"), multiple=TRUE,
                        bsCollapsePanel(
                            "Options",
                            tags$div(
                                class="dynamic_filter",
                                radioButtons(
                                    inputId='conversion_rates__graph_type',
                                    label="Graph Type",
                                    choices=c("Attribution", "Historical", "Cohort"),
                                    selected="Attribution",
                                    inline=FALSE
                                )
                            ),
                            hidden(uiOutput('conversion_rates__metric__UI'))
                        ),
                        bsCollapsePanel(
                            "Graph Options",
                            tags$div(
                                class="dynamic_filter",
                                radioButtons(
                                    inputId='conversion_rates__cohort_type',
                                    label="Cohort Type",
                                    choices=c("Week", "Month"),
                                    #choiceValues=c('%W', '%m'),
                                    selected="Week",
                                    inline=TRUE
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                radioButtons(
                                    inputId='conversion_rates__cr_type',
                                    label="Conversion Rate Type",
                                    choices=c("Absolute", "Percent of All Conversions"),
                                    selected="Absolute",
                                    inline=TRUE
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                numericInput(
                                    inputId='conversion_rates__snapshot_1_days',
                                    label="Snapshot 1 (days)",
                                    value=1,
                                    min = NA,
                                    max = NA,
                                    step = NA,
                                    width = NULL
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                numericInput(
                                    inputId='conversion_rates__snapshot_2_days',
                                    label="Snapshot 2 (days)",
                                    value=5,
                                    min = NA,
                                    max = NA,
                                    step = NA,
                                    width = NULL
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                numericInput(
                                    inputId='conversion_rates__snapshot_3_days',
                                    label="Snapshot 3 (days)",
                                    value=10,
                                    min = NA,
                                    max = NA,
                                    step = NA,
                                    width = NULL
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                numericInput(
                                    inputId='conversion_rates__max_days_to_convert',
                                    label="Max Days to Convert",
                                    value=30,
                                    min = NA,
                                    max = NA,
                                    step = NA,
                                    width = NULL
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                numericInput(
                                    inputId='conversion_rates__exclude_last_n_days',
                                    label="Exclude Last N Days",
                                    value=30,
                                    min = NA,
                                    max = NA,
                                    step = NA,
                                    width = NULL
                                )
                            )
                            
                        )
                    )
                ),
                column(9,
                    plotOutput(outputId='conversion_rates__plot')
                )                
            ),
            tabPanel("Website Traffic",
                column(3,
                    class='column-input-control-style',
                    bsCollapse(id='website_traffic__bscollapse', open=c("Options"), multiple=TRUE,
                        bsCollapsePanel(
                            "Options",
                            tags$div(
                                class="dynamic_filter",
                                radioButtons(
                                    inputId='website_traffic__cohort_type',
                                    label="Cohort Type",
                                    choices=c("Week", "Month"),
                                    selected="Week",
                                    inline=TRUE
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                checkboxInput(
                                    inputId='website_traffic__show_first_time_visits',
                                    label="Show First-Time User Visits",
                                    value=TRUE
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                checkboxInput(
                                    inputId='website_traffic__show_path_breakdown',
                                    label="Show Path Breakdown",
                                    value=TRUE
                                )
                            ),
                            tags$div(
                                class="dynamic_filter",
                                numericInput(
                                    inputId='website_traffic__top_n_paths',
                                    label="Top N Paths",
                                    value=3,
                                    min = NA,
                                    max = NA,
                                    step = NA,
                                    width = NULL
                                )
                            )
                        )
                    )
                ),
                column(9,
                    plotOutput(outputId='plot__website_traffic')
                )                
            )
        ),
        navbarMenu("More",
            tabPanel("View Raw Data",
                column(3,
                    class='column-input-control-style',
                    bsCollapse(id='main__bscollapse', open=c("Dataset"), multiple=TRUE,
                        bsCollapsePanel(
                            "Dataset",
                            tags$div(class='dynamic_filter',
                                radioButtons(
                                    inputId='raw_data__select_dataset',
                                    label="",
                                    choices=c(
                                        "Attribution Windows",
                                        "Experiment Info",
                                        "Experiment Traffic",
                                        "Website Traffic",
                                        "Conversion Events",
                                        "Experiments Summary",
                                        "Daily Summary"
                                    ),
                                    selected="Attribution Windows",
                                    inline=FALSE,
                                    width=NULL, choiceNames=NULL,
                                    choiceValues=NULL)
                            )
                        )
                    )
                ),
                column(9, #align="center",
                    tags$h2('First 1000 Records of Selected Dataset'),
                    tags$div(class='results-table', dataTableOutput(outputId='raw_data__table'))
                )      

              #DT::dataTableOutput("table")
            ),
            tabPanel("Settings",
                tags$br(),
                fluidRow(
                    column(1, #align="center",
                        tags$br()
                    ),
                    column(11, #align="center",
                        tags$h2('Settings'),
                        tags$div(class='results-table',
                            dataTableOutput(outputId='more__settings__table'))
                    )
                )
            ),
            tabPanel("About",
                fluidRow(
                    tags$br()
                #     column(6,
                #            includeMarkdown("about.md")
                #     ),
                #     column(3,
                #            img(class="img-polaroid",
                #            src=paste0("http://upload.wikimedia.org/",
                #            "wikipedia/commons/9/92/",
                #            "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                #   tags$small(
                #     "Source: Photographed at the Bay State Antique ",
                #     "Automobile Club's July 10, 2005 show at the ",
                #     "Endicott Estate in Dedham, MA by ",
                #     a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                #       "User:Sfoskett")
                #   )
                # )
                )
            )
        )
    ),
    # this is a hack because ggplot takes a long time to process some graphs, but withProgress doesn't block
    # because the ggplot object doesn't "load" until it is being rendered.
    conditionalPanel(condition="$('html').hasClass('shiny-busy')", global__progress_bar_html)
))
