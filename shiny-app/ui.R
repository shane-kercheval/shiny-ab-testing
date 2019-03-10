library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('r_scripts/definitions.R')

shinyUI(tagList(
    
    useShinyjs(),
    # this is a hack because ggplot takes a long time to process some graphs, but withProgress doesn't block
    # because the ggplot object doesn't "load" until it is being rendered.
    conditionalPanel(condition="$('html').hasClass('shiny-busy')", global__progress_bar_html),

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
                        radioButtons(
                            inputId='experiment__stat_type_select',
                            label="Statistical Methodology",
                            choices=c("Frequentist", "Bayesian"),
                            selected="Frequentist",
                            inline=TRUE,
                            width=NULL, choiceNames=NULL,
                            choiceValues=NULL),
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
            tabPanel("Duration Estimator",
                fluidRow(
                    tags$br()
                )
            ),
            tabPanel("Website Traffic",
                column(3,
                    class='column-input-control-style',
                    bsCollapse(id='main__bscollapse', open=c("Options"), multiple=TRUE,
                        bsCollapsePanel(
                            "Options",
                            radioButtons(
                                inputId='traffic__tbd',
                                label="TBD",
                                choices=c("1", "2"),
                                selected="1",
                                inline=TRUE,
                                width=NULL, choiceNames=NULL,
                                choiceValues=NULL)
                        )
                    )
                ),
                column(9,
                    tags$br(),
                    tags$p('TBD')
                )                
            ),
            tabPanel("Conversion Rates",
                fluidRow(
                    tags$br()
                )
            )
        ),
        navbarMenu("More",
            tabPanel("View Raw Data",
                tags$br()
              #DT::dataTableOutput("table")
            ),
            tabPanel("Settings",
                tags$br(),
                fluidRow(
                    column(12, align="center",
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
    )
))
