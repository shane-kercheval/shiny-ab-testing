library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('r_scripts/definitions.R')

shinyUI(fluidPage(theme="custom.css",

    useShinyjs(),

    titlePanel('A/B Tests'),

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
                uiOutput('graph_options__bayesian_posteriors__UI')
            )
        )
    ),
    column(9,
        tabsetPanel(
            id='main_tabs',
            type='tabs',
            tabPanel(
                global__experiment__tab_names__percent_change,
                #id='main_tabs__overview',
                tags$br(),
                plotOutput(outputId='plot__percent_change')
                #tags$div(class='results-table', dataTableOutput(outputId='source_data__head_table'))
            ),
            tabPanel(
                global__experiment__tab_names__percent_change_conf,
                #id='main_tabs__dropoff',
                tags$br(),
                plotOutput(outputId='plot__percent_change_confidence')
            ),
            tabPanel(
                global__experiment__tab_names__conversion_rates,
                #id='main_tabs__discontinuity',
                tags$br(),
                plotOutput(outputId='plot__conversion_rates')
            ),
            tabPanel(
                global__experiment__tab_names__trends,
                #id='main_tabs__value_counts',
                tags$br(),
                plotOutput(outputId='plot__trends')
            ),
            tabPanel(
                global__experiment__tab_names__bayesian,
                #id='main_tabs__value_counts',
                tags$br(),
                plotOutput(outputId='plot__bayesian_posteriors')
            )
        )
    )
))
