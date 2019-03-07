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

        bsCollapse(id='main__bscollapse', open='Experiment', multiple=TRUE,
            bsCollapsePanel(
                'Experiment',
                uiOutput('experiment__select__UI'),
                uiOutput('experiment__metric_select__UI'),
                radioButtons(
                    inputId='experiment__stat_type_select',
                    label="Statistics Methodology",
                    choices=c("Frequentist", "Bayesian"),
                    selected="Frequentist",
                    inline=TRUE,
                    width=NULL, choiceNames=NULL,
                    choiceValues=NULL)
            )
        )
    ),
    column(9,
        tabsetPanel(
            id='main_tabs',
            type='tabs',
            tabPanel(
                "Percent Change",
                #id='main_tabs__overview',
                tags$br(),
                plotOutput(outputId='plot__percent_change')
                #tags$div(class='results-table', dataTableOutput(outputId='source_data__head_table'))
            ),
            tabPanel(
                "Percent Change Confidence",
                #id='main_tabs__overview',
                tags$br(),
                plotOutput(outputId='plot__percent_change_confidence')
                #tags$div(class='results-table', dataTableOutput(outputId='source_data__head_table'))
            ),
            tabPanel(
                "Conversion Rates",
                #id='main_tabs__dropoff',
                tags$br(),
                plotOutput(outputId='plot__conversion_rates')
            ),
            tabPanel(
                "Trend",
                #id='main_tabs__dropoff',
                tags$br(),
                plotOutput(outputId='plot__conversion_rates')
            ),
            tabPanel(
                "Bayesian Posterior",
                #id='main_tabs__value_counts',
                tags$br(),
                plotOutput(outputId='plot__bayesian_posterior')
            )
        )
    )
))
