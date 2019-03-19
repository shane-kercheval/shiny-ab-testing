library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('r_scripts/definitions.R')

create_menu_label <- function(label, font_weight=700) {
    HTML(paste0('<span style="font-weight: ', font_weight,'; font-size: 14px;" class="shiny-text-output">', label,':</span>'))
}

shinyUI(tagList(
    useShinyjs(),
    navbarPage("A/B Test Dashboard",
        theme="custom.css",
        id='nav_bar_page',
        tabPanel(
            "Experiment Analysis",
            column(3,
                class='column-input-control-style',
                bsCollapse(id='main__bscollapse', open=c("Experiment", "Experiment Information", "Graph Options"), multiple=TRUE,
                    bsCollapsePanel(
                        "Experiment",                            
                        uiOutput('experiment__select__UI')
                    ),
                    bsCollapsePanel(
                        "Experiment Information",
                        fluidRow(
                            column(6,
                                #style='padding-left: 10px',
                                create_menu_label('Start Date'),
                                tags$br(),
                                create_menu_label('End Date')
                            ),
                            column(6,
                                style='padding-left: 0px',
                                uiOutput('experiment_info__start_date__UI', inline = TRUE),
                                tags$br(),
                                uiOutput('experiment_info__end_date__UI', inline = TRUE)
                            )
                        ),
                        tags$br(),tags$br(),
                        tags$div(id='attribution_lag_tooltip_div',
                            fluidRow(
                                column(12,
                                    create_menu_label('Attribution Lag'),
                                    tags$br(),tags$br()
                                )
                            ),
                            fluidRow(
                                column(6,
                                    #align='right',
                                    #style='padding-right: 50px',
                                    uiOutput('experiment_info__data_collection_metrics__UI')
                                ),
                                column(6,
                                    style='padding-left: 0px',
                                    uiOutput('experiment_info__data_collection_lags__UI', inline = TRUE)
                                )
                            )
                        ),
                        bsTooltip(id='attribution_lag_tooltip_div',
                                  title="Users that have joined the experiment in the last N days, where N is the attribution window per metric, will not count towards the conversion rates and will be excluded from the data until the attribution window has expired.<br><br>This section shows how many days of data is still excluded from each metric (i.e. the lag).",
                                  placement='top', trigger='hover')
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
                        uiOutput('graph_options__percent_change_confidence__UI'),
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
            tabPanel("Sample Size Calculator",
                column(3,
                    class='column-input-control-style',
                    bsCollapse(id='duration_calculator__bscollapse', open=c("Site Options", "Statistical Options"), multiple=TRUE,
                        bsCollapsePanel(
                            "Site Options",
                            uiOutput('duration_calculator__url__UI'),
                            uiOutput('duration_calculator__metrics__UI')
                        ),
                        bsCollapsePanel(
                            "Statistical Options",
                            tags$div(
                                class="dynamic_filter",
                                sliderTextInput(inputId='duration_calculator__mde',
                                   label='Minimum Detectable Effect', ## percent increase
                                   choices = c(1, seq(5, 50, 5)),
                                   selected = 5, 
                                   grid = TRUE,
                                   post  = " %")
                            ),
                            tags$div(
                                class="dynamic_filter",
                                sliderTextInput(inputId='duration_calculator__alpha',
                                   label='Probability of a False Positive (i.e. alpha)',
                                   choices = c(1, seq(5, 20, 5)),
                                   selected = 5, 
                                   grid = TRUE,
                                   post  = " %")
                            ),
                            tags$div(
                                class="dynamic_filter",
                                sliderTextInput(inputId='duration_calculator__beta',
                                   label='Probability of a False Negative (i.e. beta)',
                                   choices = c(1, seq(5, 20, 5)),
                                   selected = 20, 
                                   grid = TRUE,
                                   post  = " %")
                            ),
                            bsTooltip(id='duration_calculator__mde',
                                      title="Choose the \\'minimum detectable effect\\' (MDE) i.e. the smallest percent change from the control to the variant that can be detected for a given sample size.<br><br>The smaller the MDE, the larger the sample size needed to detect the change.",
                                      placement='top', trigger='hover'),
                            bsTooltip(id='duration_calculator__alpha',
                                      title="The probability of a false positive is the probability that the percent change from the control to the variant will be labeled as a statistically significant change, when it is actually not statistically significant.<br><br>The smaller the probability, the larger the sample size needed to detect the change.",
                                      placement='top', trigger='hover'),
                            bsTooltip(id='duration_calculator__beta',
                                      title="The probability of a false negative is the probability that the percent change from the control to the variant will be labeled as *not* being statistically significant, when it is actually statistically significant.<br><br>The smaller the probability, the larger the sample size needed to detect the change.",
                                      placement='top', trigger='hover')
                        )
                    )
                ),
                column(9,
                    tags$div(id='div_estimated_days_results',
                        tags$h4('Estimated Days & Sample Size for Running Experiment', style='margin-bottom: 40px'),
                        fluidRow(style = paste('padding: 19px',
                                               'padding-top: 1px',
                                               'margin: 2px',
                                               sep = ";"),
                            tableOutput(outputId='duration_calculator__results_table'),
                            #tags$div(class='results-table', tableOutput(outputId='duration_calculator__results_table'))
                            tags$br(),
                            tags$br(),
                            textOutput(outputId = 'duration_calculator__average_daily_traffic_header', inline = TRUE),
                            tags$head(tags$style('#duration_calculator__average_daily_traffic_header{font-weight: 700; font-size: 14px; margin-right: 10px}')),
                            textOutput(outputId = 'duration_calculator__average_daily_traffic_text', inline = TRUE)
                        )
                    )
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
                                    choices=c("Actual", "Percent of All Conversions"),
                                    selected="Actual",
                                    inline=TRUE
                                )
                            ),
                            bsTooltip(id='conversion_rates__cr_type',
                                      title="Show either the \\'actual\\' conversion rates at each snapshot, or show the \\'percent of all conversions\\' that are captured at each snapshot, relative to the \\'maximum days allowed to convert\\'.<br><br>A cohort will have a corresponding value only if all users within the cohort have had at least N days from their first visit to the site, where N is the number of days for the given snapshot, or the \\'Max Days to Convert\\' days.",
                                      placement='top', trigger='hover'),
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
                            bsTooltip(id='conversion_rates__max_days_to_convert',
                                      title="The \\'maximum days allowed to convert\\' relative to the user\\'s first visit to the site. Only conversions within the specified number of days will count towards the maximum conversion rate.",
                                      placement='top', trigger='hover')
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
    tags$div(id = "custom_progress_bar_placeholder")
))
