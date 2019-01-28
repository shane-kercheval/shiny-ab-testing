library(shiny)
library(tidyverse)
library(stringr)
library(scales)

#source('helper_scripts/plot_helpers.R')

shinyServer(function(session, input, output) {

   
    output$distPlot <- renderPlot({

        x <- 0:20
        y <- dbinom(x,
                    size=20,
                    prob=0.3)

        expected_value <- 20 * 0.3
        dataset <- data.frame(x, y)

        threshold <- 0.0001
        dataset <- dataset %>% filter(y > threshold & y < (1 - threshold))

        dataset %>%
            ggplot(aes(x, y)) +
            geom_bar(stat='identity') +
            geom_vline(xintercept=expected_value, linetype='dotted', color='red', size=1) +
            labs(title=paste0("Binomial Distribution (", 20, ":", 0.3, ")"),
                 subtitle=paste("Expected Value:", round(expected_value, 1)), 
                 x="Observations",
                 y="Probability")

    }, height = function() {

        session$clientData$output_distPlot_width * 0.66  # set height to % of width
    })
})
