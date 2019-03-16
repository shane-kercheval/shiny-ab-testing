# shiny-ab-testing
A shiny app for analyzing AB tests


# Possible Modifications for Production Environment



- The simulated experiment traffic (and the sample size calculator) assumes that returning visitors will be included in experiments.

- One of the datasets require is `experiment-traffic` which is used to calculate the bayesian prior dataset and prior alpha/beta. Rather than requiring that in the application, it could be calculated in a datamart, then the application would only require a much smaller `bayesian prior` dataset that was per experiment/metric.
