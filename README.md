# shiny-ab-testing
A shiny app for analyzing AB tests


# Possible Modifications for Production Environment


- I only simulate dates, but a production environment would/should have date/time fields (e.g. the person might convert the hour before they enter the experiment, that's counted in my simulated data). I don't believe any change is necessarily, but this isn't tested.
- One of the datasets require is `experiment-traffic` which is used to calculate the bayesian prior dataset and prior alpha/beta. Rather than requiring that in the application, it could be calculated in a datamart, then the application would only require a much smaller `bayesian prior` dataset that was per experiment/metric.
