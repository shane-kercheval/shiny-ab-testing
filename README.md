# shiny-ab-testing

A shiny app for analyzing AB tests

---

This shiny app is an example of an A/B test dashboard that uses both frequentist and bayesian methods.

The underlying data it uses is simulated to be "realistic", such that it could process production-level data.

Additionally, the underlying code/functions/logic is unit-tested and is designed to be extendable and maintainable.

This app uses "attribution windows" for all metrics, which means it only counts conversions toward each metric within a certain amount of days from the time the user joins the experiment. Here is an [exploratory blog-post](https://github.com/shane-kercheval/misc-projects/blob/master/attribution-window-effects/attribution-window-simulation.md) that shows the possible effects of using (or not using) attribution windows.

# Possible Modifications for Production Environment

- The simulated experiment traffic (and the sample size calculator) assumes that returning visitors will be included in experiments.

- One of the datasets require is `experiment-traffic` which is used to calculate the bayesian prior dataset and prior alpha/beta. Rather than requiring that in the application, it could be calculated in a datamart, then the application would only require a much smaller `bayesian prior` dataset that was per experiment/metric.
