## About

[https://github.com/shane-kercheval/shiny-ab-testing](https://github.com/shane-kercheval/shiny-ab-testing)

This shiny app is an example of an A/B test dashboard that uses both frequentist and bayesian methods.

The underlying data the app uses is simulated to be "realistic", such that production-level data could be swapped out and used by the app.

Additionally, the underlying code/functions/logic is unit-tested and is designed to be extendable and maintainable.

This app uses "attribution windows" for all metrics, which means it only counts conversions toward each metric within a certain amount of days from the time the user joins the experiment. Here is an [exploratory blog-post](https://github.com/shane-kercheval/misc-projects/blob/master/attribution-window-effects/attribution-window-simulation.md) that shows the possible effects of using (or not using) attribution windows.
