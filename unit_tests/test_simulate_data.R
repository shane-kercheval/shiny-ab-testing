library('testthat')
library(tidyverse)
library(lubridate)
library(scales)

source('../shiny-app/r_scripts/helpers.R', chdir=TRUE)
source('unit_test_helpers.R')

# to run from command line, use:
# library('testthat')
# test_file("test_simulate_data.R")


    # attribution_windows <- as.data.frame(read_csv('../shiny-app/simulated_data/attribution_windows.csv', row.names = FALSE))
    # website_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/website_traffic.csv', row.names = FALSE))
    # experiment_traffic <- as.data.frame(read_csv('../shiny-app/simulated_data/experiment_traffic.csv', row.names = FALSE))
    # conversion_rate_data <- as.data.frame(read_csv('../shiny-app/simulated_data/conversion_rate_data.csv', row.names = FALSE))

test_that("test_helpers: create", {

##########################################################################################################
# SIMULATE EXPERIMENT INFO
##########################################################################################################

    # NOTE: is_baseline can be a setting within the shiny app; but we'll set it here becuase that 
    # functionality is more suited towards a v2 than a v1
    # We could also include a description, etc., in this dataset.
    experiment_info <- as.data.frame(tribble(
        ~experiment_id,                             ~variation_name,      ~is_baseline,
        #------------------------------------------|--------------------|----
        "Redesign Website",                         "Original",          TRUE,
        "Redesign Website",                         "Site Redesign",     FALSE,
        "New Signup CTA Color",                     "Green Signup CTA",  TRUE,
        "New Signup CTA Color",                     "Blue Signup CTA",   FALSE,
        "Show Discount for First-Time Visitors",    "No Offer",          TRUE,
        "Show Discount for First-Time Visitors",    "Sales Offer",       FALSE,
        "Ask Additional Questions During Signup",   "Old Signup Path",   TRUE,
        "Ask Additional Questions During Signup",   "New Signup Path",   FALSE
    ))
    write.csv(experiment_info, file='../shiny-app/simulated_data/experiment_info.csv', row.names = FALSE)

##########################################################################################################
# SIMULATE ATTRIBUTION WINDOWS
##########################################################################################################

    # names of the metrics/conversions to track
    metrics_names <- c('Sign Up', 'Use Feature 1', 'Talk to Sales', 'Pay/Subscribe')
    
    # allowed number of days between seeing the experiment and giving credit for a conversion
    # e.g. signup should happen quick, but we may want to give people more time to subscribe/pay for the service,
    # since people want to try out the features before they decide to pay (e.g. if the service offers a trial or freemium)
    attribution_windows_days <- c(2, 3, 5, 7)
    attribution_windows <- data.frame(metric_id = metrics_names,
                                      attribution_windows = attribution_windows_days,
                                      stringsAsFactors = FALSE)
    attribution_windows <- expand.grid(experiment_id=unique(experiment_info$experiment_id),
                                       metric_id = metrics_names,
                                       stringsAsFactors = FALSE) %>%
        inner_join(attribution_windows, by = 'metric_id') %>%
        arrange(experiment_id, attribution_windows) %>%
        mutate_if(is.factor, as.character)

    write.csv(attribution_windows, file='../shiny-app/simulated_data/attribution_windows.csv', row.names = FALSE)

##########################################################################################################
# SIMULATE WEBSITE TRAFFIC
##########################################################################################################

    # unique daily visits e.g. 1 record means that the user_id may have visited the site 1+ times for that day
    # (the number of times does not matter)

    total_website_traffic <- 2000000
    set.seed(42)
    ids <- floor(runif(total_website_traffic, min=1, max=total_website_traffic * 0.8))

    # simulate dates over last e.g 6 months
    # we need to duplicate ids/days to simulate people visiting multiple paths per day; we'll use this information for our priors
    number_of_months <- 6

    website_traffic <- data.frame(user_id=ids)
    simulate_num_days <- number_of_months*30

    # offset from todays day (i.e. today - first_time_visit_index == first-time-visit)
    set.seed(42)
    # need to scale the output of the log(exponential) distribution to the number of days we are simulateing
    first_time_visit_offset <- round(scale_a_b(log2(rexp(length(unique(website_traffic$user_id)), rate=0.9) +1),
                                               a=0, 
                                               b=simulate_num_days))
    #hist(first_time_visit_offset)
    # create dataframe with the user-id and the offset that we will use from the current date
    # we'll use the current date so we can simulate the latest experiment is currently running 
    first_time_users <- data.frame(user_id=unique(ids), visit_offset=first_time_visit_offset)

    first_time_users <- first_time_users %>%
        mutate(first_visit_date=today() - visit_offset) %>%
        select(-visit_offset) %>%
        # we need to filter because the beginning of the simulated data has too few traffic and the there is a dip in the end
        filter(first_visit_date > Sys.Date() - round(simulate_num_days * 0.55),
               first_visit_date <= Sys.Date() - 7)
    
    website_traffic <- inner_join(website_traffic, first_time_users, by = 'user_id')
    # right now, we have a dataset with user-ids, that repeat for the number of unique days they have
    # visited the site, but the date field is the same for the days for given user-id
    # what we need to do now is simulate the user-id visiting the site on different days.
    website_traffic <- website_traffic %>%
        group_by(user_id) %>%
        mutate(visit_index = row_number()) %>%
        ungroup()

    # generate a new date based on the gamma distribution; note setting the seed based on the user-id,
    # which i wanted to do to recreate the same data, causes the distribution to become strange
    generated_website_traffic <- pmap(list(website_traffic$visit_index,
                              website_traffic$first_visit_date,
                              website_traffic$user_id),
                         function(x, y, z) generate_new_date(x, y, z))

    # pmap is losing the "date" and is returning a number, so we have to convert back; not sure this is the best way
    generated_website_traffic <- unlist(generated_website_traffic)
    website_traffic$visit_date <- as.Date(generated_website_traffic, origin = Sys.Date() - as.numeric(Sys.Date()))

    # NOTE: some of the dates generated will be the same. BUT, that could represent multiple different pages 
    # i.e. paths in the same day, so let's add path before we remove duplicates
    website_traffic <- website_traffic %>%
        select(user_id, visit_date) %>%
        arrange(user_id, visit_date)

    # duplicate various pages to simulate more page visits
    paths <- c(rep('example.com', 4),
               rep('example.com/features', 3), 
               rep('example.com/pricing', 2), 
               'example.com/demo')
    #set.seed(42)
    path_index <- ceiling(runif(nrow(website_traffic), min=0, max=length(paths)))
    website_traffic$path <- map_chr(path_index, ~ paths[.])
    
    # some of the dates generated will be the same. BUT, that could represent multiple different pages in the same day
    # so let's add path before we remove duplicates
    website_traffic <- distinct(website_traffic)
    
    website_traffic <- as.data.frame(website_traffic) %>%
    # we need to (RE)filter because the beginning of the simulated data has too few traffic and the there is a dip in the end
    filter(visit_date > Sys.Date() - round(simulate_num_days * 0.55),
           visit_date <= Sys.Date() - 7)

    ########
    # plot various characteristics of our simulated website traffic
    ########
    plot_object <- first_time_users %>%
        count(first_visit_date) %>%
        ggplot(aes(x=first_visit_date, y=n)) +
        geom_line() +
        expand_limits(y=0) +
        labs(title='Simulated Daily website_traffic') +
        scale_x_date(date_labels="%y-%m-%d",date_breaks  ="1 day") + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    plot_object %>% test_save_plot(file='data/simulate_data/simulated_daily_website_traffic.png')
    
    plot_object <- website_traffic %>%
        count(user_id) %>%  # gives a count of days visited for each user-id
        count(n) %>%  # gives a count user-ids for each number of unique website_traffic i.e. will give a distribution of unique (by day/path) website_traffic
        mutate(n=factor(n),
               percent_of_user_ids = nn / length(unique(website_traffic$user_id))) %>%
        rename(unique_days_visited=n,
               number_of_ids=nn) %>%
        ggplot(aes(x=unique_days_visited, y=percent_of_user_ids)) +
        geom_col() +
        geom_text(aes(label=percent(percent_of_user_ids)), vjust=-0.3) +
        geom_text(aes(label=number_of_ids), vjust=-1) +
        labs(title='Distribution of Unique Website Traffic Per User-Id')
    plot_object %>% test_save_plot(file='data/simulate_data/unique_website_traffic_per_user_id.png')

    plot_object <- website_traffic %>%
        count(path) %>%
        mutate(path=fct_reorder(path, desc(n))) %>%
        ggplot(aes(x=path, y=n)) +
        geom_col() +
        labs(title='Total website_traffic (day/user) single row represents >=1 Website Traffic for that day/user/path')
    plot_object %>% test_save_plot(file='data/simulate_data/count_of_traffic_to_paths.png')
    
    plot_object <- website_traffic %>%
        group_by(user_id) %>%
        filter(n() > 1) %>%
        summarise(t=round(sd(as.numeric(visit_date)))) %>%
        ggplot(aes(x=t)) +
            geom_histogram() +
            labs(title='Distribution of Number of Days between Website Traffic')
    plot_object %>% test_save_plot(file='data/simulate_data/distribution_of_num_days_between_traffic.png')
    
    temp <- inner_join(
        website_traffic %>% 
            group_by(user_id) %>%
            summarise(first_visit_date=min(visit_date)) %>%
            count(first_visit_date) %>%
            rename(date=first_visit_date,
                   new_visits=n),
        website_traffic %>%
            group_by(visit_date) %>%
            summarise(all_visits=n()) %>%
            rename(date=visit_date),
        by='date')
    
    plot_object <- temp %>% 
        gather(user_type, num_users, -date) %>%
        ggplot(aes(x=date, y=num_users, color=user_type)) +
            geom_line() +
            labs(title='New vs. All Visits Over time')
    plot_object %>% test_save_plot(file='data/simulate_data/new_vs_all_visits_over_time.png')
    
    
    plot_object <- temp %>%
        mutate(percent_new_visits = new_visits / all_visits) %>%
        ggplot(aes(x=date, y=percent_new_visits)) +
            geom_line() +
            expand_limits(y=0) +
            labs(title="Percent New users Over time")
    plot_object %>% test_save_plot(file='data/simulate_data/percent_new_visits_over_time.png')
    
    plot_object <- website_traffic %>%
        count(visit_date, path) %>%
        ggplot(aes(x=visit_date, y=n, color=path)) +
        geom_line() +
        labs(title='Website Traffic (per day/user) i.e. single row represents >=1 Website Traffic for that day/user/path')
    plot_object %>% test_save_plot(file='data/simulate_data/website_traffic_per_path.png')

    write.csv(website_traffic, file='../shiny-app/simulated_data/website_traffic.csv', row.names = FALSE)

##########################################################################################################
# SIMULATE EXPERIMENTS
# for each experiment, we want to simulate, from the current visits/traffic dataset, who saw the experiment,
# on which page, and which variation
##########################################################################################################

    experiment_names <- unique(experiment_info$experiment_id)
    min_traffic_date <- min(website_traffic$visit_date)
    #  max(website_traffic$visit_date) - min_traffic_date
    
    ##########################################################################################################
    # Experiment 1
    ##########################################################################################################
    # let's start the experiment 1 month after our visits dataset, and run it for a month
    min_traffic_date_offset <- 30
    experiment_duration <- 30
    start_date <- min_traffic_date + min_traffic_date_offset + 1
    end_date <- start_date + experiment_duration
    # all paths
    experiment_paths <- unique(paths)
    # experiment 1
    current_experiment_id <- experiment_names[1]
    variation_names <- (experiment_info %>% filter(experiment_id == current_experiment_id))$variation_name
    
    experiment_traffic_1 <- create_experiment_visits(website_traffic, start_date, end_date, experiment_paths,
                                                     current_experiment_id, variation_names)
    
    ##########################################################################################################
    # Experiment 2
    ##########################################################################################################
    min_traffic_date_offset <- 40
    experiment_duration <- 30
    start_date <- min_traffic_date + min_traffic_date_offset
    end_date <- start_date + experiment_duration
    experiment_paths <- c(#'example.com',
                          'example.com/features', 
                          #'example.com/pricing', 
                          'example.com/demo')
    current_experiment_id <- experiment_names[2]
    variation_names <- (experiment_info %>% filter(experiment_id == current_experiment_id))$variation_name
    
    experiment_traffic_2 <- create_experiment_visits(website_traffic, start_date, end_date, experiment_paths,
                                                     current_experiment_id, variation_names)

    
    ##########################################################################################################
    # Experiment 3
    ##########################################################################################################
    min_traffic_date_offset <- 60
    experiment_duration <- 25
    start_date <- min_traffic_date + min_traffic_date_offset
    end_date <- start_date + experiment_duration
    experiment_paths <- c(#'example.com',
                          #'example.com/features',
                          'example.com/pricing',
                          'example.com/demo')
    current_experiment_id <- experiment_names[3]
    variation_names <- (experiment_info %>% filter(experiment_id == current_experiment_id))$variation_name
    
    experiment_traffic_3 <- create_experiment_visits(website_traffic, start_date, end_date, experiment_paths,
                                                     current_experiment_id, variation_names)

    ##########################################################################################################
    # Experiment 4
    ##########################################################################################################
    min_traffic_date_offset <- 75
    experiment_duration <- 30
    start_date <- min_traffic_date + min_traffic_date_offset
    end_date <- start_date + experiment_duration
    experiment_paths <- c('example.com',
                          'example.com/features', 
                          'example.com/pricing', 
                          'example.com/demo')
    current_experiment_id <- experiment_names[4]
    variation_names <- (experiment_info %>% filter(experiment_id == current_experiment_id))$variation_name
    
    experiment_traffic_4 <- create_experiment_visits(website_traffic, start_date, end_date, experiment_paths,
                                                     current_experiment_id, variation_names)

    experiment_traffic <- rbind(experiment_traffic_1,
                                experiment_traffic_2,
                                experiment_traffic_3,
                                experiment_traffic_4)
    
    plot_object <- experiment_traffic %>%
        count(first_joined_experiment, experiment_id) %>%
        mutate(experiment_id = factor(experiment_id, levels=experiment_names)) %>%
        ggplot(aes(x=first_joined_experiment, y=experiment_id, color=experiment_id)) +
            geom_line(size=5) +
            scale_x_date(date_labels="%y-%m-%d",date_breaks  ="10 days") +
            theme(axis.text.x = element_text(angle = 30, hjust = 1, size=6),
                  legend.position="none")
    plot_object %>% test_save_plot(file='data/simulate_data/experiment_start_stop.png')

    write.csv(experiment_traffic, file='../shiny-app/simulated_data/experiment_traffic.csv', row.names = FALSE)
    ##########################################################################################################
    # SIMULATE ATTRIBUTION WINDOWS
    ##########################################################################################################

    historical_conversion_rates <- c(0.3, 0.2, 0.1, 0.08)

})



