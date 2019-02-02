# need to scale the output of the log(exponential) distribution to the number of days we are simulateing
scale_a_b <- function(x, a, b) {

    (b - a) * ((x - min(x)) / (max(x) - min(x))) + a 
}

get_random_variation <- function(variation_names, user_id) {
    #set.seed(user_id)
    # rbinom will return random 0/1's, i need random 1/2's for the index
    return (variation_names[rbinom(1, 1, 0.5) + 1])
}

# for each experiment, we want to simulate, from the current visits/traffic dataset, who saw the experiment,
# on which page, and which variation
create_experiment_visits <- function(start_date, end_date, experiment_paths, experiment_id, variation_names) {
    experiment_visits <- visits %>%
        group_by(user_id) %>%
        mutate(visit_index=rank(visit_date, ties.method = "first")) %>%
        ungroup() %>%
        mutate(new_user = ifelse(visit_index == 1, TRUE, FALSE)) %>%
        filter(visit_date >= start_date & visit_date <= end_date & path %in% experiment_paths) %>%
        select(-visit_index)
    
    expected_num_users <- length(unique(experiment_visits$user_id))
    
    experiment_visits <- experiment_visits %>%
        group_by(user_id) %>%
        mutate(visit_index=rank(visit_date, ties.method = "first")) %>%
        ungroup() %>%
        filter(visit_index == 1) %>%
        select(-visit_index) %>%
        rename(first_joined_experiment=visit_date) %>%
        mutate(experiment_id=experiment_id)
    experiment_visits$variation <- map_chr(experiment_visits$user_id, ~ get_random_variation(variation_names, .))
    
    stopifnot(nrow(experiment_visits) == expected_num_users)
    # make sure no duplicated user_ids
    stopifnot(nrow(experiment_visits) == length(unique(experiment_visits$user_id)))
    
    print(experiment_visits %>%
        count(variation) %>%
        ggplot(aes(x=variation, y=n)) +
        geom_col())
    
    print(experiment_visits %>%
        count(first_joined_experiment) %>%
        ggplot(aes(x=first_joined_experiment, y=n)) +
        geom_line() +
        labs(title='Number of People Entering Experiment'))
    
    print(experiment_visits %>%
        count(first_joined_experiment, new_user) %>%
        ggplot(aes(x=first_joined_experiment, y=n, color=new_user)) +
        geom_line() +
        labs(title='Number of People Entering Experiment, shows affect of including new returning users in experiment'))
    
    print(experiment_visits %>%
        count(first_joined_experiment, variation) %>%
        ggplot(aes(x=first_joined_experiment, y=n, color=variation)) +
        geom_line())
    
    return (experiment_visits %>% select(-new_user))

}

create_experiment_visits <- function(website_traffic, start_date, end_date, experiment_paths, 
                                     current_experiment_id, variation_names,
                                     # baseline CRs are used to graph the percent of total sample size that
                                     # has been acheived over time, say at a 5% increase
                                     baseline_conversion_rates=c(0.03, 0.05, 0.07, 0.10)) {

    experiment_visits <- website_traffic %>%
        # i want to simulate data only including new users into the experiment
        # So, I have to determine new vs. returning before i filter on start date,
        # but to speed up the process, i can filter on end date first (then get the index and determine new)
        # note: the order in which i'm filtering means that even if the person hasn't been to any of the pages
        # in the particular experiment, they are still being filtered out.. They may or may not be the policy
        # implemented by the ab test team, but it's good enough for the simulation.
        filter(visit_date <= end_date) %>%
        group_by(user_id) %>%
        mutate(visit_index=rank(visit_date, ties.method = "first")) %>%
        ungroup() %>%
        filter(visit_index == 1) %>%
        filter(visit_date >= start_date,
               path %in% experiment_paths) %>%
        select(-visit_index)
    
    stopifnot(min(experiment_visits$visit_date) == start_date)
    stopifnot(max(experiment_visits$visit_date) == end_date)
    stopifnot(all(sort(unique(experiment_visits$path)) == sort(experiment_paths)))
    
    expected_num_users <- length(unique(experiment_visits$user_id))
    
    # NOTE: because of the way we ranked/filtered, experiment_visits (at this point) is unique per user_id
    # and the visit_date is there first time to the site and by definition the first_joined_experiment date
    stopifnot(expected_num_users == nrow(experiment_visits))  # make sure num user-ids is number of rows

    experiment_visits <- experiment_visits %>%
        rename(first_joined_experiment=visit_date) %>%
        mutate(experiment_id=current_experiment_id)
    experiment_visits$variation <- map_chr(experiment_visits$user_id, ~ get_random_variation(variation_names, .))
    
    stopifnot(nrow(experiment_visits) == expected_num_users)
    # make sure no duplicated user_ids
    stopifnot(nrow(experiment_visits) == length(unique(experiment_visits$user_id)))
    
    plot_object <- experiment_visits %>%
        count(variation) %>%
        ggplot(aes(x=variation, y=n, fill=variation)) +
        geom_col() +
        theme(legend.position = 'none') +
        labs(title=paste('Variation Count -', current_experiment_id),
             subtitle='All visitors in experiment are new to the site (i.e. first visit >= experiment start date)')
    plot_object %>% test_save_plot(file=paste0('data/simulate_data/create_', 
                                               current_experiment_id,
                                               '_variation_count.png'))
        
    
    sample_sizes_required <- map_dbl(baseline_conversion_rates,
                                     ~ calculate_total_sample_size(.,
                                                                   percent_increase=0.05,
                                                                   power=0.8,
                                                                   alpha=0.05))
    names(sample_sizes_required) <- paste0('baseline_', baseline_conversion_rates)
        
    experiment_count <- experiment_visits %>%
        count(first_joined_experiment) %>%
        rename(daily_traffic=n) %>%
        arrange(first_joined_experiment) %>%
        mutate(cumulative_traffic=cumsum(daily_traffic))
    
    # this calculates the percent of the cumulative traffic relative to the total sample size needed for the
    # corresponding baseline CR
    for(baseline_index in 1:length(sample_sizes_required)) {
        
        experiment_count[, names(sample_sizes_required)[baseline_index]] <- experiment_count$cumulative_traffic / sample_sizes_required[baseline_index]
    }

    plot_object <- experiment_count %>%
        ggplot(aes(x=first_joined_experiment, y=daily_traffic)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label=daily_traffic), check_overlap = TRUE, vjust=-0.5) +
        expand_limits(y=0)+
        labs(title='Number of People Entering Experiment',
             subtitle='All visitors in experiment are new to the site (i.e. first visit >= experiment start date)')
    plot_object %>% test_save_plot(file=paste0('data/simulate_data/create_', 
                                               current_experiment_id,
                                               '_experiment_trials_over_time.png'))
    
    plot_object <- experiment_visits %>%
        count(first_joined_experiment, variation) %>%
        ggplot(aes(x=first_joined_experiment, y=n, color=variation)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label=n), check_overlap = TRUE, vjust=-0.5) +
        expand_limits(y=0)+
        labs(title='Number of People Entering Experiment - By Variation',
             subtitle='All visitors in experiment are new to the site (i.e. first visit >= experiment start date)')
    plot_object %>% test_save_plot(file=paste0('data/simulate_data/create_', 
                                               current_experiment_id,
                                               '_experiment_trials_over_time_variation.png'))

    plot_object <- experiment_count %>%
        mutate(experiment_day=row_number()) %>%
        select(experiment_day, contains('baseline_')) %>%
        gather(baseline, value, -experiment_day) %>% 
        ggplot(aes(x=experiment_day, y=value,color=baseline)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label=percent(value)), check_overlap = TRUE, vjust=-0.5) +
        expand_limits(y=0)+
        scale_x_continuous(breaks=seq(1, 100)) +
        labs(title='Percent of Cumulative Traffic Attained for the Corresponding Sample Size')
    plot_object %>% test_save_plot(file=paste0('data/simulate_data/create_', 
                                               current_experiment_id,
                                               '_percent_cumulative_traffic_to_sample_size_over_time.png'))
    
    return (experiment_visits)
}

# generate a new date based on the gamma distribution; note setting the seed based on the user-id,
# which i wanted to do to recreate the same data, causes the distribution to become strange
generate_new_date <- function(visit_index, original_date, user_id) {
    
    # we don't wanto modify the first_visit_date 
    if(visit_index == 1) {

        return (original_date)
    }
    
    #set.seed(user_id)
    offset <- floor(rgamma(1, shape=visit_index + 3))
    return (original_date + max(offset, 1))
}
