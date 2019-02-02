# need to scale the output of the log(exponential) distribution to the number of days we are simulateing
scale_a_b <- function(x, a, b) {

    (b - a) * ((x - min(x)) / (max(x) - min(x))) + a 
}

get_random_variation <- function(variation_names, user_id) {
    #set.seed(user_id)
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
