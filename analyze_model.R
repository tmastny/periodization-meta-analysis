
## ---- intercept_plots ----
study_model <- readRDS('study_model.rds')

d %>%
  add_predicted_samples(study_model) %>%
  ggplot(aes(x = pred, y = Author, height = ..density..)) +
  ggridges::geom_density_ridges(
    stat = 'density', 
    rel_min_height = 0.01) + 
  geom_vline(xintercept = 0)

## ---- exercise_plots ----

exercise_model <- readRDS('exercise_model.rds')

# no explicit grouping
d %>%
  add_predicted_samples(exercise_model) %>%
  ggplot(
    aes(
      x = pred, y = Author, height = ..density..)) +
  ggridges::geom_density_ridges(
    stat = 'density', 
    rel_min_height = 0.01) + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~outcome_type)

## ---- full_model -----
full_model <- readRDS('full_model.rds')

# periodized vs. not
# I only want to see studies that look at periodized versus not
d %>%
  add_predicted_samples(full_model) %>%
  ggplot(
    aes(
      x = pred, y = Author, height = ..density..)) +
  ggridges::geom_density_ridges(
    stat = 'density', 
    rel_min_height = 0.01) + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~periodized)

# mean effect size
d %>%
  add_predicted_samples(full_model) %>%
  group_by(periodized) %>%
  summarise(mean = mean(effect_size))

# mean effect size without O'Bryant
d %>%
  add_predicted_samples(full_model) %>%
  filter(Author != 'O\'Bryant') %>%
  group_by(periodized) %>%
  summarise(mean = mean(effect_size))

d %>%
  add_predicted_samples(full_model) %>%
  filter(Author != 'O\'Bryant') %>%
  group_by(periodized, linear, undulating, block) %>%
  summarise(mean = mean(effect_size))

d %>%
  add_predicted_samples(full_model) %>%
  filter(Author != 'O\'Bryant') %>%
  group_by(trained, linear, undulating) %>%
  summarise(mean = mean(effect_size))


# ---- outcome_type ----
d %>%
  add_predicted_samples(full_model) %>%
  filter(Author != 'O\'Bryant') %>%
  group_by(outcome_type, periodized) %>%
  summarise(mean = mean(effect_size))

d %>%
  add_predicted_samples(full_model) %>%
  filter(Author != 'O\'Bryant') %>%
  group_by(outcome_type, linear, undulating) %>%
  summarise(mean = mean(effect_size)) %>%
  spread(undulating, mean)

# Trained lifters
d %>%
  ggplot() +
  geom_point(aes(x = pre, y = post, color = trained, shape = Sex)) + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~outcome_type + Sex)


# replicate studies and see average of 100 lb squat and bench
# EDA showing variation in untrained lifters



# Show the plot of effect size with standard errors
library(modelr)
d %>%
  filter(Author != 'O\'Bryant') %>%
  data_grid(
    outcome_type,
    standard_error = seq_range(standard_error, 100),
    .model = full_model) %>%
  add_predicted_samples(full_model, n = 500)


# to sample a new study I need to allow new levels to
# pull in total variation. And then I can assign the new study
# various parameters.

d %>%
  filter(Author != 'O\'Bryant') %>%
  data_grid(
    outcome_type,
    standard_error = seq_range(standard_error, 100)) %>%
  add_predicted_samples(full_model, n = 100)



## raw data
coef(full_model)

## ppc plots


pp_check(full_model, type = "stat", stat = 'mean')
pp_check(full_model, type = "stat", stat = 'max')

pp_check(
  full_model, type = 'stat_grouped', stat = 'max', nsamples = 500,
  group = 'periodized')

# Good plot to include, we don't see the random variation below average we would expect
# due to the statistical significant filter/file drawer problem
pp_check(
  full_model, type = 'stat_grouped', stat = 'min', nsamples = 500,
  group = 'periodized')

pp_check(
  full_model, type = 'stat_grouped', stat = 'mean', nsamples = 500,
  group = 'periodized')

pp_check(full_model, type = 'hist')


# again, don't see the expected minimum variation
pp_check(
  full_model, type = 'stat_grouped', stat = 'min', nsamples = 500,
  group = 'linear')

pp_check(
  full_model, type = 'stat_grouped', stat = 'min', nsamples = 500,
  group = 'undulating')

pp_check(
  full_model, type = 'stat_grouped', stat = 'min', nsamples = 500,
  group = 'trained')

pp_check(
  full_model, type = 'stat_grouped', stat = 'mean', nsamples = 500,
  group = 'trained') + xlim(c(0.75, 2.1))

# I want subgroup pp checks, i.e. I want to see the distribution of 'trained'
# means within periodization


pp_check(
  full_model, type = 'stat_grouped', stat = 'mean', nsamples = 500,
  group = 'trained', newdata = filter(d, periodized == 1)) + xlim(c(0.75, 2.1))

pp_check(
  full_model, type = 'stat_grouped', stat = 'mean', nsamples = 500,
  group = 'trained', newdata = filter(d, periodized == 0)) + xlim(c(0.25, 3.25))                 

# untrained response much better to unperiodized training,
# versus periodized
pp_check(
  full_model, type = 'stat_grouped', stat = 'mean', nsamples = 500,
  group = 'periodized', newdata = filter(d, trained == 0))  

d %>%
  filter(trained == 0) %>%
  add_predicted_samples(full_model) %>%
  group_by(periodized) %>%
  summarise(mean = mean(effect_size))


pp_check(
  full_model, type = 'stat_grouped', stat = 'mean', nsamples = 500,
  group = 'periodized', newdata = filter(d, trained == 1)) 

d %>%
  filter(trained == 1) %>%
  add_predicted_samples(full_model) %>%
  group_by(periodized) %>%
  summarise(mean = mean(effect_size))

## Let's look at more untrained people

pp_check(
  full_model, type = 'stat_grouped', stat = 'max', nsamples = 500,
  group = 'periodized', newdata = filter(d, trained == 0))

pp_check(
  full_model, type = 'stat_grouped', stat = 'min', nsamples = 500,
  group = 'periodized', newdata = filter(d, trained == 0)) 

d %>%
  filter(trained == 0, Author != 'O\'Bryant') %>%
  pp_check(
    full_model, type = 'stat_grouped', stat = 'mean', nsamples = 500,
    group = 'periodized', newdata = .) + xlim(c(0.5, 2))

## check Greg's result of trained people responding better to UP





# Comparsion 1: Model with all data
#               Model with all data, don't use study 4
#               Model with all data, don't use study 4, but allow all variation
full_model <- readRDS('full_model.rds')
pp_check(full_model, type = 'stat', stat = 'max', nsamples = 500)
pp_check(
  full_model, type = 'stat', stat = 'max', nsamples = 500, 
  newdata = filter(d, Number != 4))

pp_check(
  full_model, type = 'stat', stat = 'max', nsamples = 500, 
  newdata = filter(d, Number != 4))


# Comparsion 2: Model that excluded 4
#               Model that had regularizing prior
full_model_exclude <- readRDS('full_model_exclude.rds')
pp_check(
  full_model_exclude, type = 'stat', stat = 'max', nsamples = 500)

pp_check(
  full_model_exclude, type = 'stat', stat = 'mean', nsamples = 500)


# included prior
hist(rnorm(n = 500, mean = 0.5, sd = 3))
full_model_prior <- readRDS('full_model_prior.rds')
pp_check(
  full_model_prior, type = 'stat', stat = 'max', nsamples = 500)

pp_check(
  full_model_prior, type = 'stat', stat = 'mean', nsamples = 500)


## use pp_checks on new data
library(bayesplot)
d %>%
  data_grid(
    Number = 100, 
    periodized,
    outcome_type = 1,
    linear = 1,
    undulating = 0,
    block = 0,
    trained = 1,
    standard_error = mean(d$standard_error)
    ) %>%
  add_predicted_samples(
    full_model, 
    n = 500,
    allow_new_levels = TRUE) %>%
  group_by(.iteration) %>%
  summarise(mean = mean(pred)) %>%
  ggplot() +
  geom_histogram(aes(x = mean)) +
  geom_vline(xintercept = mean(d$effect_size))

d %>%
  data_grid(
    Number = 100, 
    periodized,
    outcome_type = 1,
    linear = 1,
    undulating = 0,
    block = 0,
    trained = 1,
    standard_error = mean(d$standard_error),
    effect_size = mean(effect_size)
  ) %>%
  pp_check(
    full_model, type = 'stat', stat = 'mean',
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

# change number to 100
d %>%
  mutate(Number = 100) %>%
  pp_check(
    full_model, type = 'stat', stat = 'mean', 
    nsamples = 500, allow_new_levels = TRUE, newdata = .)



## auto vs. manual ppc check
# I can the same results
pp_check(
  full_model, type = 'stat', stat = 'mean', 
  nsamples = 500)

d %>% 
  add_predicted_samples(
    full_model, 
    n = 500) %>%
  group_by(.iteration) %>%
  summarise(mean = mean(pred)) %>%
  ggplot() +
  geom_histogram(aes(x = mean)) +
  geom_vline(xintercept = mean(d$effect_size))
  

# How does the inclusion of varying SE change distribution?
# It doesn't appear to
d %>%
  data_grid(
    Number = 100, 
    periodized,
    outcome_type,
    linear,
    undulating,
    block,
    trained,
    standard_error = seq_range(standard_error, n = 50),
    effect_size = mean(effect_size)
  ) %>%
  pp_check(
    full_model, type = 'stat', stat = 'mean',
    nsamples = 500, allow_new_levels = TRUE, newdata = .)



# min models over group variation vs. within group
new_data <- d %>%
  data_grid(
    Number = 100, 
    periodized,
    outcome_type,
    linear,
    undulating,
    block,
    trained,
    standard_error = seq_range(standard_error, n = 50)
  )

new_data %>%
  mutate(effect_size = min(d$effect_size)) %>%
  pp_check(
    full_model, type = 'stat', stat = 'min',
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

pp_check(
  full_model, type = 'stat', stat = 'min',
  nsamples = 500, newdata = d)

# max
new_data %>%
  mutate(effect_size = max(d$effect_size)) %>%
  pp_check(
    full_model, type = 'stat', stat = 'max',
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

pp_check(
  full_model, type = 'stat', stat = 'max',
  nsamples = 500, newdata = d)













