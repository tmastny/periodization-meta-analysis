## ---- read_models ----
full_model <- readRDS('full_model.rds')
full_model_exclude <- readRDS('full_model_exclude.rds')
full_model_prior <- readRDS('full_model_prior.rds')

## ---- read_cleaned_data
d <- read_csv('factorized_periodization.csv')

## ---- replicated_studies ----
replicated_studies <- d %>%
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
replicated_studies_exclude <- d %>%
  filter(Number != 4) %>%
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

## ---- mean_within_studies ----
pp_check(
  full_model, type = 'stat', stat = 'mean', 
  nsamples = 500)

## ---- mean_varying_studies ----
replicated_studies %>%
  mutate(effect_size = mean(d$effect_size)) %>%
  pp_check(
    full_model, type = 'stat', stat = 'mean', 
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

## ---- min_within_studies ----
pp_check(
  full_model, type = 'stat', stat = 'min', 
  nsamples = 500)

## ---- min_varying_studies ----
replicated_studies %>%
  mutate(effect_size = min(d$effect_size)) %>%
  pp_check(
    full_model, type = 'stat', stat = 'min', 
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

## ---- max_within_studies ----
pp_check(
  full_model, type = 'stat', stat = 'max', 
  nsamples = 500)

## ---- max_varying_studies ----
replicated_studies %>%
  mutate(effect_size = max(d$effect_size)) %>%
  pp_check(
    full_model, type = 'stat', stat = 'max', 
    nsamples = 500, allow_new_levels = TRUE, newdata = .)


## ---- prior_vs_exclusion ----
#' So when we replicate studies using the full group variation
#' we do see a level of regularization by including the errors
#' in a Bayesian multi-level model.
#' 
#' Another possible way to deal with the O'Bryant study is by
#' exclusion or prior information.


#' Interestingly, the expected max is still around 5,
#' but with fewer high value maximums. This actually gives me good confidence
#' in the non-excluded model, as it seems to discover the average max
#' while still including the O'Bryant study.
pp_check(
  full_model_exclude, type = 'stat', stat = 'max', 
  nsamples = 500)

replicated_studies_exclude %>%
  mutate(effect_size = max(d$effect_size[d$Number != 4])) %>%
  pp_check(
    full_model_exclude, type = 'stat', stat = 'max', 
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

#' With the minimum, again the expected value seems spot on, but with much more
#' variation towards the min and max minimum values.
pp_check(
  full_model_exclude, type = 'stat', stat = 'min', 
  nsamples = 500)

#' similar results with the mean
pp_check(
  full_model_exclude, type = 'stat', stat = 'mean', 
  nsamples = 500)

#' Next we can compare exclusion to including prior information.
pp_check(
  full_model_prior, type = 'stat', stat = 'max', 
  nsamples = 500)

replicated_studies %>%
  mutate(effect_size = max(d$effect_size)) %>%
  pp_check(
    full_model_prior, type = 'stat', stat = 'max', 
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

# mean
replicated_studies %>%
  mutate(effect_size = mean(d$effect_size)) %>%
  pp_check(
    full_model_prior, type = 'stat', stat = 'mean', 
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

## ---- linear_compared_prior ----
replicated_studies %>%
  mutate(effect_size = mean(d$effect_size)) %>%
  pp_check(
    full_model, type = 'stat_grouped', stat = 'mean',
    group = 'periodized', nsamples = 500, 
    allow_new_levels = TRUE, newdata = .)

replicated_studies %>%
  mutate(effect_size = mean(d$effect_size)) %>%
  pp_check(
    full_model_prior, type = 'stat_grouped', stat = 'mean',
    group = 'periodized', nsamples = 500, 
    allow_new_levels = TRUE, newdata = .)

