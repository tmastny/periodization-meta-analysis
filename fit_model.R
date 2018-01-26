## ---- library ----
library(tidyverse)
library(magrittr)
library(brms)
library(tidyverse)
library(rstan)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

## ---- read_factorized ----
d <- read_csv('factorized_periodization.csv')

## ---- intercept_model ----
study_model <- brm(
  effect_size | se(standard_error) ~ 1 + (1 | Number), data = d)

saveRDS(study_model, 'study_model.rds')

## ---- exercise_model ----
exercise_model <- brm(
  effect_size | se(standard_error) ~ 
  1 + (1 | Number) + (1 | outcome_type), data = d)

saveRDS(exercise_model, 'exercise_model.rds')

## ---- full_model ----
full_model <- brm(
  effect_size | se(standard_error) ~ 
  1 + (1 | Number) + (1 | outcome_type) + 
  (1 | periodized) + (1 | linear) + (1 | undulating) + (1 | block) +
  (1 | trained), data = d, iter = 8000, warmup = 4000, 
  control = list(adapt_delta = 0.96)) 

saveRDS(full_model, 'full_model.rds')

## ---- week_model ----
week_model <- brm(
  effect_size | se(standard_error) ~ 
    1 + weeks + (1 | Number) + (1 | outcome_type) + 
    (1 | periodized) + (1 | linear) + (1 | undulating) + (1 | block) +
    (1 | trained), data = d, iter = 8000, warmup = 4000, 
  control = list(adapt_delta = 0.96)) 

saveRDS(week_model, 'week_model.rds')


## ---- full_model_prior ----
full_model <- brm(
  effect_size | se(standard_error) ~ 
  1 + (1 | Number) + (1 | outcome_type) + (1 | periodized) + 
  (1 | linear) + (1 | undulating) + (1 | block) + (1 | trained), 
  prior = c(
    prior(normal(0.5, 4), class = Intercept)
  ),
  data = d, iter = 8000, warmup = 4000, 
  control = list(adapt_delta = 0.96)) 

saveRDS(full_model, 'full_model_prior.rds')



## ---- exclude_bryant_full ----
full_model <- brm(
  effect_size | se(standard_error) ~ 
    1 + (1 | Number) + (1 | outcome_type) + 
    (1 | periodized) + (1 | linear) + (1 | undulating) + (1 | block) +
    (1 | trained), data = filter(d, Number != 4), iter = 8000, warmup = 4000, 
  control = list(adapt_delta = 0.96)) 

saveRDS(full_model, 'full_model_exclude.rds')

## ---- exclude_bryant_week ----
week_model <- brm(
  effect_size | se(standard_error) ~ 
    1 + weeks + (1 | Number) + (1 | outcome_type) + 
    (1 | periodized) + (1 | linear) + (1 | undulating) + (1 | block) +
    (1 | trained), data = filter(d, Number != 4), iter = 8000, warmup = 4000, 
  control = list(adapt_delta = 0.96)) 

saveRDS(week_model, 'week_model_exclude.rds')



