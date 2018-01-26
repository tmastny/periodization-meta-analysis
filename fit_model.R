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

## ---- calculate_es ----
d %<>%
  mutate(effect_size = (post - pre)/sd) %>%
  mutate(standard_error = sqrt(2/N + effect_size^2/(4*N)))

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





