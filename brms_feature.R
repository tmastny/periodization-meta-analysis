
# is there a better way to include raw data in pp_check?
library(rstanarm)
library(brms)
library(bayesplot)
library(modelr)

stan_mod <- stan_lm(mpg ~ wt + hp + am, data = mtcars, prior = R2(0.75))
newdata = modelr::data_grid(
  mtcars, wt = seq_range(wt, n = 50), hp = seq_range(hp, n = 50), am)

y <- mtcars$mpg
yrep <- posterior_predict(stan_mod, newdata = newdata, draws = 500)
ppc_stat(y, yrep[,1:32], stat = 'mean')

d %>%
  data_grid(
    Number = 100, 
    periodized,
    outcome_type = 1,
    linear = 1,
    undulating = 0,
    block = 0,
    trained = 1,
    standard_error = seq_range(standard_error, n = 50),
    effect_size = mean(effect_size)
  ) %>%
  pp_check(
    full_model, type = 'stat', stat = 'mean',
    nsamples = 500, allow_new_levels = TRUE, newdata = .)

