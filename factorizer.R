# This goes beyond tidying. I am now making decisions what to include
# in the meta-analysis

## ---- library ----
library(tidyverse)
library(magrittr)

## ---- read_data ----
d <- read_csv('tidied_periodization.csv')

## ---- dichotomize_trained ----
d %<>%
  filter(str_detect(outcome_type, 'bench') | str_detect(outcome_type, 'squat')) %>%
  select(
    -`Study Title`, -Issues, -`Intensity Closest to 1RM test`, 
    -`Volume Equated?`)

# consistent with most of Greg's study exclusion
d %<>%
  filter(Number != 1) %>%   # small sd, used body weight indices
  filter(Number != 42)      # studied seniors

d %<>% 
  rename(weeks = `Length (weeks)`)

d %<>%
  mutate(`Participants (training status)` = str_to_lower(`Participants (training status)`)) %>%
  mutate(trained = if_else(
    str_detect(`Participants (training status)` , 'untrained') |
    str_detect(`Participants (training status)` , 'sedentary') |
    str_detect(`Participants (training status)` , 'not engaged in lifting for') |
    str_detect(`Participants (training status)` , 'rotc physical conditioning experience') |
    str_detect(`Participants (training status)` , 'female tennis players') |
    str_detect(`Participants (training status)` , 'female tennis players'), 0, 1)) %>%
  select(-`Participants (training status)`)

d %<>%
  mutate(periodized = if_else(str_detect(`Program Label`, 'NP'), 0, 1)) %>%
  mutate(linear = if_else(
    (periodized == 1) & str_detect(`Program Label`, 'LP'), 1, 0)) %>%
  mutate(undulating = if_else(
    (periodized == 1) & str_detect(`Program Label`, 'UP'), 1, 0)) %>%
  mutate(block = if_else(
    (periodized == 1) & str_detect(`Program Label`, 'BP'), 1, 0)) %>%
  select(-`Program Label`, -`Program Details`)

## ---- calculate_es ----
d %<>%
  mutate(effect_size = (post - pre)/sd) %>%
  mutate(standard_error = sqrt(2/N + effect_size^2/(4*N)))

write_csv(d, 'factorized_periodization.csv')
  









