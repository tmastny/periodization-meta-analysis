
library(tidyverse)
library(magrittr)
d <- readxl::read_excel('Periodization Stuff.xlsx')

for (i in 1:nrow(d)) {
  if (is.na(d[i,1])) {
    d[i, c(1, 2, 3, 4, 5)] = d[i - 1, c(1, 2, 3, 4, 5)]
  }
}

# remove URL
d$`Other 1 pre` <- as.numeric(d$`Other 1 pre`)

d %<>%
  select(
    -`Measurements at 3+ time points?`, -Author, -`Study Title`,
    -`Participants (training status)`, -Age, -Sex, -`Length (weeks)`,
    -`Intensity Closest to 1RM test`, -`Volume Equated?`, -Issues)

# add in missing values
d %<>%
  gather(type, outcome, `LBM Pre`:`ES 1 vs. 3__4`) %>%
  mutate(type = str_to_lower(type)) %>%
  filter(
    (str_detect(type, ' pre') |
    str_detect(type, ' post') |
    str_detect(type, ' sd')) &
    not(str_detect(type, "/"))) %>%
    separate(
      type, c('outcome_type', 'outcome_status'), 
      ' (?=(pre$|post$|sd$))')

d <- d[complete.cases(d$outcome),]
d$outcome <- as.numeric(d$outcome)

d %<>% #add outcome_status to this grouping as well
  mutate_if(is.character, funs(factor(.))) %>%
  group_by(
    outcome_type, outcome_status, Number, `Program Label`, `Program Details`) %>%
  mutate(i = row_number()) %>%
  spread(outcome_status, outcome) %>%
  ungroup()


d %>%
  filter(Number == 1) %>%
  select(-`Program Details`)
