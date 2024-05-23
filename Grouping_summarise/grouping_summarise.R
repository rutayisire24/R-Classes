library(readxl)
library(tidyverse)

linelist_cleaned <- read_excel("data/linelist_cleaned.xlsx")
ll_by_outcome <- linelist %>% 
  group_by(outcome)

ll_by_outcome


# Unique groups
linelist %>% 
  group_by(outcome) %>% 
  tally()

linelist %>% 
  group_by(outcome, gender) %>% 
  tally()


# New columns
# group dat based on a binary column created *within* the group_by() command
linelist %>% 
  group_by(
    age_class = ifelse(age >= 18, "adult", "child")) %>% 
  tally(sort = T)


# Add/drop grouping columns
# Grouped by outcome
by_outcome <- linelist %>% 
  group_by(outcome)

# Add grouping by gender in addition
by_outcome_gender <- by_outcome %>% 
  group_by(gender, .add = TRUE)

# Un-group
linelist %>% 
  group_by(outcome, gender) %>% 
  tally() %>% 
  ungroup()

linelist %>% 
  group_by(outcome, gender) %>% 
  tally() %>% 
  ungroup(gender) # remove the grouping by gender, leave grouping by outcome


# Summarise

# summary statistics on ungrouped linelist
linelist %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years, na.rm=T),
    max_age  = max(age_years, na.rm=T),
    min_age  = min(age_years, na.rm=T),
    n_males  = sum(gender == "m", na.rm=T))

# summary statistics on grouped linelist
linelist %>% 
  group_by(outcome) %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years, na.rm=T),
    max_age  = max(age_years, na.rm=T),
    min_age  = min(age_years, na.rm=T),
    n_males    = sum(gender == "m", na.rm=T))

# Counts and tallies

linelist %>% 
  tally()




linelist %>% 
  group_by(outcome) %>% 
  tally(sort = TRUE)




linelist %>% 
  count(outcome)




linelist %>% 
  count(age_class = ifelse(age >= 18, "adult", "child"), sort = T)




linelist %>% 
  # produce counts by unique outcome-gender groups
  count(gender, hospital) %>% 
  # gather rows by gender (3) and count number of hospitals per gender (6)
  count(gender, name = "hospitals per gender" ) 



# Add counts

linelist %>% 
  as_tibble() %>%                   # convert to tibble for nicer printing 
  add_count(hospital) %>%           # add column n with counts by hospital
  select(hospital, n, everything()) # re-arrange for demo purposes

# Add totals

linelist %>%                                  # case linelist
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts of two columns
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions with column denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

# Grouping by date

# Linelist cases into days

daily_counts <- linelist %>% 
  drop_na(date_onset) %>%        # remove that were missing date_onset
  count(date_onset)              # count number of rows per unique date



daily_counts <- linelist %>% 
  drop_na(date_onset) %>%                 # remove case missing date_onset
  count(date_onset) %>%                   # count number of rows per unique date
  complete(                               # ensure all days appear even if no cases
    date_onset = seq.Date(                # re-define date colume as daily sequence of dates
      from = min(date_onset, na.rm=T), 
      to = max(date_onset, na.rm=T),
      by = "day"),
    fill = list(n = 0))                   # set new filled-in rows to display 0 in column n (not NA as default) 


# Linelist cases into weeks

# Make dataset of weekly case counts
weekly_counts <- linelist %>% 
  drop_na(date_onset) %>%                 # remove cases missing date_onset
  mutate(week = lubridate::floor_date(date_onset, unit = "week")) %>%  # new column of week of onset
  count(week) %>%                         # group data by week and count rows per group
  complete(                               # ensure all days appear even if no cases
    week = seq.Date(                      # re-define date colume as daily sequence of dates
      from = min(week, na.rm=T), 
      to = max(week, na.rm=T),
      by = "week"),
    fill = list(n = 0))                   # set new filled-in rows to display 0 in column n (not NA as default) 



# Linelist cases into months

# Make dataset of monthly case counts
monthly_counts <- linelist %>% 
  drop_na(date_onset) %>% 
  mutate(month = lubridate::floor_date(date_onset, unit = "months")) %>%  # new column, 1st of month of onset
  count(month) %>%                          # count cases by month
  complete(
    month = seq.Date(
      min(month, na.rm=T),     # include all months with no cases reported
      max(month, na.rm=T),
      by="month"),
    fill = list(n = 0))

# Filter on grouped data


# Slice rows per group

linelist %>%
  group_by(hospital) %>%
  arrange(hospital, date_hospitalisation) %>%
  slice_head(n = 5) %>% 
  arrange(hospital) %>%                            # for display
  select(case_id, hospital, date_hospitalisation)  # for display


# Filter on group size

linelist %>% 
  as_tibble() %>% 
  add_count(hospital) %>%          # add "number of rows admitted to same hospital as this row" 
  select(hospital, n, everything())

linelist %>% 
  add_count(hospital) %>% 
  filter(n < 500)

# Mutate on grouped data

linelist %>% 
  # group data by hospital (no change to linelist yet)
  group_by(hospital) %>% 
  
  # new columns
  mutate(
    # mean days to admission per hospital (rounded to 1 decimal)
    group_delay_admit = round(mean(days_onset_hosp, na.rm=T), 1),
    
    # difference between row's delay and mean delay at their hospital (rounded to 1 decimal)
    diff_to_group     = round(days_onset_hosp - group_delay_admit, 1)) %>%
  
  # select certain rows only - for demonstration/viewing purposes
  select(case_id, hospital, days_onset_hosp, group_delay_admit, diff_to_group)
