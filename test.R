library(tidyverse)
#library(lubridate)
#library(hrbrthemes)
library(highcharter)

state_abb_list <- c("VA", "MD", "DC")
county_fips_list <- c(
  51510, # VA: Alexandria City
  51013, # VA: Arlington County
  51059, # VA: Fairfax County
  24003, # MD: Anne Arundel County 
  24017, # MD: Charles
  24031, # MD: Montgomery
  24033, # MD: Prince George's
  11001  # DC
)

county_pop <- read_csv("co-est2020.csv") %>% 
  mutate(county_fips = paste0(STATE, COUNTY)) %>% 
  select(county_fips, pop = POPESTIMATE2020, county_name = CTYNAME)

county_patients <- read_csv("COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv") %>% 
  rename(
    adults_sum = previous_day_admission_adult_covid_confirmed_7_day_sum,
    kids_sum = previous_day_admission_pediatric_covid_confirmed_7_day_sum,
    adults_reports = previous_day_admission_adult_covid_confirmed_7_day_coverage,
    kids_reports = previous_day_admission_pediatric_covid_confirmed_7_day_coverage
  ) %>% 
  # assume censored values and missings = 0
  mutate(across(adults_sum|kids_sum, ~ if_else(.x == -999999 | is.na(.x), 0, .x))) %>%
  mutate(adults_avg = if_else(
    adults_reports > 0,
    adults_sum / adults_reports,
    0
  )) %>% 
  mutate(kids_avg = if_else(
    kids_reports > 0,
    kids_sum / kids_reports,
    0
  )) %>% 
  group_by(county_fips = fips_code, week = collection_week) %>% 
  summarize(
    admits = sum(adults_avg + kids_avg),
    state_abb = first(state)
  ) %>% 
  ungroup() %>% 
  left_join(county_pop, by = "county_fips")

theme_options <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank()
)

county_patients %>% 
  filter(state_abb %in% state_abb_list) %>% 
  group_by(state_abb, week) %>% 
  summarize(across(admits|pop, sum)) %>% 
  ungroup() %>% 
  mutate(rate = admits / pop * 100000) %>% 
  mutate(rate = round(rate, 2)) %>% 
  ungroup() %>% 
  hchart(
    "line", 
    hcaes(x = week, y = rate, group = state_abb),
    showInLegend = TRUE
  ) %>% 
  hc_tooltip(
    crosshairs = TRUE, 
    headerFormat = "<b>{series.name}</b><br>",
    pointFormat = "{point.x:%B %d, %Y}<br>{point.y:.2f}"
  ) %>% 
  hc_xAxis(title = FALSE) %>% 
  hc_yAxis(title = FALSE) %>% 
  hc_plotOptions(
    series = list(
      states = list(inactive = list(opacity = 1))
    )
  )


