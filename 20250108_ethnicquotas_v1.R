
# UPDATE THESE

## Set the lowest age of the sample

threshold_age <- 18

## Set the highest age of the sample (for all above threshold, set ni to 85,
## scotland to 85, and eng_wales to 100. Scotland does not have ethnicity by single year
## so highest age needs to be highest in a 5 year range)

highest_age_ni <- 85

highest_age_eng_wales <- 100

highest_age_sc <- 85

## Set the target geography. Geography is either uk (for whole UK) or country (for ethnicity by devolved nation)

total_geo <- 'census_country'

## Set up geo_var

if (total_geo == 'uk') {
  geo_var <- 'uk'
} else if (total_geo == 'country') {
  geo_var <- 'country'
} else if (total_geo == 'gb') {
  geo_var <- 'gb'
} else if (total_geo == 'census_country') {
  geo_var <- 'census_country'
} 

# LIBRARY & DATA

library(rio)
library(tidyverse)
library(writexl)
library(janitor)

# CLEAN NORTHERN IRELAND DATA

ni <- import("ethnicdata/DL2024-05-30_ni_2021 census_ethnicity.xlsx", which = "Table", skip = 3)
colnames(ni) <- tolower(colnames(ni))

ni <- ni %>%
  rename(ethnicity = 'ethnic group - 5 categories label') %>%
  rename(age = 'age - 86 categories code') %>%
  rename(values = count) %>%
  mutate(country = 'northern ireland') %>%
  select(-c('ethnic group - 5 categories code', 'age - 86 categories label'))

## Create a string for age group condition based on threshold_age

first_age_group <- paste0("age %in% ", threshold_age, ":24 ~ \"", threshold_age, "-24\"")

age_range_ni <- paste0("age %in% ", threshold_age, ":", highest_age_ni, "~ \"", 
                     threshold_age, "-", highest_age_ni,"\"")

## Create age group variable

ni <- ni %>%
  mutate(age_group = case_when(
      eval(parse(text = first_age_group)), 
      age %in% 25:34 ~ "25-34", 
      age %in% 35:44 ~ "35-44",
      age %in% 45:54 ~ "45-54", 
      age %in% 55:64 ~ "55-64", 
      age %in% 65:85 ~ "65+",
      #age %in% 75:85 ~ "75+",
      TRUE ~ NA_character_)) %>%
  mutate(age_range = case_when(
    eval(parse(text = age_range_ni)),
    TRUE ~ NA_character_)) %>%
  filter(!is.na(age_range)) %>%
  mutate(ethnicity = tolower(ethnicity)) %>%
  select(c('country','ethnicity', 'age_group', 'values'))
  
  
# CLEAN SCOTLAND DATA

sc <- import("ethnicdata/DL2024-05-30_scotland_2022 census_ethnicity.xlsx", skip = 11)
colnames(sc) <- tolower(colnames(sc))

## Set the age group variable

if (threshold_age == '16') {
  age_var <- 'age_group_16'
} else if (threshold_age == '18') {
  age_var <- 'age_group_18'
}

## Set the age range variable to filter on 

if (threshold_age == '16' && highest_age_sc == '85') {
  age_range_sc <- 'age16plus'
} else if (threshold_age == '18' && highest_age_sc == '85') {
  age_range_sc <- 'age18plus'
} else if (threshold_age == '18' && highest_age_sc == '29') {
  age_range_sc <- 'age18_29'
} else if (threshold_age == '16' && highest_age_sc == '29') {
  age_range_sc <- 'age16_29'
} else if (threshold_age == '18' && highest_age_sc == '24') {
  age_range_sc <- 'age18_24'
}

sc$age <- as.character(sc$age)

agevalue <- unique(sc$age)
print(agevalue)

sc <- sc %>%
  filter(sex == "All people") %>%
  filter(age != "Total") %>%
  select(-'all people') %>%
  select(-matches("total"))

sc <- sc %>%
  pivot_longer(cols = `white: white scottish`:`other ethnic groups: other ethnic group`,
               names_to = "ethnicity_detailed",
               values_to = "values") %>%
  mutate(country = 'scotland') %>%
  mutate(age = gsub(" ","", age),
         age_group_16 = case_when(
           age %in% c('0-4','0-5','5-9', '10-14','15') ~ 'underage', 
           age %in% c('16-17', '18-19', '20-24') ~ '16-24',
           age %in% c('25-29', '30-34') ~ "25-34",
           age %in% c('35-39', '40-44') ~ "35-44",
           age %in% c('45-49', '50-54') ~ "45-54", 
           age %in% c('55-59', '60-64') ~ "55-64", 
           age %in% c('65-69', '70-74') ~ "65-74",
           age %in% c('75-79','80-84','85andover') ~ "75+",
           TRUE ~ "other"),
         age_group_18 = case_when(
           age %in% c('0-4','0-5','5-9','10-14','15', '16-17') ~ 'underage', 
           age %in% c('18-19', '20-24') ~ '18-24',
           age %in% c('25-29', '30-34') ~ "25-34",
           age %in% c('35-39', '40-44') ~ "35-44",
           age %in% c('45-49', '50-54') ~ "45-54", 
           age %in% c('55-59', '60-64') ~ "55-64", 
           age %in% c('65-69', '70-74') ~ "65-74",
           age %in% c('75-79','80-84','85andover') ~ "75+",
           TRUE ~ "other"),
         ethnicity = case_when(
           str_detect(ethnicity_detailed, "^white") ~ 'white',
           str_detect(ethnicity_detailed, "^african") ~ 'black',
           str_detect(ethnicity_detailed, "^caribbean") ~ 'black',
           str_detect(ethnicity_detailed, "^mixed") ~ 'mixed',
           str_detect(ethnicity_detailed, "^asian") ~ 'asian',
           str_detect(ethnicity_detailed, "^other") ~ 'other',
           TRUE ~ "other")) %>%
  mutate(age16plus = case_when(age %in% c('16-17', '18-19', '20-24', '25-29', '30-34',
                                          '35-39','40-44','45-49', '50-54','55-59', '60-64',
                                          '65-69', '70-74', '75-79','80-84','85andover') ~ '16+',
                               TRUE ~ NA_character_),
    age18plus = case_when(age %in% c('18-19', '20-24', '25-29', '30-34','35-39','40-44',
               '45-49', '50-54','55-59', '60-64','65-69', '70-74', '75-79','80-84',
               '85andover') ~ '18+',
    TRUE ~ NA_character_),
    age18_29 = case_when(age %in% c('18-19', '20-24', '25-29') ~ '18_29',
                         TRUE ~ NA_character_),
    age16_29 = case_when(age %in% c('16-17','18-19', '20-24', '25-29') ~ '18_29',
                         TRUE ~ NA_character_),
    age18_24 = case_when(age %in% c('16-17','18-19', '20-24') ~ '18_24',
                         TRUE ~ NA_character_))

sc <- sc %>%
  filter(!is.na(.data[[age_range_sc]])) %>%
  rename(age_group = .data[[age_var]]) %>%
  select(c('country', 'ethnicity_detailed','ethnicity', 'age_group', 'values'))

# CLEAN ENGLAND AND WALES DATA

ew <- import("ethnicdata/DL20250108_ew_2021census_ethnicity+age.csv")
colnames(ew) <- tolower(colnames(ew))

age_range_ew <- paste0("age %in% ", threshold_age, ":", highest_age_eng_wales, "~ \"", 
                       threshold_age, "-", highest_age_eng_wales,"\"")

ew <- ew %>%
  rename(country = 'countries') %>%
  rename(age = 'age (101 categories) code') %>%
  rename(values = 'observation') %>%
  rename(ethnicity_detailed = 'ethnic group (20 categories)') %>%
  filter(ethnicity_detailed != "Does not apply")

ew <- ew %>%
  mutate(age_group = case_when(
    eval(parse(text = first_age_group)), 
    age %in% 25:34 ~ "25-34", 
    age %in% 35:44 ~ "35-44",
    age %in% 45:54 ~ "45-54", 
    age %in% 55:64 ~ "55-64", 
    age %in% 65:74 ~ "65-74",
    age %in% 75:100 ~ "75+",
    TRUE ~ NA_character_),
  ethnicity = case_when(
    str_detect(ethnicity_detailed, "^White") ~ 'white',
    str_detect(ethnicity_detailed, "^Black") ~ 'black',
    str_detect(ethnicity_detailed, "^Mixed") ~ 'mixed',
    str_detect(ethnicity_detailed, "^Asian") ~ 'asian',
    str_detect(ethnicity_detailed, "^Other") ~ 'other',
    TRUE ~ "other")) %>%
  mutate(country = tolower(country)) %>%
  mutate(age_range = case_when(
    eval(parse(text = age_range_ew)),
    TRUE ~ NA_character_)) %>%
  filter(!is.na(age_range)) %>%
  select(c('country', 'ethnicity_detailed', 'ethnicity', 'age_group', 'values'))

# COMBINE DATASETS

ethnicityxage <- bind_rows(ew, sc, ni)

# CREATE UK VARIABLE

ethnicityxage <- ethnicityxage %>%
  mutate(uk = 'uk') %>%
  mutate(gb = case_when(
    country %in% c("england", "wales", "scotland") ~ "gb", 
    TRUE ~ NA_character_)) %>%
  mutate(census_country = case_when(
    country %in% c("england", "wales") ~ "england & wales",
    country %in% c("scotland") ~ "scotland",
    country %in% c("northern ireland") ~ "northern ireland",
    TRUE ~ NA_character_))

# CREATE PROPORTIONS

total <- ethnicityxage %>%
  group_by(.data[[geo_var]]) %>%
  summarise(total_pop = sum(values))

ethnicity <- ethnicityxage %>%
  left_join(total, by = geo_var) %>%
  group_by(.data[[geo_var]], ethnicity) %>%
  summarise(population = sum(values),
            total_pop = first(total_pop)) %>%
  mutate(prop = population / total_pop)

ethnicity_detailed <- ethnicityxage %>%
  left_join(total, by = geo_var) %>%
  group_by(.data[[geo_var]], ethnicity_detailed) %>%
  summarise(population = sum(values),
            total_pop = first(total_pop)) %>%
  mutate(prop = population / total_pop)

ethnicityxage <- ethnicityxage %>%
  left_join(total, by = geo_var) %>%
  group_by(.data[[geo_var]], ethnicity, age_group) %>%
  summarise(population = sum(values),
            total_pop = first(total_pop)) %>%
  mutate(prop = population / total_pop)

# REORDER ETHNICITY

ethnicity_order <- c("asian", "black", "mixed", "white",
                  "other")

ethnicity$ethnicity <- factor(ethnicity$ethnicity, levels = ethnicity_order)

ethnicity <- ethnicity %>%
  arrange(ethnicity)

# EXPORT

if (highest_age_eng_wales == 100) {
  file_name <- paste0("quotas/ethnicity_", geo_var, "_", threshold_age, "+", ".xlsx")
} else {
  file_name <- paste0("quotas/ethnicity_", geo_var, "_", threshold_age, "-", highest_age_eng_wales, ".xlsx")
}

write_xlsx(ethnicity, path = file_name)

write_xlsx(ethnicityxage, path <- paste0("quotas/ethnicityxage_", geo_var,"_", threshold_age, "+", ".xlsx"))

write_xlsx(ethnicity_detailed, path <- paste0("quotas/ethnicity_detailed_", geo_var,"_", threshold_age, "+", ".xlsx"))