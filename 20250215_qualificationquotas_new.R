# LIBRARY & DATA

library(rio)
library(tidyverse)
library(writexl)
library(janitor)

#--------------------------------------------------------------------------------#

# UPDATE THESE

## Set the lowest age of the sample

threshold_age <- 16

## Set the highest age of the sample. For all over the threshold age, set highest_age to 85.

highest_age <- 85

top_age_break <- '65+'

## Set the target geography. Geography is either uk (for whole UK) or country (for ethnicity by devolved nation)

total_geo <- 'uk'

## Set up geo_var

geo_var <- case_when(
  total_geo == 'country' ~ 'country',
  TRUE ~ 'uk'  # default fallback
)

## Folder

folder <- "quotas/"

#--------------------------------------------------------------------------------#

# CLEAN NORTHERN IRELAND DATA

ni <- import("qualificationdata/20250214_ni_census2021_qualification8_age85+.csv")
colnames(ni) <- tolower(colnames(ni))

ni <- ni %>%
  rename(qualification_code = 'qualifications (highest level) code') %>%
  rename(qualification_detailed = 'qualifications (highest level) label') %>%
  rename(age = 'age - 86 categories code') %>%
  rename(values = 'count') %>%
  mutate(country = 'northern ireland')

ni <- ni %>%
  mutate(qualification = case_when(
    qualification_code %in% 0 ~ "no qualifications",
    qualification_code %in% c(1,2,4,6) ~ "secondary school",
    qualification_code %in% 3 ~ "apprenticeship",
    qualification_code %in% 5 ~ "degree",
    TRUE ~ NA_character_)) %>%
  select(c('country', 'qualification_detailed', 'qualification', 'age', 'values'))

#--------------------------------------------------------------------------------#

# CLEAN SCOTLAND DATA

sc <- import("qualificationdata/20241112_scotland_2022 census_highestqualification.xlsx", skip = 11)
colnames(sc) <- tolower(colnames(sc))

## Transform data

sc <- sc %>%
  filter(sex == "All", age != "Total") %>%
  select(-matches("all people")) %>%
  pivot_longer(cols = -c(sex, age),
               names_to = "qualification_detailed",
               values_to = "values") %>%
  mutate(country = 'scotland') %>%
  mutate(age = gsub("\\s+", "", age))

## Age transformation

sc <- sc %>%
  mutate(min_age_sc = case_when(
    age == "85andover" ~ 85,
    TRUE ~ as.numeric(str_extract(age, "^\\d+")))) %>%
  mutate(max_age_sc = case_when(
    age == "85andover" ~ 85,
    str_detect(age, "to") ~ as.numeric(str_extract(age, "\\d+$")),
    TRUE ~ min_age_sc))

## Qualification transformation

sc <- sc %>%
  mutate(qualification = case_when(
    qualification_detailed %in% c('no qualifications') ~ 'no qualifications',
    qualification_detailed %in% c('lower school qualifications', 
                                  'upper school qualifications') ~ 'secondary school',
    qualification_detailed %in% c('apprenticeship qualifications') ~ 'apprenticeship',
    qualification_detailed %in% c('further education and sub-degree higher education qualifications incl. hnc/hnds',
                                  'degree level qualifications or above') ~ 'degree',
    TRUE ~ NA_character_))

## Select variables

sc <- sc %>%
  select(c('country', 'qualification_detailed','qualification', 'min_age_sc', 'max_age_sc', 'values'))

#--------------------------------------------------------------------------------

# CLEAN ENGLAND AND WALES DATA

ew <- import("qualificationdata/20250214_ew_census2021_qualification8_age85+.csv")
colnames(ew) <- tolower(colnames(ew))

ew <- ew %>%
  rename(country = 'countries') %>%
  rename(age = 'age (86 categories) code') %>%
  rename(values = 'observation') %>%
  rename(qualification_detailed = 'highest level of qualification (8 categories)') %>%
  rename(qualification_code = 'highest level of qualification (8 categories) code') %>%
  filter(qualification_detailed != "Does not apply")


## Qualification transformation

ew <- ew %>%
  mutate(qualification = case_when(
    qualification_code %in% 0 ~ "no qualifications",
    qualification_code %in% c(1,2,4,6) ~ "secondary school",
    qualification_code %in% 3 ~ "apprenticeship",
    qualification_code %in% 5 ~ "degree",
    TRUE ~ NA_character_)) %>%
  select(c('country', 'qualification_detailed', 'qualification', 'age', 'values'))

#------------------------------------------------------------------------------#

# COMBINE ALL DATA

censusxage <- bind_rows(ew, sc, ni)

#------------------------------------------------------------------------------#

# AGE GROUP VARIABLES

## Set up age_var

age_var <- case_when(
  threshold_age %in% c(16,18) & highest_age == 85 & top_age_break == '65+' ~ 'age_group_standard',
  threshold_age %in% c(16,18) & highest_age == 24 & top_age_break == '20-24' ~ 'age_group_youth',
  threshold_age == 18 & highest_age == 85 & top_age_break == '80+' ~ 'age_group_highest80plus',
  threshold_age == 16 & highest_age == 75 ~ 'age_group_homeoffice',
  threshold_age == 18 & highest_age == 85 & top_age_break == '75+' ~ 'age_group_highest75plus',
  TRUE ~ 'age_group_standard'  # default fallback
)

## Calculate age_group variables

censusxage <- censusxage %>%
  mutate(age_group_standard = case_when(
    between(age,threshold_age, 24) ~ paste0(threshold_age, "-24"),
    min_age_sc >= threshold_age & max_age_sc <= 24 ~ paste0(threshold_age, "-24"),
    between(age, 25, 34) ~ "25-34",
    min_age_sc >= 25 & max_age_sc <= 34 ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    min_age_sc >= 35 & max_age_sc <= 44 ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    min_age_sc >= 45 & max_age_sc <= 54 ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    min_age_sc >= 55 & max_age_sc <= 64 ~ "55-64",
    between(age, 65, 85) ~ "65+",
    min_age_sc >= 65 & max_age_sc <= 85 ~ "65+",
    TRUE ~ NA_character_)) %>%
  mutate(age_group_highest75plus = case_when(
    between(age,threshold_age, 24) ~ paste0(threshold_age, "-24"),
    min_age_sc >= threshold_age & max_age_sc <= 24 ~ paste0(threshold_age, "-24"),
    between(age, 25, 34) ~ "25-34",
    min_age_sc >= 25 & max_age_sc <= 34 ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    min_age_sc >= 35 & max_age_sc <= 44 ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    min_age_sc >= 45 & max_age_sc <= 54 ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    min_age_sc >= 55 & max_age_sc <= 64 ~ "55-64",
    between(age, 65, 74) ~ "65-74",
    min_age_sc >= 65 & max_age_sc <= 74 ~ "65-74",
    between(age, 75, 85) ~ "75+",
    min_age_sc >= 75 & max_age_sc <= 85 ~ "75+",
    TRUE ~ NA_character_)) %>%
  mutate(age_group_homeoffice = case_when(
    between(age,threshold_age, 24) ~ paste0(threshold_age, "-24"),
    min_age_sc >= threshold_age & max_age_sc <= 24 ~ paste0(threshold_age, "-24"),
    between(age, 25, 34) ~ "25-34",
    min_age_sc >= 25 & max_age_sc <= 34 ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    min_age_sc >= 35 & max_age_sc <= 44 ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    min_age_sc >= 45 & max_age_sc <= 54 ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    min_age_sc >= 55 & max_age_sc <= 64 ~ "55-64",
    between(age, 65, 75) ~ "65-75",
    min_age_sc >= 65 & max_age_sc <= 74 ~ "65-75",
    TRUE ~ NA_character_))

qualification_order <- c("no qualifications", "secondary school", "apprenticeship", 
                         "degree")

censusxage$qualification <- factor(censusxage$qualification, levels = qualification_order)

qualificationxage <- censusxage%>%
  arrange(qualification) %>%
  rename(age_group = .data[[age_var]]) %>%
  filter(!is.na(age_group)) %>%
  mutate(uk = 'UK') %>%
  mutate(qualification = str_to_sentence(qualification)) %>%
  select(c('uk','country', 'qualification_detailed', 'qualification', 'age_group', 'values'))

#-------------------------------------------------------------------------------#

# CREATE PROPORTIONS

total <- qualificationxage %>%
  group_by(.data[[geo_var]]) %>%
  summarise(total_pop = sum(values))

qualification <- qualificationxage %>%
  group_by(.data[[geo_var]], qualification) %>%
  summarise(population = sum(values), .groups = 'drop') %>%
  mutate(total_pop = total$total_pop) %>%
  mutate(prop = population / total_pop)

qualification_detailed <- qualificationxage %>%
  group_by(.data[[geo_var]], qualification_detailed) %>%
  summarise(population = sum(values), .groups = 'drop') %>%
  mutate(total_pop = total$total_pop) %>%
  mutate(prop = population / total_pop)

qualificationxage <- qualificationxage %>%
  group_by(.data[[geo_var]], qualification, age_group) %>%
  summarise(population = sum(values), .groups = 'drop') %>%
  mutate(total_pop = total$total_pop) %>%
  mutate(prop = population / total_pop)

#------------------------------------------------------------------------------#

# EXPORT

list_of_dfs <- list("Qualification" = qualification, 
                    "Qualification Detailed" = qualification_detailed,
                    "Qualification X Age" = qualificationxage)

if (highest_age == 85 ) {
  file_name <- paste0(total_geo, "_qualification_", threshold_age, "+","__top-age-break-", top_age_break,".xlsx")
} else {
  file_name <- paste0(total_geo, "_qualification_", threshold_age, "-", highest_age, "_top-age-break-", top_age_break,
                      ".xlsx")
}

## Write to Excel file
write_xlsx(list_of_dfs, path  <- paste0(folder,file_name))
