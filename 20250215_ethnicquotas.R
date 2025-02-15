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

ni <- import("ethnicdata/DL2024-05-30_ni_2021 census_ethnicity.xlsx", which = "Table", skip = 3)
colnames(ni) <- tolower(colnames(ni))

ni <- ni %>%
  rename(ethnicity = 'ethnic group - 5 categories label') %>%
  rename(age = 'age - 86 categories code') %>%
  mutate(age = as.numeric(age)) %>%
  rename(values = count) %>%
  mutate(country = 'northern ireland') %>%
  select(c('country', 'ethnicity', 'age', 'values'))

#--------------------------------------------------------------------------------#

# CLEAN SCOTLAND DATA

sc <- import("ethnicdata/DL2024-05-30_scotland_2022 census_ethnicity.xlsx", skip = 11)
colnames(sc) <- tolower(colnames(sc))

## Transform data

sc <- sc %>%
  filter(sex == "All people", age != "Total") %>%
  select(-matches("total|all people")) %>%
  pivot_longer(cols = -c(sex, age),
               names_to = "ethnicity_detailed",
               values_to = "values") %>%
  mutate(country = 'scotland') %>%
  mutate(age = gsub("\\s+", "", age))

## Age transformation

sc <- sc %>%
  mutate(min_age_sc = case_when(
    age == "85andover" ~ 85,
    TRUE ~ as.numeric(str_extract(age, "^\\d+"))),
    max_age = case_when(
      age == "85andover" ~ 85,
      str_detect(age, "-") ~ as.numeric(str_extract(age, "\\d+$")),
      TRUE ~ min_age_sc)) %>%
  mutate(max_age_sc = case_when(
    age == "85andover" ~ 85,
    str_detect(age, "-") ~ as.numeric(str_extract(age, "\\d+$")),
    TRUE ~ min_age_sc))

## Ethnicity transformation

sc <- sc %>%
  mutate(ethnicity = case_when(
  str_detect(ethnicity_detailed, "^white") ~ 'white',
  str_detect(ethnicity_detailed, "^african") ~ 'black',
  str_detect(ethnicity_detailed, "^caribbean") ~ 'black',
  str_detect(ethnicity_detailed, "^mixed") ~ 'mixed',
  str_detect(ethnicity_detailed, "^asian") ~ 'asian',
  str_detect(ethnicity_detailed, "^other") ~ 'other',
  TRUE ~ "other"))

## Select variables

sc <- sc %>%
  select(c('country', 'ethnicity_detailed', 'ethnicity', 'min_age_sc', 'max_age_sc', 'values')) %>%
  glimpse()

#--------------------------------------------------------------------------------

# CLEAN ENGLAND AND WALES DATA

ew <- import("ethnicdata/DL20250108_ew_2021census_ethnicity+age(85).csv")
colnames(ew) <- tolower(colnames(ew))

ew <- ew %>%
  rename(country = 'countries') %>%
  rename(age = 'age (86 categories) code') %>%
  rename(values = 'observation') %>%
  rename(ethnicity_detailed = 'ethnic group (20 categories)') %>%
  filter(ethnicity_detailed != "Does not apply")
  

## Ethnicity transformation

ew <- ew %>%
  mutate(ethnicity = case_when(
  str_detect(ethnicity_detailed, "^White") ~ 'white',
  str_detect(ethnicity_detailed, "^Black") ~ 'black',
  str_detect(ethnicity_detailed, "^Mixed") ~ 'mixed',
  str_detect(ethnicity_detailed, "^Asian") ~ 'asian',
  str_detect(ethnicity_detailed, "^Other") ~ 'other',
  TRUE ~ "other"))

## Select variables

ew <- ew %>%
  select(c('country', 'ethnicity_detailed', 'ethnicity', 'age', 'values'))

#------------------------------------------------------------------------------#

# COMBINE ALL DATA

ethnicityxage <- bind_rows(ew, sc, ni)

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

ethnicityxage <- ethnicityxage %>%
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

ethnicity_order <- c("asian", "black", "mixed", "white",
                     "other")

ethnicityxage2$ethnicity <- factor(ethnicityxage2$ethnicity, levels = ethnicity_order)

ethnicityxage2 <- ethnicityxage%>%
  arrange(ethnicity) %>%
  rename(age_group = .data[[age_var]]) %>%
  filter(!is.na(age_group)) %>%
  mutate(uk = 'UK') %>%
  mutate(ethnicity = str_to_sentence(ethnicity)) %>%
  select(c('uk','country', 'ethnicity_detailed', 'ethnicity', 'age_group', 'values'))

#-------------------------------------------------------------------------------#

# CREATE PROPORTIONS

total <- ethnicityxage2 %>%
  group_by(.data[[geo_var]]) %>%
  summarise(total_pop = sum(values))

ethnicity <- ethnicityxage2 %>%
  group_by(.data[[geo_var]], ethnicity) %>%
  summarise(population = sum(values)) %>%
  mutate(total_pop = total$total_pop) %>%
  mutate(prop = population / total_pop)

ethnicity_detailed <- ethnicityxage2 %>%
  group_by(.data[[geo_var]], ethnicity_detailed) %>%
  summarise(population = sum(values)) %>%
  mutate(total_pop = total$total_pop) %>%
  mutate(prop = population / total_pop)

ethnicityxage2 <- ethnicityxage2 %>%
  group_by(.data[[geo_var]], ethnicity, age_group) %>%
  summarise(population = sum(values)) %>%
  mutate(total_pop = total$total_pop) %>%
  mutate(prop = population / total_pop)

#------------------------------------------------------------------------------#

# EXPORT

list_of_dfs <- list("Ethnicity" = ethnicity, "Ethnicity Detailed" = ethnicity_detailed,
                    "Ethnicity X Age" = ethnicityxage2)

if (highest_age == 85 ) {
  file_name <- paste0(total_geo, "_ethnicity_", threshold_age, "+","__top-age-break-", top_age_break,".xlsx")
} else {
  file_name <- paste0(total_geo, "_ethnicity_", threshold_age, "-", highest_age, "_top-age-break-", top_age_break,
                      ".xlsx")
}

## Write to Excel file
write_xlsx(list_of_dfs, path  <- paste0(folder,file_name))
