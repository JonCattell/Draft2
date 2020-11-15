library(tidyverse)
library(knitr)

survey_answers <- c(NA, 3, 4, 4, 5, 2, 4, NA, 6, 3, 5, 4, 0, 5, 7, 5, NA, 5, 2, 4, NA, 3, 3, 5, NA)

# Question 1.1 

answers_no_NAs <- survey_answers[!is.na(survey_answers)]
length(which(answers_no_NAs == 1)) + length(which(answers_no_NAs == 7))

# Question 1.2

length(which(survey_answers == 5))

# Question 2.1

library(palmerpenguins)
penguins <- palmerpenguins::penguins

# Question 2.2

penguins %>%
  dplyr::select(species, island, bill_length_mm, body_mass_g) %>%
  dplyr::filter(species == "Gentoo") %>%
  dplyr::slice_max(body_mass_g , n = 10) %>%
  knitr::kable()
  
# Question 2.3

penguins %>%
  select(bill_length_mm, island) %>%
  dplyr::filter(!is.na(bill_length_mm)) %>%
  dplyr::group_by(island) %>%
  dplyr::summarise(
    avg_bill_length_per_island = mean(bill_length_mm)
  ) %>%
  dplyr::arrange(avg_bill_length_per_island) %>%
  knitr::kable()

# Question 2.4

penguins %>%
  dplyr::select(species, bill_length_mm, bill_depth_mm) %>%
  dplyr::filter(!is.na(bill_depth_mm)) %>%
  dplyr::filter(!is.na(bill_length_mm)) %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(
    min_bill_length_mm = min(bill_length_mm),
    max_bill_length_mm = max(bill_length_mm),
    med_bill_length_mm = median(bill_length_mm),
    min_bill_depth_mm = min(bill_depth_mm),
    max_bill_depth_mm = max(bill_depth_mm),
    med_bill_depth_mm = median(bill_depth_mm)
  ) %>%
  knitr::kable()

# Question 3.1

covid_data <-
  readr::read_csv("covid19_cases_20200301_20201017.csv")

# Question 3.2

# Could use fill() function for the NA but still tricky might have to miss it out

Southampton_complete_covid_data <- covid_data %>%
  dplyr::select(specimen_date, area_name, newCasesBySpecimenDate, cumCasesBySpecimenDate) %>%
  dplyr::filter(area_name == "Southampton") %>%
  dplyr::group_by(specimen_date) %>%
  dplyr::summarise(
    newCasesBySpecimenDate = replace_na(newCasesBySpecimenDate, 0),
    cumCasesBySpecimenDate = replace_na(cumCasesBySpecimenDate, 0)
  )

Southampton_complete_covid_data %>%
  knitr::kable()


# Question 3.3
library(lubridate)

Southampton_day_before <- Southampton_complete_covid_data %>%
  dplyr::mutate(
    day_to_match = (specimen_date + 1)
  ) %>%
  dplyr::rename(newCases_day_before = newCasesBySpecimenDate) %>%
  dplyr::summarise(
    specimen_date,
    day_to_match,
    newCases_day_before
  )

Southampton_day_before %>%
  knitr::kable()

combined <- dplyr::full_join(
  Southampton_day_before,
  Southampton_complete_covid_data,
  by = c("day_to_match" = "specimen_date")
)

combined %>%
  knitr::kable()

combined %>%
  dplyr::mutate(
    newCases_percentage = ((newCasesBySpecimenDate / newCases_day_before) * 100)
  ) %>%
  dplyr::summarise(
    specimen_date,
    day_to_match,
    newCasesBySpecimenDate,
    newCases_day_before,
    newCases_percentage = trunc(newCases_percentage), 
    newCases_percentage = na_if(na_if(newCases_percentage, "NaN"), "Inf")
  ) %>%
  knitr::kable()


# Question 4.1

population_data <-
  readr::read_csv("lad19_population.csv")

population_data %>%
  dplyr::select(lad19_area_name, area_population) %>%
  dplyr::filter(lad19_area_name %in% c("Brighton and Hove", "Portsmouth", "Southampton")) %>%
  knitr::kable()

covid_cases_population <- dplyr::full_join(
  covid_data,
  population_data,
  by = c("area_name" = "lad19_area_name")
)

covid_cases_population %>%
  knitr::kable()

covid_cases_population %>%
  dplyr::filter(area_name %in% c("Southampton", "Portsmouth", "Brighton and Hove")) %>%
  dplyr::summarise(
    specimen_date,
    area_name,
    newCasesBySpecimenDate,
    cumCasesBySpecimenDate,
    percentage_population_with_covid = ((cumCasesBySpecimenDate / area_population) * 100),
    percentage_population_with_covid = round(percentage_population_with_covid, digits = 3)
  ) %>%
  knitr::kable()

covid_cases_population %>%
  dplyr::filter(area_name %in% c("Southampton", "Portsmouth", "Brighton and Hove")) %>%
  dplyr::summarise(
    specimen_date,
    area_name,
    newCasesBySpecimenDate,
    cumCasesBySpecimenDate,
    covid_per_hun_thousand = ((cumCasesBySpecimenDate / area_population) * 100000),
    covid_per_hun_thousand = round(covid_per_hun_thousand, digits = 1)
  ) %>%
  knitr::kable()
