library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(forcats)

kl_data_processed <- read_csv("pre/kl_data_processed.csv")
trial_level_processed_data <- read_csv("pre/trial_level_processed_data.csv")
highest_count <- read_csv("pre/highest_count.csv")

country <- read_csv("pre/country.csv")
country_by_subject <- read_csv("pre/country_by_subj.csv")

hc <- highest_count |>
  select(dataset_id = Experiment, subject_id = SID, hc = highest_count) |>
  group_by(dataset_id, subject_id) |>
  summarise(hc = max(hc)) |> # take each kid's max hc (only a few multiples)
  ungroup()
write_csv(hc, "post/hc.csv", na = "")

kld <- kl_data_processed |>
  left_join(country) |>
  left_join(country_by_subject, by = c("Experiment", "Subject")) |>
  unite("country", Country.x, Country.y, na.rm = TRUE) |>
  rename_with(str_to_lower) |>
  select(dataset_id = experiment, language, country, method,
         subject_id = subject, age = age_months, kl) |>
  mutate(method = method |>
           fct_collapse("non-titrated" = c("Non-titrated", "nontitrated"))) |>
  filter(!is.na(age)) |>
  group_by(dataset_id, language, country, method, subject_id, age) |>
  # filter(n() > 1)
  filter(n() == 1) |> # filter out kids with multiple KLs -- upstream problem!
  ungroup() |>
  mutate(kl = kl |> factor() |> fct_relabel(\(l) paste0(l, "-knower")), # add "-knower" to kl
         kl_subset = if_else(kl == "CP-knower", kl, "Subset-knower")) |> # code subset-knowers
  left_join(hc)
write_csv(kld, "post/kld.csv", na = "")

kls <- kld |> select(-hc)

td <- trial_level_processed_data |>
  left_join(country) |>
  left_join(country_by_subject, by = c("Experiment", "Subject")) |>
  unite("country", Country.x, Country.y, na.rm = TRUE) |>
  rename_with(str_to_lower) |>
  select(dataset_id = experiment, language, country, method,
         subject_id = subject, age = age_months, query, response) |>
  mutate(method = method |>
           fct_collapse("non-titrated" = c("Non-titrated", "nontitrated"))) |>
  left_join(kls)
write_csv(td, "post/td.csv", na = "")
