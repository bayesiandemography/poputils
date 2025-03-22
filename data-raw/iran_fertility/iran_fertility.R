
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(command)
})

cmd_assign(.rural = "iran_fertility/table_4.1.csv",
           .urban = "iran_fertility/table_4.2.csv",
           .out = "../data/iran_fertility.rda")

rural <- read_csv(.rural, show_col_types = FALSE) |>
  rename(time = Year) |>
  select(-TFR) |>
  pivot_longer(-time, names_to = "age", values_to = "rate") |>
  mutate(area = "Rural")

urban <- read_csv(.urban, show_col_types = FALSE) |>
  rename(time = Year) |>
  select(-TFR) |>
  pivot_longer(-time, names_to = "age", values_to = "rate") |>
  mutate(area = "Urban")

iran_fertility <- bind_rows(rural, urban)

save(iran_fertility, file = .out, compress = "bzip2")





  
