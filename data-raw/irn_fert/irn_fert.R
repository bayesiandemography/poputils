
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(command)
})

cmd_assign(.rural = "irn_fert/table_4.1.csv",
           .urban = "irn_fert/table_4.2.csv",
           .out = "../data/irn_fert.rda")

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

irn_fert <- bind_rows(rural, urban)

save(irn_fert, file = .out, compress = "bzip2")





  
