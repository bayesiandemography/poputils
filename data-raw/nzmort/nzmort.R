
library(command)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(poputils)


cmd_assign(.popn = "nzmort/DPE403905_20230924_095611_86.csv",
           .dth = "nzmort/VSD349204_20230924_095835_74.csv",
           .out = "../data/nzmort.rda")


popn <- read_csv(file = .popn,
                 skip = 4,
                 col_names = c("age", "Male.2021", "Female.2021", "Male.2022", "Female.2022"),
                 col_types = "ciiii",
                 n_max = 96) %>%
    pivot_longer(cols = -age,
                 names_to = c("sex", "year"),
                 names_sep = "\\.",
                 names_transform = list(year = as.integer),
                 values_to = "popn") %>%
    mutate(age = combine_age(age, to = "lt"),
           age = reformat_age(age)) %>%
    count(age, sex, year, wt = popn, name = "popn")

    
deaths <- read_csv(file = .dth,
                   skip = 3,
                   col_names = c("age", "Male.2021", "Female.2021", "Male.2022", "Female.2022"),
                   col_types = "ciiii",
                   n_max = 22) %>%
    pivot_longer(cols = -age,
                 names_to = c("sex", "year"),
                 names_sep = "\\.",
                 names_transform = list(year = as.integer),
                 values_to = "deaths") %>%
    mutate(age = set_age_open(age, lower = 95)) %>%
    count(age, sex, year, wt = deaths, name = "deaths") %>%
    mutate(age_lab = if_else(age == "95+", "95 years and older", age),
           age = reformat_age(age))


nzmort <- left_join(deaths, popn, by = c("age", "sex", "year")) %>%
    arrange(age) %>%
    select(year, gender = sex, age = age_lab, deaths, popn) %>%
    mutate(age = factor(age, levels = unique(age))) %>%
    arrange(year, gender, age)


save(nzmort, file = .out, compress = "bzip2")
    



           
