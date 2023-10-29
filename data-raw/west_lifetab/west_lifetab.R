
library(command)
library(demogR)
library(dplyr, warn.conflicts = FALSE)
library(poputils)

cmd_assign(.out = "../data/west_lifetab.rda")

nms <- c("nmx", "nax", "nqx", "lx", "ndx", "nLx", "ex")

female <- lapply(cdmltw(sex = "F")[nms], as.numeric)
female <- bind_cols(!!!female)
female$level <- rep(1:25, times = 21)
female$age <- rep(age_labels(type = "lt", max = 95), each = 25)
female$sex <- "Female"

male <- lapply(cdmltw(sex = "M")[nms], as.numeric)
male <- bind_cols(!!!male)
male$level <- rep(1:25, times = 21)
male$age <- rep(age_labels(type = "lt", max = 95), each = 25)
male$sex <- "Male"

west_lifetab <- bind_rows(female, male) %>%
    select(level, sex, age, everything()) %>%
    mutate(age = reformat_age(age)) %>%
    arrange(level, sex, age) %>%
    rename_with(starts_with("n"), .fn = function(x) sub("^n", "", x))

save(west_lifetab, file = .out, compress = "bzip2")


