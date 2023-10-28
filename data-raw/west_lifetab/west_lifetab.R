
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

west_existing <- bind_rows(female, male) %>%
    select(level, sex, age, everything()) %>%
    mutate(age = factor(age, levels = unique(age))) %>%
    arrange(level, sex, age) %>%
    rename_with(starts_with("n"), .fn = function(x) sub("^n", "", x))


target <- data.frame(level = c(-2, -1, 0),
                     ex = c(12.5, 15, 17.5, 18 - 3 * 2.4, 18 - 2 * 2.4, 18 - 2.4),
                     sex = rep(c("Female", "Male"), each = 3))

standard <- west_existing %>%
    filter(level == 1) %>%
    select(sex, lx)
    
west_new <- ex_to_lifetab_brass(target = target,
                                standard = standard,
                                age = age_labels(type = "lt", max = 95),
                                infant = "CD",
                                child = "CD")

west_lifetab <- bind_rows(west_new, west_existing)

save(west_lifetab, file = .out, compress = "bzip2")

