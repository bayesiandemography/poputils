
library(command)
library(demogR)
library(dplyr, warn.conflicts = FALSE)
library(poputils)

cmd_assign(.out = "../data/west_mx.rda")

mx_female <- (cdmltw(sex = "F")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE,
                        responseName = "mx") %>%
    mutate(sex = "Female")

mx_male <- (cdmltw(sex = "M")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE,
                        responseName = "mx") %>%
    mutate(sex = "Male")

west_mx <- bind_rows(mx_female, mx_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = reformat_age(age)) %>%
    mutate(level = as.integer(level)) %>%
    select(age, sex, level, mx) %>%
    arrange(level, sex, age) %>%
    as_tibble()

save(west_mx, file = .out, compress = "bzip2")

