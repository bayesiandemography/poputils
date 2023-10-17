
library(command)
library(demogR)
library(dplyr, warn.conflicts = FALSE)
library(poputils)

cmd_assign(.out = "../data/west_qx.rda")

qx_female <- (cdmltw(sex = "F")$nqx) %>%
    as.data.frame.table(stringsAsFactors = FALSE,
                        responseName = "qx") %>%
    mutate(sex = "Female")

qx_male <- (cdmltw(sex = "M")$nqx) %>%
    as.data.frame.table(stringsAsFactors = FALSE,
                        responseName = "qx") %>%
    mutate(sex = "Male")

west_qx <- bind_rows(qx_female, qx_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = reformat_age(age)) %>%
    mutate(level = as.integer(level)) %>%
    select(age, sex, level, qx) %>%
    arrange(level, sex, age) %>%
    as_tibble()

save(west_qx, file = .out, compress = "bzip2")

