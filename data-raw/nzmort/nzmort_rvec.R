
library(command)
library(bage)
library(dplyr, warn.conflicts = FALSE)
library(poputils)


cmd_assign(.nzmort = "../data/nzmort.rda",
           .out = "../data/nzmort_rvec.rda")

load(.nzmort)
nzmort <- nzmort %>%
    mutate(age_old = age,
           age = reformat_age(age))

mod <- mod_pois(deaths ~ age * gender + year,
                data = nzmort,
                exposure = popn) %>%
    set_prior(age ~ RW2()) %>%
    set_prior(age:gender ~ SVD(HMD)) %>%
    fit()

nzmort_rvec <- mod %>%
    augment() %>%
    select(year, gender, age = age_old, mx = .fitted)
              
save(nzmort_rvec, file = .out, compress = "bzip2")
    



           
