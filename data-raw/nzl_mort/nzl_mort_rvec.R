
suppressPackageStartupMessages({
  library(bage)
  library(dplyr)
  library(poputils)
  library(command)
})


cmd_assign(.nzl_mort = "../data/nzl_mort.rda",
           .out = "../data/nzl_mort_rvec.rda")

load(.nzl_mort)
nzl_mort <- nzl_mort %>%
    mutate(age_old = age,
           age = reformat_age(age))

mod <- mod_pois(deaths ~ age * gender + year,
                data = nzl_mort,
                exposure = popn) %>%
    set_prior(age ~ RW2()) %>%
    set_prior(age:gender ~ SVD(HMD)) %>%
    fit()

nzl_mort_rvec <- mod %>%
    augment() %>%
    select(year, gender, age = age_old, mx = .fitted)
              
save(nzl_mort_rvec, file = .out, compress = "bzip2")
    



           
