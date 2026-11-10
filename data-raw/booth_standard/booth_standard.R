
suppressPackageStartupMessages({
  library(dplyr)
  library(poputils)
  library(command)
})

cmd_assign(.out = "../data/booth_standard.rda")


## Yx from Booth, 1984, "Transforming Gompertz's Function
## for Fertility Analysis", Table 2
Yx <- c(-Inf, -1.77306, -0.69130, 0.02564, 0.70000, 1.47872, 2.62602, 4.80970, Inf)

value <- diff(exp(-exp(-Yx)))

age <- age_labels(type = "five",
                  min = 10,
                  max = 50)

booth_standard <- tibble::tibble(age = age,
                                 value = value)

save(booth_standard,
     file = .out,
     compress = "bzip2")
