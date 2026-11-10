
suppressPackageStartupMessages({
  library(readr)
  library(poputils)
  library(command)
})

cmd_assign(.table = "booth_standard/table_3.csv",
           .out = "../data/booth_standard_single.rda")


## Ys(x) at exact ages 11, ..., 50, from Booth, 1984, "Transforming
## Gompertz's Function for Fertility Analysis", Table 3
## (see booth_standard/README.md)
table_3 <- read_csv(.table, show_col_types = FALSE)

stopifnot(all(table_3$age_lower == 10:49),
          all(table_3$age_upper == 11:50))

Yx <- c(-Inf, table_3$Ys_upper)

## check transcription: differences in Ys match column 'standard_dY'
## (typed independently), to within the rounding of the table
dY <- diff(Yx)[2:39]
stopifnot(max(abs(dY - table_3$standard_dY[2:39])) < 1.5e-5)

value <- diff(exp(-exp(-Yx)))

age <- age_labels(type = "single",
                  min = 10,
                  max = 50)

booth_standard_single <- tibble::tibble(age = age,
                                        value = value)

## check consistency with five-year standard (Table 2)
value_five <- tapply(value, rep(1:8, each = 5), sum)
stopifnot(max(abs(value_five - booth_standard$value)) < 1e-12)

save(booth_standard_single,
     file = .out,
     compress = "bzip2")
