
# poputils 0.7.0

## New features

- New functions `collapse_dims()` and `collapse_levels()` aggregate
  data frames. `collapse_dims()` removes whole dimensions, such as
  region or sex. `collapse_levels()` merges levels within dimensions,
  such as regions or age groups, using a named list, a lookup table,
  or a function such as `combine_age()`. Counts are summed. Rates,
  probabilities, means, and ratios are combined using a weight `wt`:
  weighted means over dimensions where the weight is partitioned, and
  sums over dimensions where the weight is shared, such as cause of
  death or destination of migrants (`wt_shared`). All measures can be
  rvecs.

## Bug fixes

- `lifetab()` and `lifeexp()` with the `"constant"` method no longer
  return `NaN` when a closed age group has a mortality rate of `Inf`
  or a probability of dying of 1. Everyone in that age group now dies
  at the start of the interval, so `Lx` is 0, and life expectancy at
  earlier ages is calculated as usual. `q0_to_m0()` with
  `infant = "constant"` now returns `Inf`, rather than `NaN`, when
  `q0` is 1.
- `lifetab()` and `lifeexp()` with the `"constant"` method are now
  accurate for very small mortality rates and probabilities of dying,
  such as probabilities below about 1e-11.
- `lifetab()` now removes existing life table columns so that every
  life table quantity in the result comes from the current
  calculation. With no `suffix`, existing `mx`, `qx`, `lx`, `dx`,
  `Lx`, and `ex` columns are removed, and an existing `ax` column is
  removed unless `ax` was supplied. A column that is the mortality
  input, or a `by` or grouping variable, is kept. With a `suffix`,
  only the suffixed columns are replaced, and the original `mx` and
  `qx` columns stay. `lifeexp()` continues to replace only `ex`.
  `lifetab()`, `lifeexp()`, and `tfr()` also read columns after those
  removals, so later columns are no longer taken from the wrong place.
- `reformat_age()` now labels numeric life table ages from their own
  lower limits when the series has a gap. For example, in
  `c(0, 1, 5, ..., 50, 60)`, 60 is labelled `"60+"` rather than
  `"55-59"`.
- `tfr_to_asfr_scale()` now scales every standard rate by the same
  factor, `tfr / sum(value * width)`. It gives an error when the
  standard includes an open age group.
- `check_age(unique = TRUE)` now treats different spellings of the
  same age group as duplicates. For example, `"0"` and `"infant"`
  are reported as the same age group. The message names both
  spellings. `lifetab()` and `tfr()` use this check.
- `lifeexp()` with `child = "CD"` now uses the age-0 mortality rate
  in the Coale-Demeny formula for ages 1-4. Previously, calculating
  life expectancy from age 1 used the 1-4 rate in place of the infant
  rate. The result now matches the `"1-4"` row of `lifetab()`. If
  age 0 is absent and `ax` is not supplied for `"1-4"`, `lifeexp()`
  gives an error.
- `lifeexp()`, `age_labels()`, and `set_age_open()` now require whole
  numbers for ages. For example, `lifeexp(at = 60.7)` and
  `age_labels(min = 0.9)` give an error instead of silently using
  `60` and `0`. `60` and `60.0` are still accepted.
- `lifetab()` and `lifeexp()` now give an error when a `qx`
  calculation has only one age group. The open age group is
  calculated from the previous age group, so `qx` needs at least
  two. This includes `lifeexp()` at an age that leaves only the
  final age group. A supplied `ax` does not bypass the error.
- `tfr()` now names the `by` group where a problem occurred.
  Previously the message always named the last group.
- `tfr()` now ignores unused levels of a `sex` factor. A level that
  does not appear in the data is no longer checked.
- `check_n()` now uses the supplied name in the message when a value
  is not divisible by `divisible_by`. For example, with
  `nm_n = "count"`, the message names `count` rather than `nm_n`.
- `rr3()` now gives an error when the rounded value would fall
  outside the range of an R integer. Previously,
  `.Machine$integer.max` rounded up, and `-.Machine$integer.max`
  rounded down, wrapped around to a value of the opposite sign.
- `reformat_sex()` now accepts `"woman"`, `"girl"`, `"man"`, and
  `"boy"`. `find_label_female()` recognizes `"woman"`, and
  `find_label_male()` recognizes `"man"`. `"girls"` and `"boys"`
  match only those words.
- `reformat_age()` now prefers one-year ages, then five-year ages,
  then life table ages, when labels fit more than one type. This is
  the same order as `age_group_type()`. For example, `c("0", "90+")`
  is filled with one-year ages. A series that includes `"1-4"` or
  `"5-9"` is still treated as a life table.


## Documentation

- The vignette now gives the correct constant-mortality formula for
  `a0` when the input is `q0`.
- The vignette now gives the Coale-Demeny formula for females aged
  1-4, with a `q0` input, as `1.524 - 1.625 q0`.
- The vignette now uses `q0`, rather than `m0`, in the Coale-Demeny
  threshold for female `a0` when the input is `q0`.
- The vignette now places age-interval widths as left subscripts on
  the life table symbols, so that they render correctly.
- The help for `lifetab()` now defines `qx` as the probability of
  dying.


# poputils 0.6.1

## Reducing minimum R version

- Reduced minimum R version required from 4.3.0 to 4.2.0, following
  the reduction in package `rvec`.


## Bug fix

- Discovered that function `ex_to_lifetab_brass()` was using a
  non-standard formula (applying logit to lx rather than 1 -
  lx). Hard-deprecated the function, and created a new function,
  `e0_to_lifetab_logit()`, that uses the correct formula.


# poputils 0.6.0

## Interface

- Extended `.intrinsic_growth_rate()` to allow for rvec `mx` and `Lx`
  arguments.
- Improved documentation for `tfr_to_asfr_scale()` and
  `ex_to_lifetab_brass()`.
  

# poputils 0.5.0

## Interface

* Renamed `iran_fertility` to `irn_fert`.
* Renamed `nzmort` to `nzl_mort`.
* Renamed `nzmort_rvec` to `nzl_mort_rvec`.
* Added function `.intrinsic_growth_rate()` to calculate growth rate
  associated with fertility, mortality schedules. Fairly low level -
  more user-friendly functions calling `intrinsic_growth_rate()` will
  be added in due course.
* `ex_to_lifetable_brass()` returns a tibble rather than a base R data
  frame.
* `asfr` argument for `tfr()` now defaults to `asfr`.

## Internal calculations

* `to_matrix()` now uses `rlang::all_of()` with tidyselect arguments,
  to avoid deprecation warnings.


# poputils 0.4.2

## Interface

* Modify function `rr3` to use C code. (#30)
* Modified tests for upcoming changes to package `rvec` (where random
  variant functions for discrete distributions will always return
  doubles).

# poputils 0.4.1

* Modify C++ function `qx_to_ex()` to avoid possible memory leekage
  found by valgrind.
  

# poputils 0.4.0

## Interface

* If `data` already contains columns called `"lx"`, `"Lx"`, `"dx"`,
  `"ex"`, then `lifetab()` overwrites these columns, with a message,
  rather than creating new versions.
* `at` argument to `lifeexp()` can now be a vector with length
  > 1. In this case, `lifeexp()` calculates life expectancy for each
  value (within each combination of the 'by' variables, if present.)
* Added `n_core` argument to `lifetab()` and `lifeexp()`. Setting
  `n_core` to a value greater than 1 triggers parallel processing.
* Added `closed` argument to `check_age()`, to check whether the
  oldest age group is closed.
* Added function `tfr()` for calculating total fertility rates.
* Extended vignette.


## Bug fixes

* `combine_age()` previously not working properly if `"to"` is
  `"five"`, and lower limit of youngest age group in `x` not divisible
  by 5.
* `ex_to_lifetab_brass()` previously assumed, without checking that
  age groups were correctly ordered. `ex_to_lifetab_brass()` now
  automatically reorders them.


# poputils 0.3.3

## Bug fixes

* Previously calling `lifetab()` with value supplied for `qx` created
  a duplicate `qx` column in the result. The extra column has been
  removed.
  

## Interface

* Added `at` argument to `lifeexp()`.
* Removed `mx` column from output of `lifetab()` when calculated from
  mx (to be consistent with behaviour of `lifetab()` when calculated
  from qx.)


## New functions

* Added function `check_n()`.
* Added function `q0_to_m0()`.
* Added function `rr3()`.
* Added function `trim_01()`.


## Internal

* Removed `stop(gettextf(` style error messages, except in functions
  involving age (which will be superceded by package **agetime**.


# poputils 0.3.2

## Internal 

* Removed all uses of `rvec::rvec.is.numeric()`, in preparation for
  removing from **rvec** package.
  
## Bug fixes

* Fixed bug in `combine_age()`. Previously giving wrong answers when
  last age group was closed.

# poputils 0.3.1

## Bug fixes

* Fixed "lifetab.cpp:455:16: runtime error: inf is outside the
  range of representable values of type 'int'" - changed nx from int
  to double

# poputils 0.3.0

* Released on to CRAN

# poputils 0.2.0

* Coverage of tests 100 percent

