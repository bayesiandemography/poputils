
# poputils 0.6.1

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

