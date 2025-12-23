# Changelog

## poputils 0.5.0

### Interface

- Renamed `iran_fertility` to `irn_fert`.
- Renamed `nzmort` to `nzl_mort`.
- Renamed `nzmort_rvec` to `nzl_mort_rvec`.
- Added function
  [`.intrinsic_growth_rate()`](https://bayesiandemography.github.io/poputils/reference/dot-intrinsic_growth_rate.md)
  to calculate growth rate associated with fertility, mortality
  schedules. Fairly low level - more user-friendly functions calling
  `intrinsic_growth_rate()` will be added in due course.
- `ex_to_lifetable_brass()` returns a tibble rather than a base R data
  frame.
- `asfr` argument for
  [`tfr()`](https://bayesiandemography.github.io/poputils/reference/tfr.md)
  now defaults to `asfr`.

### Internal calculations

- [`to_matrix()`](https://bayesiandemography.github.io/poputils/reference/to_matrix.md)
  now uses `rlang:all_of()` with tidyselect arguments, to avoid
  deprecation warnings.

## poputils 0.4.2

CRAN release: 2025-07-12

### Interface

- Modify function `rr3` to use C code.
  ([\#30](https://github.com/bayesiandemography/poputils/issues/30))
- Modified tests for upcoming changes to package `rvec` (where random
  variant functions for discrete distributions will always return
  doubles).

## poputils 0.4.1

CRAN release: 2025-03-27

- Modify C++ function `qx_to_ex()` to avoid possible memory leekage
  found by valgrind.

## poputils 0.4.0

CRAN release: 2025-03-23

### Interface

- If `data` already contains columns called `"lx"`, `"Lx"`, `"dx"`,
  `"ex"`, then
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  overwrites these columns, with a message, rather than creating new
  versions.
- `at` argument to
  [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  can now be a vector with length \> 1. In this case,
  [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  calculates life expectancy for each value (within each combination of
  the ‘by’ variables, if present.)
- Added `n_core` argument to
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  and
  [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md).
  Setting `n_core` to a value greater than 1 triggers parallel
  processing.
- Added `closed` argument to
  [`check_age()`](https://bayesiandemography.github.io/poputils/reference/check_age.md),
  to check whether the oldest age group is closed.
- Added function
  [`tfr()`](https://bayesiandemography.github.io/poputils/reference/tfr.md)
  for calculating total fertility rates.
- Extended vignette.

### Bug fixes

- [`combine_age()`](https://bayesiandemography.github.io/poputils/reference/combine_age.md)
  previously not working properly if `"to"` is `"five"`, and lower limit
  of youngest age group in `x` not divisible by 5.
- [`ex_to_lifetab_brass()`](https://bayesiandemography.github.io/poputils/reference/ex_to_lifetab_brass.md)
  previously assumed, without checking that age groups were correctly
  ordered.
  [`ex_to_lifetab_brass()`](https://bayesiandemography.github.io/poputils/reference/ex_to_lifetab_brass.md)
  now automatically reorders them.

## poputils 0.3.3

CRAN release: 2024-09-14

### Bug fixes

- Previously calling
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  with value supplied for `qx` created a duplicate `qx` column in the
  result. The extra column has been removed.

### Interface

- Added `at` argument to
  [`lifeexp()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md).
- Removed `mx` column from output of
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  when calculated from mx (to be consistent with behaviour of
  [`lifetab()`](https://bayesiandemography.github.io/poputils/reference/lifetab.md)
  when calculated from qx.)

### New functions

- Added function
  [`check_n()`](https://bayesiandemography.github.io/poputils/reference/check_n.md).
- Added function
  [`q0_to_m0()`](https://bayesiandemography.github.io/poputils/reference/q0_to_m0.md).
- Added function
  [`rr3()`](https://bayesiandemography.github.io/poputils/reference/rr3.md).
- Added function
  [`trim_01()`](https://bayesiandemography.github.io/poputils/reference/trim_01.md).

### Internal

- Removed `stop(gettextf(` style error messages, except in functions
  involving age (which will be superceded by package **agetime**.

## poputils 0.3.2

### Internal

- Removed all uses of `rvec::rvec.is.numeric()`, in preparation for
  removing from **rvec** package.

### Bug fixes

- Fixed bug in
  [`combine_age()`](https://bayesiandemography.github.io/poputils/reference/combine_age.md).
  Previously giving wrong answers when last age group was closed.

## poputils 0.3.1

CRAN release: 2024-06-12

### Bug fixes

- Fixed “lifetab.cpp:455:16: runtime error: inf is outside the range of
  representable values of type ‘int’” - changed nx from int to double

## poputils 0.3.0

CRAN release: 2024-05-24

- Released on to CRAN

## poputils 0.2.0

- Coverage of tests 100 percent
