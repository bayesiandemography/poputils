
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

