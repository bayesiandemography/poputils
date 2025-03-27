
# Submission of version 0.4.1, 27 March 2025

- I am resubmitted (with version number 0.4.1 rather than 0.4.0) in
  response to a valgrind test run by Professor Ripley, which
  uncovered a possible memory leekage. The relevant part of the
  valgrind log is:
  
```
==1056036== HEAP SUMMARY:
==1056036==     in use at exit: 126,121,421 bytes in 24,285 blocks
==1056036==   total heap usage: 2,171,524 allocs, 2,147,239 frees, 1,998,434,806 bytes allocated
==1056036== 
==1056036== LEAK SUMMARY:
==1056036==    definitely lost: 0 bytes in 0 blocks
==1056036==    indirectly lost: 0 bytes in 0 blocks
==1056036==      possibly lost: 0 bytes in 0 blocks
==1056036==    still reachable: 126,121,069 bytes in 24,282 blocks
==1056036==         suppressed: 352 bytes in 3 blocks
==1056036== Reachable blocks (those to which a pointer was found) are not shown.
==1056036== To see them, rerun with: --leak-check=full --show-leak-kinds=all
==1056036== 
==1056036== For lists of detected and suppressed errors, rerun with: -s
==1056036== ERROR SUMMARY: 1 errors from 1 contexts (suppressed: 0
from 0)
```

- The cause of the error was a vector that was not initialised when it
  was created. I have revised the code to carry out the
  initialization. Running valgrind on the revised code via
  `rhub::rhub_check()` gives the following report:
  
```
==1544== 
==1544== HEAP SUMMARY:
==1544==     in use at exit: 128,402,046 bytes in 24,157 blocks
==1544==   total heap usage: 2,166,341 allocs, 2,142,184 frees, 2,024,268,856 bytes allocated
==1544== 
==1544== LEAK SUMMARY:
==1544==    definitely lost: 0 bytes in 0 blocks
==1544==    indirectly lost: 0 bytes in 0 blocks
==1544==      possibly lost: 0 bytes in 0 blocks
==1544==    still reachable: 128,402,046 bytes in 24,157 blocks
==1544==                       of which reachable via heuristic:
==1544==                         newarray           : 4,264 bytes in 1 blocks
==1544==         suppressed: 0 bytes in 0 blocks
==1544== Reachable blocks (those to which a pointer was found) are not shown.
==1544== To see them, rerun with: --leak-check=full --show-leak-kinds=all
==1544== 
==1544== For lists of detected and suppressed errors, rerun with: -s
==1544== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
```
  
The number of errors has dropped to zero. There *is* still a report of
some blocks still reachable, but my understanding is that it is
harmless.


## Changes since previous CRAN submission (version 0.3.3)

- Adding parallel processing option to `lifeexp()`
- Added function `tfr()`
- Allowing `at` argument to `lifeexp()` to be vector
- Bug fixes in `combine_age()`, `ex_to_lifetab_brass()`


## Reverse dependencies

- **poputils** has a single reverse dependency, **bage**. All tests in
  **bage** are passing with version 0.4.0 of **poputils**


# Submission of version 0.3.3, 14 September 2024

## Changes since previous CRAN submission (version 0.3.1)

- Fixed bugs in functions `combine_age()` and `lifetab()`
- Added new functions `check_n()`, `q0_to_m0()`, `rr3()`, `trim_01()`
- Added `at` argument to `lifeexp()`

## Reverse dependencies

- **poputils** has a single reverse dependency, **bage**. All tests in
  **bage** are passing with version 0.3.3 of **poputils**


# Revised submission, 10 June 2024, version 0.3.1

## clang-UBSAN error

On 24 May, Professor Ripley picked up the following error by running
clang-UBSAN:

```
lifetab.cpp:455:16: runtime error: inf is outside the range of
representable values of type 'int'
```

The message from Professor Ripley asked that the problems be fixed by
14 June 2024.

I have:
- replicated the error by using rhub::rhub_check(platform =
  "clang-asan")
- fixed the bug by changing a variable declaration from int to
  double
- verified that the error is fixed by re-running rhub::rhub_check(platform =
  "clang-asan")
  

## Notes about non-API calls

The check page for the package
(https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-debian-clang/poputils-00check.html)
has notes warning about non-API calls. However, these non-API calls
appear to come from the **vctrs** package (which **poputils** imports)
rather than from **poputils** itself.



# Initial submission, 17 May 2024, version 0.3.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* There are no specific references describing this package, though the
  help for some functions (eg `lifetab()`) does cite the demographic
  literature.
* When checking on win-builder, I get the following note: 

    * checking compiled code ... NOTE
    File 'poputils/libs/x64/poputils.dll':
      Found non-API calls to R: 'SETLENGTH', 'SET_TRUELENGTH'
	  
  However,this appears to be an issue with the package 'cpp11', which
  is imported by 'poputils', rather than with 'poputils' itself. A
  quick google search shows that other packages are getting the same
  note.
  
  





