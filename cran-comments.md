

# Revised submission, 10 June 2024, version 0.3.1

## clang-UBSAN error

On 24 May, Professor Ripley picked up the following error by running
clang-UBSAN:

```
lifetab.cpp:455:16: runtime error: inf is outside the range of
representable values of type 'int'
```

I have:
- replicated the error by using rhub::rhub_check(platform =
  "clang-asan")
- fixed the bug by changing a variable declaration from int to
  double
- verified that the error is fixed by re-running rhub::rhub_check(platform =
  "clang-asan")
  


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
  
  





