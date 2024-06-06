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
  
  





