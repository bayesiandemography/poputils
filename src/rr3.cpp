#include "cpp11.hpp"
#include "cpp11/integers.hpp"
#include <limits>

extern "C" {
#include <R_ext/Random.h>
}

using namespace cpp11;
using namespace std;
namespace writable = cpp11::writable;

// r-style modulo
inline int mod3(int x) { 
  int r = x % 3;
  return (r < 0) ? r + 3 : r;
}

// HAS_TESTS
[[cpp11::register]]
writable::integers rr3_inner(integers x) {
  int n = x.size();
  writable::integers out(n);
  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    int xi = x[i];
    if (xi == NA_INTEGER) {
      out[i] = NA_INTEGER;
      continue;
    }
    int rem = mod3(xi);
    double p = unif_rand();
    long long xi_ll = static_cast<long long>(xi);
    long long rounded;
    if (rem == 1) {
      rounded = (p < 2.0 / 3.0) ? xi_ll - 1 : xi_ll + 2;
    } else if (rem == 2) {
      rounded = (p < 1.0 / 3.0) ? xi_ll - 2 : xi_ll + 1;
    } else {
      rounded = xi_ll; // rem == 0
    }
    long long lo = static_cast<long long>(std::numeric_limits<int>::min()) + 1;
    long long hi = std::numeric_limits<int>::max();
    if (rounded < lo || rounded > hi) {
      PutRNGstate();
      stop("Randomly rounded value is outside the range of an integer.");
    }
    out[i] = static_cast<int>(rounded);
  }
  PutRNGstate();
  return out;
}
