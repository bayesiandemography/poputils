// lifetab_const.hpp
//
// Life tables under a constant hazard within each age interval.
//
// Header-only and templated on the container and number types, so the
// same code serves poputils' own C++ (doubles, via cpp11) and other
// packages' C++, including TMB templates (automatic-differentiation
// types). Use from another package with 'LinkingTo: poputils' and
//
//   #include <poputils/lifetab_const.hpp>
//
// Conventions
// - One life table per call. Containers need only operator[] and
//   size(): std::vector, TMB's vector<Type>, Eigen arrays, ...
// - Intervals are ordered by age. The last interval is open.
//   'width' gives the width of each interval; the element for the
//   open interval is ignored.
// - Outputs are written into containers supplied by the caller, with
//   the same length as the inputs.
// - lx is the proportion surviving to the start of each interval,
//   with lx[0] = 1 for a complete table.
// - The open interval has the hazard of the last closed interval,
//   when the hazard is derived from lx (lx says nothing about
//   mortality beyond the last age).
//
// No value-dependent branching
//   TMB records the operations once and reuses the record, so a branch
//   on a value (eg 'if (mx == 0)') would be fixed at whichever case
//   occurred first. The functions here therefore contain no branches
//   on values, only on positions. The price is that every hazard must
//   be positive (lx strictly decreasing): with a zero hazard, the
//   formulas for person-years divide zero by zero. Callers working
//   with doubles that need zero hazards must handle them separately.
//
// Requirements are not checked: callers check inputs.

#ifndef POPUTILS_LIFETAB_CONST_HPP
#define POPUTILS_LIFETAB_CONST_HPP

#include <cmath>
#include <cstddef>

namespace poputils {

// Hazards from survivorship.
//
// mx[i] = log(lx[i] / lx[i + 1]) / width[i] for closed intervals.
// The open interval gets the hazard of the last closed interval.
// Requires: at least two intervals; lx positive and strictly
// decreasing.
template <class VecL, class VecW, class VecM>
void lx_to_mx_const(const VecL& lx, const VecW& width, VecM& mx) {
  using std::log;
  const std::size_t n = lx.size();
  for (std::size_t i = 0; i + 1 < n; i++)
    mx[i] = log(lx[i] / lx[i + 1]) / width[i];
  mx[n - 1] = mx[n - 2];
}

// Survivorship from hazards.
//
// lx[0] = 1; lx[i + 1] = lx[i] * exp(-mx[i] * width[i]).
template <class VecM, class VecW, class VecL>
void mx_to_lx_const(const VecM& mx, const VecW& width, VecL& lx) {
  using std::exp;
  const std::size_t n = mx.size();
  lx[0] = 1.0;
  for (std::size_t i = 0; i + 1 < n; i++)
    lx[i + 1] = lx[i] * exp(-mx[i] * width[i]);
}

// Person-years lived in each interval.
//
// Closed: Lx[i] = lx[i] * (1.0 - exp(-mx[i] * width[i])) / mx[i].
// Open:   Lx[n - 1] = lx[n - 1] / mx[n - 1].
// Requires: every mx positive.
template <class VecM, class VecL, class VecW, class VecLL>
void mx_to_Lx_const(const VecM& mx, const VecL& lx, const VecW& width, VecLL& Lx) {
  using std::exp;
  const std::size_t n = mx.size();
  for (std::size_t i = 0; i + 1 < n; i++)
    Lx[i] = lx[i] * (1.0 - exp(-mx[i] * width[i])) / mx[i];
  Lx[n - 1] = lx[n - 1] / mx[n - 1];
}

// Life expectancy at the start of each interval.
//
// Tx is the sum of Lx from interval i onwards; ex[i] = Tx[i] / lx[i].
// Does not depend on the assumption about hazards.
template <class VecLL, class VecL, class VecE>
void Lx_to_ex(const VecLL& Lx, const VecL& lx, VecE& ex) {
  const std::size_t n = Lx.size();
  ex[n - 1] = Lx[n - 1];
  for (std::size_t i = n - 1; i > 0; i--)
    ex[i - 1] = Lx[i - 1] + ex[i];  // Tx, accumulated in place
  for (std::size_t i = 0; i < n; i++)
    ex[i] = ex[i] / lx[i];
}

// Brass logit life table.
//
// logit(1 - lx) = alpha + beta * logit(1 - lx_std), ie
// lx = 1 / (1 + exp(alpha) * ((1 - lx_std) / lx_std)^beta),
// for every age after the first; lx[0] = 1.
// Requires: lx_std[0] = 1, and 0 < lx_std < 1 at later ages.
template <class VecS, class TypeA, class TypeB, class VecL>
void brass_logit_lx(const VecS& lx_std, const TypeA& alpha, const TypeB& beta, VecL& lx) {
  using std::exp;
  using std::log;
  const std::size_t n = lx_std.size();
  lx[0] = 1.0;
  for (std::size_t i = 1; i < n; i++)
    lx[i] = 1.0 / (1.0 + exp(alpha + beta * log((1.0 - lx_std[i]) / lx_std[i])));
}

}  // namespace poputils

#endif  // POPUTILS_LIFETAB_CONST_HPP
