// stable.hpp
//
// Intrinsic growth rate and stable age composition.
//
// Header-only and templated on the container and number types, so the
// same code serves poputils' own C++ (doubles, via cpp11) and other
// packages' C++, including TMB templates (automatic-differentiation
// types). Use from another package with 'LinkingTo: poputils' and
//
//   #include <poputils/stable.hpp>
//
// Conventions (as lifetab_const.hpp)
// - One schedule per call. Containers need only operator[] and
//   size(): std::vector, TMB's vector<Type>, Eigen arrays, ...
// - Age groups are ordered by age; 'age_mid' gives a representative
//   age for each (the caller chooses the value for an open group).
// - Outputs are written into variables or containers supplied by the
//   caller.
// - 'Lx' is person-years lived in each age group per newborn (radix 1).
//
// No value-dependent branching
//   TMB records the operations once and reuses the record, so a branch
//   on a value would be fixed at whichever case occurred first. The
//   growth rate is therefore found with a fixed number of Newton
//   steps, with no test for convergence. Starting from
//   log(NRR) / (mean age of childbearing), 10 steps are ample for
//   realistic schedules; callers working with doubles can check the
//   result (eg that sum(exp(-r * age_mid) * mx * Lx) is close to 1).
//
// Requirements are not checked: callers check inputs. The growth
// rate needs sum(mx * Lx) > 0 and a positive mean age of
// childbearing; the composition needs some positive 'Lx'.

#ifndef POPUTILS_STABLE_HPP
#define POPUTILS_STABLE_HPP

#include <cmath>
#include <cstddef>

namespace poputils {

// Intrinsic growth rate.
//
// Solves sum_a exp(-r * age_mid[a]) * mx[a] * Lx[a] = 1 for r, with
// 'n_iter' Newton steps from r0 = log(NRR) / mean age of
// childbearing, where NRR = sum(mx * Lx). 'mx' is the fertility
// schedule for one sex (eg daughters per woman), and 'Lx' the
// matching person-years. The result is written to 'r'.
template <class VecM, class VecL, class VecA, class T>
void intrinsic_growth_rate(const VecM& mx,
                           const VecL& Lx,
                           const VecA& age_mid,
                           int n_iter,
                           T& r) {
  using std::exp;
  using std::log;
  const std::size_t n = mx.size();
  T nrr = 0;
  T sum_age = 0;
  for (std::size_t i = 0; i < n; i++) {
    nrr += mx[i] * Lx[i];
    sum_age += age_mid[i] * mx[i] * Lx[i];
  }
  r = log(nrr) / (sum_age / nrr);
  for (int k = 0; k < n_iter; k++) {
    T f = -1;
    T fp = 0;
    for (std::size_t i = 0; i < n; i++) {
      T w = exp(-r * age_mid[i]) * mx[i] * Lx[i];
      f += w;
      fp -= age_mid[i] * w;
    }
    r = r - f / fp;
  }
}

// Stable age composition, one sex.
//
// c[a] proportional to exp(-r * age_mid[a]) * Lx[a], summing to 1.
template <class VecL, class VecA, class T, class VecC>
void stable_composition_one_sex(const VecL& Lx,
                                const VecA& age_mid,
                                const T& r,
                                VecC& c) {
  using std::exp;
  const std::size_t n = Lx.size();
  T total = 0;
  for (std::size_t i = 0; i < n; i++) {
    c[i] = exp(-r * age_mid[i]) * Lx[i];
    total += c[i];
  }
  for (std::size_t i = 0; i < n; i++)
    c[i] = c[i] / total;
}

// Stable age-sex composition, two sexes.
//
// Births are split by sex using 'sex_ratio' (males per 100 females):
// a share 100 / (100 + sex_ratio) female and sex_ratio / (100 +
// sex_ratio) male. Both sexes grow at rate 'r'. 'Lx_female' and
// 'Lx_male' are per newborn of each sex. 'c_female' and 'c_male'
// together sum to 1.
template <class VecL1, class VecL2, class VecA, class T, class S, class VecC1, class VecC2>
void stable_composition_two_sex(const VecL1& Lx_female,
                                const VecL2& Lx_male,
                                const VecA& age_mid,
                                const T& r,
                                const S& sex_ratio,
                                VecC1& c_female,
                                VecC2& c_male) {
  using std::exp;
  const std::size_t n = Lx_female.size();
  const S p_female = S(100) / (S(100) + sex_ratio);
  const S p_male = sex_ratio / (S(100) + sex_ratio);
  T total = 0;
  for (std::size_t i = 0; i < n; i++) {
    T e = exp(-r * age_mid[i]);
    c_female[i] = p_female * e * Lx_female[i];
    c_male[i] = p_male * e * Lx_male[i];
    total += c_female[i] + c_male[i];
  }
  for (std::size_t i = 0; i < n; i++) {
    c_female[i] = c_female[i] / total;
    c_male[i] = c_male[i] / total;
  }
}

} // namespace poputils

#endif
