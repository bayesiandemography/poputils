// Wrappers for the growth-rate and stable-composition functions in
// inst/include/poputils/stable.hpp. Called by the exported developer
// functions in R/intrinsic_growth_rate.R and R/stable_composition.R,
// which check inputs.

#include "cpp11.hpp"
#include "cpp11/doubles.hpp"
#include "cpp11/matrix.hpp"
#include <poputils/stable.hpp>
#include <cmath>
#include <vector>
using namespace cpp11;
namespace writable = cpp11::writable;


// Checks the conditions the header assumes, and that the fixed
// number of Newton steps reached the root.

// HAS_TESTS
[[cpp11::register]]
double intrinsic_growth_rate_cpp11(doubles mx,
                                   doubles Lx,
                                   doubles age_mid,
                                   int n_iter) {
  const int n = mx.size();
  std::vector<double> m(mx.begin(), mx.end());
  std::vector<double> l(Lx.begin(), Lx.end());
  std::vector<double> a(age_mid.begin(), age_mid.end());
  double nrr = 0.0;
  double sum_age = 0.0;
  for (int i = 0; i < n; i++) {
    nrr += m[i] * l[i];
    sum_age += a[i] * m[i] * l[i];
  }
  if (!std::isfinite(nrr) || nrr <= 0.0)
    cpp11::stop("nrr = sum(mx * Lx) must be positive and finite.");
  const double mean_age = sum_age / nrr;
  if (!std::isfinite(mean_age) || mean_age <= 0.0)
    cpp11::stop("mean age of reproduction must be positive and finite.");
  double r;
  poputils::intrinsic_growth_rate(m, l, a, n_iter, r);
  double f = -1.0;
  for (int i = 0; i < n; i++)
    f += std::exp(-r * a[i]) * m[i] * l[i];
  if (!std::isfinite(r) || std::abs(f) > 1e-8)
    cpp11::stop("failed to converge.");
  return r;
}


// Rows are age groups, columns are draws.

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> stable_composition_one_sex_mat(doubles_matrix<> Lx,
                                                          doubles age_mid,
                                                          doubles r) {
  const int n_age = Lx.nrow();
  const int n_col = Lx.ncol();
  std::vector<double> a(age_mid.begin(), age_mid.end());
  std::vector<double> l(n_age), c(n_age);
  writable::doubles_matrix<> ans(n_age, n_col);
  for (int j = 0; j < n_col; j++) {
    for (int i = 0; i < n_age; i++)
      l[i] = Lx(i, j);
    poputils::stable_composition_one_sex(l, a, r[j], c);
    for (int i = 0; i < n_age; i++)
      ans(i, j) = c[i];
  }
  return ans;
}


// HAS_TESTS
[[cpp11::register]]
cpp11::writable::list stable_composition_two_sex_mat(doubles_matrix<> Lx_female,
                                                     doubles_matrix<> Lx_male,
                                                     doubles age_mid,
                                                     doubles r,
                                                     double sex_ratio) {
  const int n_age = Lx_female.nrow();
  const int n_col = Lx_female.ncol();
  std::vector<double> a(age_mid.begin(), age_mid.end());
  std::vector<double> lf(n_age), lm(n_age), cf(n_age), cm(n_age);
  writable::doubles_matrix<> female(n_age, n_col);
  writable::doubles_matrix<> male(n_age, n_col);
  for (int j = 0; j < n_col; j++) {
    for (int i = 0; i < n_age; i++) {
      lf[i] = Lx_female(i, j);
      lm[i] = Lx_male(i, j);
    }
    poputils::stable_composition_two_sex(lf, lm, a, r[j], sex_ratio, cf, cm);
    for (int i = 0; i < n_age; i++) {
      female(i, j) = cf[i];
      male(i, j) = cm[i];
    }
  }
  using namespace cpp11::literals;
  return writable::list({"female"_nm = female, "male"_nm = male});
}
