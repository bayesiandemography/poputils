// Matrix wrappers for the constant-hazard life-table functions in
// inst/include/poputils/lifetab_const.hpp. Rows are age intervals
// (last one open), columns are draws. Called by the exported
// developer functions in R/lifetab_const.R, which check inputs.

#include "cpp11.hpp"
#include "cpp11/doubles.hpp"
#include "cpp11/matrix.hpp"
#include <poputils/lifetab_const.hpp>
#include <cmath>
#include <vector>
using namespace cpp11;
namespace writable = cpp11::writable;


// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> lx_to_mx_const_mat(cpp11::doubles_matrix<> lx,
                                              cpp11::doubles width) {
  int n_age = lx.nrow();
  int n_col = lx.ncol();
  std::vector<double> w(width.begin(), width.end());
  std::vector<double> l(n_age), m(n_age);
  writable::doubles_matrix<> ans(n_age, n_col);
  for (int j = 0; j < n_col; j++) {
    for (int i = 0; i < n_age; i++)
      l[i] = lx(i, j);
    poputils::lx_to_mx_const(l, w, m);
    for (int i = 0; i < n_age; i++)
      ans(i, j) = m[i];
  }
  return ans;
}


// Unlike the header, handles zero hazards: person-years in a closed
// interval are then lx * width, and in the open interval infinite.

// HAS_TESTS
[[cpp11::register]]
cpp11::writable::list mx_to_lifetab_const_mat(cpp11::doubles_matrix<> mx,
                                              cpp11::doubles width) {
  int n_age = mx.nrow();
  int n_col = mx.ncol();
  std::vector<double> w(width.begin(), width.end());
  std::vector<double> m(n_age), l(n_age), L(n_age), e(n_age);
  writable::doubles_matrix<> qx(n_age, n_col);
  writable::doubles_matrix<> lx(n_age, n_col);
  writable::doubles_matrix<> dx(n_age, n_col);
  writable::doubles_matrix<> Lx(n_age, n_col);
  writable::doubles_matrix<> ex(n_age, n_col);
  for (int j = 0; j < n_col; j++) {
    for (int i = 0; i < n_age; i++)
      m[i] = mx(i, j);
    poputils::mx_to_lx_const(m, w, l);
    poputils::mx_to_Lx_const(m, l, w, L);
    for (int i = 0; i + 1 < n_age; i++)
      if (m[i] == 0)
        L[i] = l[i] * w[i];
    if (m[n_age - 1] == 0)
      L[n_age - 1] = R_PosInf;
    poputils::Lx_to_ex(L, l, e);
    for (int i = 0; i < n_age; i++) {
      double q = (i + 1 < n_age) ? 1.0 - l[i + 1] / l[i] : 1.0;
      qx(i, j) = q;
      lx(i, j) = l[i];
      dx(i, j) = l[i] * q;
      Lx(i, j) = L[i];
      ex(i, j) = e[i];
    }
  }
  writable::list ans({qx, lx, dx, Lx, ex});
  ans.names() = {"qx", "lx", "dx", "Lx", "ex"};
  return ans;
}


// 'lx_std' has one column (used for every value of 'alpha') or one
// column per value of 'alpha'. 'beta' has the same length as 'alpha'.

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> brass_logit_lx_mat(cpp11::doubles_matrix<> lx_std,
                                              cpp11::doubles alpha,
                                              cpp11::doubles beta) {
  int n_age = lx_std.nrow();
  int n_std = lx_std.ncol();
  int n_col = alpha.size();
  std::vector<double> s(n_age), l(n_age);
  writable::doubles_matrix<> ans(n_age, n_col);
  for (int j = 0; j < n_col; j++) {
    int j_std = (n_std == 1) ? 0 : j;
    for (int i = 0; i < n_age; i++)
      s[i] = lx_std(i, j_std);
    double a = alpha[j];
    double b = beta[j];
    poputils::brass_logit_lx(s, a, b, l);
    for (int i = 0; i < n_age; i++)
      ans(i, j) = l[i];
  }
  return ans;
}


// Life expectancy at birth from survivorship, constant hazard, with
// the open interval given the hazard of the last closed interval.
// Doubles only, so zero hazards can be handled with branches.
double e0_from_lx_const(const std::vector<double>& l,
                        const std::vector<double>& w,
                        std::vector<double>& m,
                        std::vector<double>& L) {
  int n_age = l.size();
  poputils::lx_to_mx_const(l, w, m);
  poputils::mx_to_Lx_const(m, l, w, L);
  for (int i = 0; i + 1 < n_age; i++)
    if (m[i] == 0)
      L[i] = l[i] * w[i];
  if (m[n_age - 1] == 0)
    return R_PosInf;
  double ans = 0;
  for (int i = 0; i < n_age; i++)
    ans += L[i];
  return ans;
}


// Brass level parameter alpha giving each target life expectancy at
// birth. e0 decreases in alpha (for beta > 0), so bisection on a
// bracket converges. The bracket starts at [-1, 1] and doubles until
// it contains the target, up to |alpha| = 64; beyond that, NaN (the
// R function reports the error).

// HAS_TESTS
[[cpp11::register]]
writable::doubles e0_to_alpha_brass_mat(cpp11::doubles e0,
                                        cpp11::doubles_matrix<> lx_std,
                                        cpp11::doubles width,
                                        cpp11::doubles beta) {
  int n_age = lx_std.nrow();
  int n_std = lx_std.ncol();
  int n_target = e0.size();
  std::vector<double> w(width.begin(), width.end());
  std::vector<double> s(n_age), l(n_age), m(n_age), L(n_age);
  writable::doubles ans(n_target);
  const double alpha_max = 64;
  const double tol = 1e-12;
  for (int k = 0; k < n_target; k++) {
    int j_std = (n_std == 1) ? 0 : k;
    for (int i = 0; i < n_age; i++)
      s[i] = lx_std(i, j_std);
    double target = e0[k];
    double b = beta[k];
    auto e0_at = [&](double a) {
      poputils::brass_logit_lx(s, a, b, l);
      return e0_from_lx_const(l, w, m, L);
    };
    double lo = -1, hi = 1;
    // e0 decreasing in alpha: want e0(lo) >= target >= e0(hi)
    while (e0_at(lo) < target && lo > -alpha_max)
      lo *= 2;
    while (e0_at(hi) > target && hi < alpha_max)
      hi *= 2;
    if (e0_at(lo) < target || e0_at(hi) > target) {
      ans[k] = NA_REAL;
      continue;
    }
    while (hi - lo > tol) {
      double mid = 0.5 * (lo + hi);
      if (e0_at(mid) > target)
        lo = mid;
      else
        hi = mid;
    }
    ans[k] = 0.5 * (lo + hi);
  }
  return ans;
}
