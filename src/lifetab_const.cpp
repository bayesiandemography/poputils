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
