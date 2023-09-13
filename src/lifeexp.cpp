#include <cpp11.hpp>
#include "cpp11/doubles.hpp"
#include "cpp11/matrix.hpp"
using namespace cpp11;
using namespace std;
namespace writable = cpp11::writable;

// Helper functions -----------------------------------------------------------


double make_a0_cd(double m0, string sex) {
  if (sex == "Female")
    return (m0 >= 0.107) ? 0.35 : (0.053 + 2.8 * m0);
  else if (sex == "Male")
    return (m0 >= 0.107) ? 0.33 : (0.045 + 2.684 * m0);
  else {
    stop("Internal error: Invalid value for 'sex'.");
  }
}


double make_a0_hmd(double m0, string sex) {
  if (sex == "Female") {
    if (m0 >= 0.06891)
      return 0.31411;
    else if (m0 >= 0.01724)
      return 0.04667 + 3.88089 * m0;
    else
      return 0.14903 - 2.05527 * m0;
  }
  else if (sex == "Male") {
    if (m0 >= 0.08307)
      return 0.29915;
    else if (m0 >= 0.023)
      return 0.02832 + 3.26021 * m0;
    else
      return 0.14929 - 1.99545 * m0;
  }
  else {
    stop("Internal error: Invalid value for 'sex'.");
  }
}


int make_nx(string age_group_type) {
  if (age_group_type == "0")
    return 1;
  if (age_group_type == "1-4")
    return 4;
  if (age_group_type == "single")
    return 1;
  if (age_group_type == "five")
    return 5;
  if (age_group_type == "open")
    stop("Internal error: 'nx' not defined when 'age_group_type' is \"open\".");
  stop("Internal error: Invalid value for 'age_group_type'.");
}

double make_qx_ax(double mx, double ax, int nx) {
  double ans = 1;
  if (isfinite(mx)) {
    ans = (nx * mx) / (1 + (nx - ax) * mx);
    if (ans > 1)
      ans = 1;
  }
  return ans;
}


// 'mx_to_ex' -----------------------------------------------------------------


[[cpp11::register]]
writable::doubles mx_to_ex_const(cpp11::doubles_matrix<> mx,
				 strings age_group_type,
				 doubles a0) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles ans(n);
  writable::doubles lx(n);
  double a00 = a0[0];
  bool has_a0 = a00 >= 0;
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double mxj = mx(i, j);
      if (mxj > 0) {
	double pxj = (is_age_0 && has_a0) ? 1 - make_qx_ax(mxj, a00, nx) : exp(-1 * nx * mxj);
	double lxj_old = lx[j];
	double lxj_new = pxj * lxj_old;
	ans[j] += (lxj_old - lxj_new) / mxj;
	lx[j] = lxj_new;
      }
      else {
	ans[j] += nx;
      }
    }
  }
  for (int j = 0; j < n; j++)
    ans[j] += 1.0 / mx(m - 1, j);
  return ans;
}


// 'mx_to_lx' -----------------------------------------------------------------


[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_cd(strings age_group_type,
				       cpp11::doubles_matrix<> mx,
				       strings sex,
				       doubles a0) {
  int m = mx.nrow();
  int n = mx.ncol();
  string sex0 = sex[0];
  writable::doubles_matrix<> ans(m, n);
  double a00 = a0[0];
  bool has_a0 =  a00 >= 0;
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_zero = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double mxj = mx(i, j);
      double axj = is_age_zero ? (has_a0 ? a00 : make_a0_cd(mxj, sex0)) : 0.5 * nx;
      double qxj = make_qx_ax(mxj, axj, nx);
      ans(i + 1, j) = (1 - qxj) * ans(i, j);
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_const(strings age_group_type,
					  cpp11::doubles_matrix<> mx,
					  doubles a0) {
  int m = mx.nrow();
  int n = mx.ncol();
  double a00 = a0[0];
  bool has_a0 = a00 > 0;
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double mxj = mx(i, j);
      double pxj = (has_a0 && is_age_0) ? 1 - make_qx_ax(mxj, a00, nx) : exp(-1 * nx * mxj);
      ans(i + 1, j) = pxj * ans(i, j);
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_hmd(strings age_group_type,
					cpp11::doubles_matrix<> mx,
					strings sex,
					doubles a0) {
  int m = mx.nrow();
  int n = mx.ncol();
  string sex0 = sex[0];
  double a00 = a0[0];
  bool has_a0 = a00 >= 0;
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double mxj = mx(i, j);
      double axj = is_age_0 ? (has_a0 ? a00 : make_a0_hmd(mxj, sex0)) : 0.5 * nx;
      double qxj = make_qx_ax(mxj, axj, nx);
      ans(i + 1, j) = (1 - qxj) * ans(i, j);
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_mid(strings age_group_type,
					cpp11::doubles_matrix<> mx,
					doubles a0) {
  int m = mx.nrow();
  int n = mx.ncol();
  double a00 = a0[0];
  bool has_a0 = a00 >= 0;
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_0 = age_group_type[i] == "0";
    double ax = (has_a0 && is_age_0) ? a00 : 0.5 * nx;
    for (int j = 0; j < n; j++) {
      double mxj = mx(i, j);
      double qxj = make_qx_ax(mxj, ax, nx);
      ans(i + 1, j) = (1 - qxj) * ans(i, j);
    }
  }
  return ans;
}


// 'mx_to_Lx' -----------------------------------------------------------------

[[cpp11::register]]
writable::doubles_matrix<> mx_to_Lx_const(strings age_group_type,
					  cpp11::doubles_matrix<> mx,
					  doubles a0) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  double a00 = a0[0];
  bool has_a0 = a00 >= 0;
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double mxj = mx(i, j);
      if (mxj > 0) {
	double pxj = (has_a0 && is_age_0) ? 1 - make_qx_ax(mxj, a00, nx) : exp(-1 * nx * mxj);
	double lxj_old = lx[j];
	double lxj_new = pxj * lxj_old;
	ans(i, j) = (lxj_old - lxj_new) / mxj;
	lx[j] = lxj_new;
      }
      else {
	ans(i, j) = nx;
      }
    }
  }
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = 1.0 / mx(m - 1, j);
  return ans;
}


// 'qx_to_Lx' -----------------------------------------------------------------

[[cpp11::register]]
writable::doubles_matrix<> qx_to_Lx_const(strings age_group_type,
					  cpp11::doubles_matrix<> qx,
					  doubles a0) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  writable::doubles mx_last(n);
  double a00 = a0[0];
  bool has_a0 = a00 >= 0;
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double qxj = qx(i, j);
      double lxj_old = lx[j];
      double dxj = qxj * lxj_old;
      double lxj_new = lxj_old - dxj;
      if (has_a0 && is_age_0) {
	ans(i, j) = lxj_new * nx + dxj * a00;
      }
      else {
	if (qxj > 0) {
	  if (qxj < 1) {
	    double mxj = -1 * log(1 - qxj) / nx;
	    ans(i, j) = dxj / mxj;
	  }
	  else {
	    ans(i, j) = 0;
	  }
	  lx[j] = lxj_new;
	}
	else {
	  ans(i, j) = nx;
	}
      }
      if (i == m - 2) {
	mx_last[j] = (qxj < 1) ? dxj / ans(i, j) : R_PosInf;
      }
    }
  }
  for (int j = 0; j < n; j++)
      ans(m - 1, j) = 1 / mx_last[j];
  return ans;
}


// 'qx_to_mx' -----------------------------------------------------------------

[[cpp11::register]]
writable::doubles_matrix<> qx_to_mx_const(strings age_group_type,
					  cpp11::doubles_matrix<> qx,
					  doubles a0) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  double a00 = a0[0];
  bool has_a0 = a00 >= 0;
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx = make_nx(age_group_type[i]);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double qxj = qx(i, j);
      double lxj_old = lx[j];
      double dxj = qxj * lxj_old;
      double lxj_new = lxj_old - dxj;
      if (has_a0 && is_age_0) {
	ans(i, j) = dxj / (lxj_new * nx + dxj * a00);
      }
      else {
	if (qxj > 0) {
	  if (qxj < 1) {
	    ans(i, j) = -1 * log(1 - qxj) / nx;;
	  }
	  else {
	    ans(i, j) = R_PosInf;
	  }
	  lx[j] = lxj_new;
	}
	else {
	  ans(i, j) = 0;
	}
      }
    }
  }
  for (int j = 0; j < n; j++) {
    double val = ans(m - 2, j);
    ans(m - 1, j) = val;
  }
  return ans;
}


  
// 'qx_to_lx' -----------------------------------------------------------------

[[cpp11::register]]
writable::doubles_matrix<> qx_to_lx(cpp11::doubles_matrix<> qx) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++)
    for (int j = 0; j < n; j++)
      ans(i + 1, j) = (1 - qx(i, j)) * ans(i, j);
  return ans;
}



