#include "cpp11.hpp"
#include "cpp11/doubles.hpp"
#include "cpp11/matrix.hpp"
using namespace cpp11;
using namespace std;
namespace writable = cpp11::writable;


// Helper functions -----------------------------------------------------------

double make_nx(string age) {
  if (age == "0")
    return 1.0;
  if (age == "1-4")
    return 4.0;
  if (age == "single")
    return 1.0;
  if (age == "five")
    return 5.0;
  if (age == "open")
    return R_PosInf;
  stop("Internal error: Invalid value for 'age'.");
}

// HAS_TESTS
[[cpp11::register]]
writable::logicals is_ax_le_nx(doubles ax,
			       strings age_group_categ) {
  int n = ax.size();
  writable::logicals ans(n);
  for (int i = 0; i < n; i++) {
    double ax_i = ax[i];
    if (isnan(ax_i))
      ans[i] = true;
    else {
      string age = age_group_categ[i];
      double nx_i = make_nx(age);
      ans[i] = (ax_i <= nx_i);
    }
  }
  return ans;
}

double make_qx_ax(double mx,
		  double ax,
		  double nx) {
  double ans = 1;
  if (isfinite(mx) && isfinite(nx)) {
    ans = (nx * mx) / (1 + (nx - ax) * mx);
    if (ans > 1)
      ans = 1;
  }
  return ans;
}


// make_ax_ij --------------------------------------------------------------------

double make_ax_ij_const(double mx,
			double nx) {
  double num = 1 - (nx * mx + 1) * exp(-nx * mx);
  double den = mx * (1 - exp(-nx * mx));
  return num / den;
}

double make_ax_ij_closed(double mx,
			 double nx,
			 string method) {
  if (method == "constant")
    return make_ax_ij_const(mx, nx);
  else if (method == "linear")
    return 0.5 * nx;
  else
    stop("Internal error: Invalid value for 'method'.");
}

double make_ax_ij_open(double mx,
		       string method) {
  if (method == "constant")
    return 1 / mx;
  else
    stop("Internal error: Invalid value for 'method'.");
}


double make_ax_ij_infant(double m0,
			 string sex,
			 string method) {
  if (method == "CD") {
    if (sex == "Female")
      return (m0 >= 0.107) ? 0.35 : (0.053 + 2.8 * m0);
    else if (sex == "Male")
      return (m0 >= 0.107) ? 0.33 : (0.045 + 2.684 * m0);
    else {
      stop("Internal error: Invalid value for 'sex'.");
    }
  }
  else if (method == "constant") {
    return make_ax_ij_const(m0, 1.0);
  }
  else if (method == "HMD") {
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
  else if (method == "linear") {
    return 0.5;
  }
  else {
    stop("Internal error: Invalid value for 'sex'.");
  }
}    
  
double make_ax_ij_child(double mx,
			double m0,
			string sex,
			string method) {
  if (method == "CD") {
    if (sex == "Female")
      return (m0 >= 0.107) ? 1.361 : (1.522 - 1.518 * m0);
    else if (sex == "Male")
      return (m0 >= 0.107) ? 1.352 : (1.651 - 2.816 * m0);
    else {
      stop("Internal error: Invalid value for 'sex'.");
    }
  }
  else if (method == "constant") {
    return make_ax_ij_const(mx, 4.0);
  }
  else if (method == "linear") {
    return 2.0;
  }
  else {
    stop("Internal error: Invalid value for 'method'.");
  }
}

double make_ax_ij(double mx,
		  double m0,
		  string age,
		  string sex,
		  strings methods) {
  if (age == "0")
    return make_ax_ij_infant(m0, sex, methods[0]);
  else if (age == "1-4")
    return make_ax_ij_child(mx, m0, sex, methods[1]);
  else if (age == "single")
    return make_ax_ij_closed(mx, 1.0, methods[2]);
  else if (age == "five")
    return make_ax_ij_closed(mx, 5.0, methods[2]);
  else if (age == "open")
    return make_ax_ij_open(mx, methods[3]);
  else
    stop("Internal error: Invalid value for 'method'.");
}


// 'Lx_to' --------------------------------------------------------------------

// HAS_TESTS
// Assumes that radix is 1
[[cpp11::register]]
writable::doubles_matrix<> Lx_to_ex(cpp11::doubles_matrix<> Lx) {
  int m = Lx.nrow();
  int n = Lx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = Lx(m - 1, j);
  for (int i = m - 2; i >= 0; i--) {
    for (int j = 0; j < n; j++) {
      double ex_end = ans(i + 1, j);
      ans(i, j) = Lx(i, j) + ex_end;
    }
  }
  return ans;
}


// 'lx_to' --------------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> lx_to_dx(cpp11::doubles_matrix<> lx) {
  int m = lx.nrow();
  int n = lx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int i = 0; i < m - 1; i++) {
    for (int j = 0; j < n; j++) {
      ans(i, j) = lx(i, j) - lx(i + 1, j);
    }
  }
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = lx(m - 1, j);
  return ans;
}

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> lx_to_qx(cpp11::doubles_matrix<> lx) {
  int m = lx.nrow();
  int n = lx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int i = 0; i < m - 1; i++) {
    for (int j = 0; j < n; j++) {
      double px_ij = lx(i + 1, j) / lx(i, j);
      ans(i, j) = 1 - px_ij;
    }
  }
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = 1;
  return ans;
}


// 'mx_to' --------------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> mx_to_ex(cpp11::doubles_matrix<> mx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(1, n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 0.0;
  for (int j = 0; j < n; j++)
    lx[j] = 1.0;
  for (int i = 0; i < m; i++) {
    string age = age_group_categ[i];
    double nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans(0, j) = NA_REAL;
      }
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij(mx_ij, mx(0, j), age, sex[0], methods);
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double lx_ij_start = lx[j];
	double px_ij = 1 - qx_ij;
	double lx_ij_end = px_ij * lx_ij_start;
	double dx_ij = lx_ij_start - lx_ij_end;
	double Lx_ij = dx_ij * ax_ij;
	if (isfinite(nx_i))
	  Lx_ij += lx_ij_end * nx_i;
	ans(0, j) += Lx_ij;
	lx[j] = lx_ij_end;
      }
      else {
	ans(0, j) += lx[j] * nx_i;
      }
    }
  }
  return ans;
}
	       
// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx(cpp11::doubles_matrix<> mx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1.0;
  for (int i = 0; i < m - 1 ; i++) {
    string age = age_group_categ[i];
    double nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij))
	ans(i + 1, j) = NA_REAL;
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij(mx_ij, mx(0, j), age, sex[0], methods);
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double px_ij = 1 - qx_ij;
	double lx_ij_start = ans(i, j);
	double lx_ij_end = px_ij * lx_ij_start;
	ans(i + 1, j) = lx_ij_end;
      }
      else {
	double lx_ij_end = ans(i, j);
	ans(i + 1, j) = lx_ij_end;
      }
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> mx_to_Lx(cpp11::doubles_matrix<> mx,
				    strings age_group_categ,
				    strings sex,
				    doubles ax,
				    strings methods) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1.0;
  for (int i = 0; i < m; i++) {
    string age = age_group_categ[i];
    int nx_i = make_nx(age);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans(i, j) = NA_REAL;
	lx[j] = NA_REAL;
      }
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else
	  ax_ij = make_ax_ij(mx_ij, mx(0, j), age, sex[0], methods);
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double px_ij = 1 - qx_ij;
	double lx_ij_start = lx[j];
	double lx_ij_end = px_ij * lx_ij_start;
	double dx_ij = lx_ij_start - lx_ij_end;
	double Lx_ij = dx_ij * ax_ij;
	if (isfinite(nx_i))
	  Lx_ij += lx_ij_end * nx_i;
	ans(i, j) = Lx_ij;
	lx[j] = lx_ij_end;
      }
      else {
	ans(i, j) = lx[j] * nx_i;
      }
    }
  }
  return ans;
}

  
// 'qx_to_lx' -----------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> qx_to_lx(cpp11::doubles_matrix<> qx) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    for (int j = 0; j < n; j++) {
      double px_ij = 1 - qx(i, j);
      ans(i + 1, j) = px_ij * ans(i, j);
    }
  }
  return ans;
}
