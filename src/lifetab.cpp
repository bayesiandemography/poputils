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

double make_a14_cd(double m0, string sex) {
  if (sex == "Female")
    return (m0 >= 0.107) ? 1.361 : (1.522 - 1.518 * m0);
  else if (sex == "Male")
    return (m0 >= 0.107) ? 1.352 : (1.651 - 2.816 * m0);
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
    return R_PosInf;
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


// 'check_ax_le_nx' -----------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::logicals check_ax_le_nx(doubles ax, strings age_group_type) {
  int n = ax.size();
  writable::logicals ans(n);
  for (int i = 0; i < n; i++) {
    double ax_i = ax[i];
    if (isnan(ax_i))
      ans[i] = true;
    else {
      int nx_i = make_nx(age_group_type[i]);
      ans[i] = (ax_i <= nx_i);
    }
  }
  return ans;
}


// 'lx_to_dx' -----------------------------------------------------------------

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


// 'lx_to_qx' -----------------------------------------------------------------

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


// 'mx_to_ex' -----------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles mx_to_ex_const(cpp11::doubles_matrix<> mx,
				 strings age_group_type,
				 doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles ans(n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans[j] = NA_REAL;
      }
      else if (mx_ij > 0) {
	double lx_ij_old = lx[j];
	double px_ij = has_ax_i ? 1 - make_qx_ax(mx_ij, ax_i, nx_i) : exp(-1 * nx_i * mx_ij);
	double lx_ij_new = px_ij * lx_ij_old;
	ans[j] += (lx_ij_old - lx_ij_new) / mx_ij;
	lx[j] = lx_ij_new;
      }
      else {
	ans[j] += nx_i;
      }
    }
  }
  for (int j = 0; j < n; j++)
    ans[j] += lx[j] / mx(m - 1, j);
  return ans;
}

// HAS_TESTS
[[cpp11::register]]
writable::doubles mx_to_ex_cd(cpp11::doubles_matrix<> mx,
			      strings age_group_type,
			      strings sex,
			      doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  string sex0 = sex[0];
  writable::doubles ans(n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    bool is_age_0 = age_group_type[i] == "0";
    bool is_age_1_4 = age_group_type[i] == "1-4";
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans[j] = NA_REAL;
      }
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else {
	  if (is_age_0)
	    ax_ij = make_a0_cd(mx_ij, sex0);
	  else if (is_age_1_4) {
	    double mx_0j = mx(0, j);
	    ax_ij = make_a14_cd(mx_0j, sex0);
	  }
	  else
	    ax_ij = 0.5 * nx_i;
	}
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double lx_ij_old = lx[j];
	double lx_ij_new = (1 - qx_ij) * lx_ij_old;
	double Lx_ij = lx_ij_new * nx_i + (lx_ij_old - lx_ij_new) * ax_ij;
	ans[j] += Lx_ij;
	lx[j] = lx_ij_new;
      }
      else {
	ans[j] += nx_i;
      }
    }
  }
  for (int j = 0; j < n; j++)
    ans[j] += lx[j] / mx(m - 1, j);
  return ans;
}

// HAS_TESTS
[[cpp11::register]]
writable::doubles mx_to_ex_hmd(cpp11::doubles_matrix<> mx,
			       strings age_group_type,
			       strings sex,
			       doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  string sex0 = sex[0];
  writable::doubles ans(n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans[j] = NA_REAL;
      }
      else if (mx_ij > 0) {
	double ax_ij;
	if (has_ax_i)
	  ax_ij = ax_i;
	else {
	  if (is_age_0)
	    ax_ij = make_a0_hmd(mx_ij, sex0);
	  else
	    ax_ij = 0.5 * nx_i;
	}
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	double lx_ij_old = lx[j];
	double lx_ij_new = (1 - qx_ij) * lx_ij_old;
	double Lx_ij = lx_ij_new * nx_i + (lx_ij_old - lx_ij_new) * ax_ij;
	ans[j] += Lx_ij;
	lx[j] = lx_ij_new;
      }
      else {
	ans[j] += nx_i;
      }
    }
  }
  for (int j = 0; j < n; j++)
    ans[j] += lx[j] / mx(m - 1, j);
  return ans;
}


// 'mx_to_lx' -----------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_cd(cpp11::doubles_matrix<> mx,
				       strings age_group_type,
				       strings sex,
				       doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  string sex0 = sex[0];
  writable::doubles_matrix<> ans(m, n);
  double ax_ij;
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    bool is_age_0 = age_group_type[i] == "0";
    bool is_age_1_4 = age_group_type[i] == "1-4";
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij))
	ans(i + 1, j) = NA_REAL;
      else {
	if (has_ax_i)
	  ax_ij = ax_i;
	else {
	  if (is_age_0)
	    ax_ij = make_a0_cd(mx_ij, sex0);
	  else if (is_age_1_4) {
	    double mx_0j = mx(0, j);
	    ax_ij = make_a14_cd(mx_0j, sex0);
	  }
	  else
	    ax_ij = 0.5 * nx_i;
	}
	double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
	ans(i + 1, j) = (1 - qx_ij) * ans(i, j);
      }
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_const(cpp11::doubles_matrix<> mx,
					  strings age_group_type,
					  doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      double px_ij = has_ax_i ? 1 - make_qx_ax(mx_ij, ax_i, nx_i) : exp(-1 * nx_i * mx_ij);
      ans(i + 1, j) = px_ij * ans(i, j);
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_hmd(cpp11::doubles_matrix<> mx,
					strings age_group_type,
					strings sex,
					doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  string sex0 = sex[0];
  writable::doubles_matrix<> ans(m, n);
  double ax_ij;
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    bool is_age_0 = age_group_type[i] == "0";
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (has_ax_i)
	ax_ij = ax_i;
      else
	ax_ij = is_age_0 ? make_a0_hmd(mx_ij, sex0) : 0.5 * nx_i;
      double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
      ans(i + 1, j) = (1 - qx_ij) * ans(i, j);
    }
  }
  return ans;
}


[[cpp11::register]]
writable::doubles_matrix<> mx_to_lx_mid(cpp11::doubles_matrix<> mx,
					strings age_group_type,
					doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  for (int j = 0; j < n; j++)
    ans(0, j) = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    double ax_ij = has_ax_i ? ax_i : 0.5 * nx_i;
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      double qx_ij = make_qx_ax(mx_ij, ax_ij, nx_i);
      ans(i + 1, j) = (1 - qx_ij) * ans(i, j);
    }
  }
  return ans;
}


// 'mx_to_Lx' -----------------------------------------------------------------

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> mx_to_Lx_const(cpp11::doubles_matrix<> mx,
					  strings age_group_type,
					  doubles ax) {
  int m = mx.nrow();
  int n = mx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double mx_ij = mx(i, j);
      if (isnan(mx_ij)) {
	ans(i, j) = NA_REAL;
	lx[j] = NA_REAL;
      }
      else if (mx_ij > 0) {
	double px_ij = has_ax_i ? 1 - make_qx_ax(mx_ij, ax_i, nx_i) : exp(-1 * nx_i * mx_ij);
	double lx_ij_old = lx[j];
	double lx_ij_new = px_ij * lx_ij_old;
	ans(i, j) = (lx_ij_old - lx_ij_new) / mx_ij;
	lx[j] = lx_ij_new;
      }
      else {
	ans(i, j) = nx_i;
      }
    }
  }
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = lx[j] / mx(m - 1, j);
  return ans;
}


// 'qx_to_Lx' -----------------------------------------------------------------

// 'qx' does not allow calculation of 'mx' for open age group,
// so we assume that 'mx' for the open age group is identical
// to 'mx' for the second-to-oldest age group

// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> qx_to_Lx_const(cpp11::doubles_matrix<> qx,
					  strings age_group_type,
					  doubles ax) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  writable::doubles mx_last(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    bool is_second_oldest = i == m - 2;
    for (int j = 0; j < n; j++) {
      double qx_ij = qx(i, j);
      if (isnan(qx_ij)) {
	ans(i, j) = NA_REAL;
	lx[j] = NA_REAL;
	if (is_second_oldest)
	  mx_last[j] = NA_REAL;
      }
      else {
	double lx_ij_old = lx[j];
	double dx_ij = qx_ij * lx_ij_old;
	double lx_ij_new = lx_ij_old - dx_ij;
	if (has_ax_i) {
	  ans(i, j) = lx_ij_new * nx_i + dx_ij * ax_i;
	}
	else {
	  if (qx_ij > 0) {
	    double mx_ij = -1 * log(1 - qx_ij) / nx_i;
	    ans(i, j) = dx_ij / mx_ij;
	    lx[j] = lx_ij_new;
	  }
	  else {
	    ans(i, j) = nx_i;
	  }
	}
	lx[j] = lx_ij_new;
	if (is_second_oldest) {
	  double mx_ij = (ans(i, j) > 0) ? (dx_ij / ans(i, j)) : R_PosInf;
	  mx_last[j] = mx_ij;
	}
      }
    }
  }
  for (int j = 0; j < n; j++)
    ans(m - 1, j) = lx[j] / mx_last[j];
  return ans;
}


// 'qx_to_mx' -----------------------------------------------------------------

// 'qx' does not allow calculation of 'mx' for open age group,
// so we assume that 'mx' for the open age group is identical
// to 'mx' for the second-to-oldest age group



// HAS_TESTS
[[cpp11::register]]
writable::doubles_matrix<> qx_to_mx_const(cpp11::doubles_matrix<> qx,
					  strings age_group_type,
					  doubles ax) {
  int m = qx.nrow();
  int n = qx.ncol();
  writable::doubles_matrix<> ans(m, n);
  writable::doubles lx(n);
  for (int j = 0; j < n; j++)
    lx[j] = 1;
  for (int i = 0; i < m - 1; i++) {
    int nx_i = make_nx(age_group_type[i]);
    double ax_i = ax[i];
    bool has_ax_i = !isnan(ax_i);
    for (int j = 0; j < n; j++) {
      double qx_ij = qx(i, j);
      if (isnan(qx_ij)) {
	ans(i, j) = NA_REAL;
	lx[j] = NA_REAL;
      }
      else {
	double lx_ij_old = lx[j];
	double dx_ij = qx_ij * lx_ij_old;
	double lx_ij_new = lx_ij_old - dx_ij;
	if (has_ax_i) {
	  ans(i, j) = dx_ij / (lx_ij_new * nx_i + dx_ij * ax_i);
	}
	else {
	  ans(i, j) = -1 * log(1 - qx_ij) / nx_i;
	}
	lx[j] = lx_ij_new;
      }
    }
  }
  for (int j = 0; j < n; j++) {
    double mx_ij = ans(m - 2, j);
    ans(m - 1, j) = mx_ij;
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



