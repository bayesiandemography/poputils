#include <Rcpp.h>
using namespace Rcpp;


// HAS_TESTS
//' Calculate life expectancy based on
//' 'ax' and 5-year age groups
//'
//' Calculate life expectancy using an
//' ax-based method, with 5-year age groups.
//' In practice, the only ax-based method for calculating
//' life expectancy that works when all age groups
//' are 5-year is the "mid" method, ie the one
//' where  ax is 2.5 for all age groups except
//' the open age group.
//'
//' Wih this method, the probability of dying, qx,
//' can exceed 1. \code{lifeexp_ax_five}
//' adjusts the probability
//' downwards, with a warning.
//'
//' @param mx A matrix of mortality rates,
//' using 5-year age groups. 
//'
//' @return A vector of life expectancies,
//' with length equal to mx.nrow().
//'
//' @noRd
//[[Rcpp::export]]
NumericVector lifeexp_ax_five(NumericMatrix mx) {
  int n_val = mx.nrow();
  int n_age = mx.ncol();
  NumericVector ans(n_val);
  int n_q_outside_limit = 0;
  for (int i_val = 0; i_val < n_val; i_val++) {
    double lifeexp = 0;
    double l_prev = 1;
    double l_curr = 0;
    double L_curr;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_val, i_age);
      if (NumericVector::is_na(m_curr)) {
	lifeexp = NA_REAL;
	break;
      }
      if (m_curr < 0)
	stop("'mx' has negative value [%s]", m_curr);
      if (is_closed_age_group) {
	bool q_curr_inside_limit = (m_curr < 0.4);
	if (q_curr_inside_limit) {
	  double q_curr = 5 * m_curr / (1 + 2.5 * m_curr);
	  l_curr = (1 - q_curr) * l_prev;
	  L_curr = 2.5 * (l_prev + l_curr);
	}
	else {
	  L_curr = 2.5 * l_prev;
	  lifeexp += L_curr;
	  n_q_outside_limit += (m_curr > 0.4);
	  break;
	}
      }
      else {
	L_curr = l_prev / m_curr;
      }
      lifeexp += L_curr;
      l_prev = l_curr;
    }
    ans[i_val] = lifeexp;
  }
  if (n_q_outside_limit > 0)
    warning("estimated probability of dying 'qx' exceeded 1.0 in %d cell(s) : "
	    "adjusted downwards to 1.0", n_q_outside_limit);
  return ans;
}


// HAS_TESTS
//' Calculate life expectancy based on
//' 'ax' and abridged life table age groups
//'
//' Calculate life expectancy using ax-based
//' methods with abridged life table age groups, ie
//' 0, 1-4, 5-9, ...., A+. The minimum number
//' of age groups is 3.
//'
//' Argument \code{index_method} can be 1 (= "mid")
//' 2 (= "CD-Female") or 3 (= "CD-Male"). The
//' value for \code{index_method} affects
//' the way that a0 and a1 are calculated.
//' 'ax' equals 2.5 in all other age groups, apart from
//' A+.
//'
//' With this method, the probability of dying,
//' qx, can exceed 1.
//' \code{lifeexp_ax_lt} adjusts the estimated
//' probability downwards, with a warning.
//'
//' @param mx A matrix of mortality rates,
//' using life table age groups.
//' @param index_method Index number for method
//' for calculating 'a0' and 'a1'.
//'
//' @return A vector of life expectancies,
//' with length equal to mx.nrow().
//'
//' @noRd
//[[Rcpp::export]]
NumericVector lifeexp_ax_lt(NumericMatrix mx, int index_method) {
  int n_val = mx.nrow();
  int n_age = mx.ncol();
  NumericVector ans(n_val);
  int n_q_outside_limit = 0;
  for (int i_val = 0; i_val < n_val; i_val++) {
    double lifeexp = 0;
    double l_prev = 1;
    double l_curr = 0;
    double L_curr;
    double m0 = mx(i_val, 0);
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_val, i_age);
      if (NumericVector::is_na(m_curr)) {
	lifeexp = NA_REAL;
	break;
      }
      if (m_curr < 0)
	stop("'mx' has negative value [%s]", m_curr);
      if (is_closed_age_group) {
	bool is_age_0 = i_age == 0;
	bool is_age_1_4 = i_age == 1;
	bool is_m_finite = is_finite(NumericVector::create(m_curr))[0];
	if (is_age_0) {
	  double a0;
	  if (index_method == 1)      // mid
	    a0 = 0.5;
	  else if (index_method == 2) // CD-Female
	    a0 = (m0 >= 0.107) ? 0.35 : (0.053 + 2.8 * m0);
	  else if (index_method == 3) // CD-Male
	    a0 = (m0 >= 0.107) ? 0.33 : (0.045 + 2.684 * m0);
	  else
	    stop("unexpected value for 'index_method' : %d", index_method);
	  double q0 = is_m_finite ? (m0 / (1 + (1 - a0) * m0)) : 1;
	  l_curr =  (1 - q0) * l_prev;
	  L_curr = l_curr + a0 * (l_prev - l_curr);
	  if (!is_m_finite)
	    break;
	}
	else if (is_age_1_4) {
	  double a1;
	  if (index_method == 1)      // mid
	    a1 = 2;
	  else if (index_method == 2) // CD-Female
	    a1 = (m0 >= 0.107) ? 1.361 : (1.522 - 1.518 * m0);
	  else if (index_method == 3) // CD-Male
	    a1 = (m0 >= 0.107) ? 1.352 : (1.651 - 2.816 * m0);
	  else
	    stop("unexpected value for 'index_method' : %d", index_method);
	  double q1 = is_m_finite ? (4 * m_curr / (1 + (4 - a1) * m_curr)) : 1;
	  l_curr = (1 - q1) * l_prev;
	  L_curr = 4 * l_curr + a1 * (l_prev - l_curr);
	  if (!is_m_finite)
	    break;
	}
	else {
	  bool q_curr_inside_limit = (m_curr < 0.4);
	  if (q_curr_inside_limit) {
	    double q_curr = 5 * m_curr / (1 + 2.5 * m_curr);
	    l_curr = (1 - q_curr) * l_prev;
	    L_curr = 2.5 * (l_prev + l_curr);
	  }
	  else {
	    L_curr = 2.5 * l_prev;
	    lifeexp += L_curr;
	    n_q_outside_limit += (m_curr > 0.4);
	    break;
	  }
	}
      }
      else {
	L_curr = l_prev / m_curr;
      }
      lifeexp += L_curr;
      l_prev = l_curr;
    }
    ans[i_val] = lifeexp;
  }
  if (n_q_outside_limit > 0)
    warning("estimated probability of dying 'qx' exceeded 1.0 in %d cell(s) : "
	    "adjusted downwards to 1.0", n_q_outside_limit);
  return ans;
}







// HAS_TESTS
//' Calculate life expectancy based on constant
//' mortality rates and 5-year age groups
//'
//' Calculate life expectancy using the assumption
//' of constant mortality rates within each
//' age group, and with 5-year age groups.
//'
//' @param mx A matrix of mortality rates,
//' using 5-year age groups.
//'
//' @return A vector of life expectancies,
//' with length equal to mx.nrow().
//'
//' @noRd
//[[Rcpp::export]]
NumericVector lifeexp_const_five(NumericMatrix mx) {
  int n_val = mx.nrow();
  int n_age = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double L_curr;
    double l_prev = 1;
    double l_curr = 0;
    double lifeexp = 0;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_val, i_age);
      if (NumericVector::is_na(m_curr)) {
	lifeexp = NA_REAL;
	break;
      }
      if (m_curr < 0)
	stop("'mx' has negative value [%s]", m_curr);
      if (m_curr > 0) {
	if (is_closed_age_group) {
	  double p_curr = exp(-5 * m_curr);
	  if (p_curr <= 0) {
	    break;
	  }
	  l_curr = p_curr * l_prev;
	  L_curr = (l_prev - l_curr) / m_curr;
	}
	else
	  L_curr = l_prev / m_curr;
      }
      else {
	if (is_closed_age_group) {
	  l_curr = l_prev;
	  L_curr = 5 * l_curr;
	}
	else
	  L_curr = R_PosInf;
      }
      lifeexp += L_curr;
      l_prev = l_curr;
    }
    ans[i_val] = lifeexp;
  }
  return ans;
}


// HAS_TESTS
//' Calculate life expectancy based on constant
//' mortality rates and life table age groups
//'
//' Calculate life expectancy using the assumption
//' of constant mortality rates within each
//' age group, and with abridged life table age groups, ie
//' 0, 1-4, 5-9, ...., A+.
//'
//' @param mx A matrix of mortality rates,
//' using life table age groups.
//'
//' @return A vector of life expectancies,
//' with length equal to mx.nrow().
//'
//' @noRd
//[[Rcpp::export]]
NumericVector lifeexp_const_lt(NumericMatrix mx) {
  int n_val = mx.nrow();
  int n_age = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double L_curr;
    double l_prev = 1;
    double l_curr = 0;
    double lifeexp = 0;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      int n_curr = (i_age == 0) ? 1 : ((i_age == 1) ? 4 : 5);
      double m_curr = mx(i_val, i_age);
      if (NumericVector::is_na(m_curr)) {
	lifeexp = NA_REAL;
	break;
      }
      if (m_curr < 0)
	stop("'mx' has negative value [%s]", m_curr);
      if (m_curr > 0) {
	if (is_closed_age_group) {
	  double p_curr = exp(-1 * n_curr * m_curr);
	  if (p_curr <= 0)
	    break;
	  l_curr = p_curr * l_prev;
	  L_curr = (l_prev - l_curr) / m_curr;
	}
	else
	  L_curr = l_prev / m_curr;
      }
      else {
	if (is_closed_age_group) {
	  l_curr = l_prev;
	  L_curr = n_curr * l_curr;
	}
	else
	  L_curr = R_PosInf;
      }
      lifeexp += L_curr;
      l_prev = l_curr;
    }
    ans[i_val] = lifeexp;
  }
  return ans;
}


// HAS_TESTS
//' Calculate life expectancy based on constant
//' mortality rates and 1-year age groups
//'
//' Calculate life expectancy using the assumption
//' of constant mortality rates within each
//' age group, and with 1-year age groups.
//'
//' @param mx A matrix of mortality rates,
//' using 1-year age groups.
//'
//' @return A vector of life expectancies,
//' with length equal to mx.nrow().
//'
//' @noRd
//[[Rcpp::export]]
NumericVector lifeexp_const_single(NumericMatrix mx) {
  int n_val = mx.nrow();
  int n_age = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double L_curr;
    double l_prev = 1;
    double l_curr = 0;
    double lifeexp = 0;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_val, i_age);
      if (NumericVector::is_na(m_curr)) {
	lifeexp = NA_REAL;
	break;
      }
      if (m_curr < 0)
	stop("'mx' has negative value [%s]", m_curr);
      if (m_curr > 0) {
	if (is_closed_age_group) {
	  double p_curr = exp(-1 * m_curr);
	  if (p_curr <= 0) {
	    break;
	  }
	  l_curr = p_curr * l_prev;
	  L_curr = (l_prev - l_curr) / m_curr;
	}
	else
	  L_curr = l_prev / m_curr;
      }
      else {
	if (is_closed_age_group) {
	  l_curr = l_prev;
	  L_curr = l_curr;
	}
	else
	  L_curr = R_PosInf;
      }
      lifeexp += L_curr;
      l_prev = l_curr;
    }
    ans[i_val] = lifeexp;
  }
  return ans;
}
