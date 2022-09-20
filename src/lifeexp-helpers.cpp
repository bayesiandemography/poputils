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
  int n_above_1 = 0;
  for (int i_val = 0; i_val < n_val; i_val++) {
    double lifeexp = 0;
    double l_prev = 1;
    double l_curr = 0;
    double L_curr;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_val, i_age);
      if (is_closed_age_group) {
	double q_curr = 5 * m_curr / (1 + 2.5 * m_curr); // assumes width is 5 and ax is 2.5
	if (q_curr > 1) {
	  q_curr = 1;
	  n_above_1++;
	}
	l_curr = (1 - q_curr) * l_prev;
	L_curr = 2.5 * (l_prev + l_curr); // assumes width is 5 and ax is 2.5
      }
      else {
	L_curr = l_prev / m_curr;
      }
      lifeexp += L_curr;
      l_prev = l_curr;
    }
    ans[i_val] = lifeexp;
  }
  if (n_above_1 > 0L)
    warning("estimated probability of dying 'qx' exceeded 1.0 in %d cell(s) : "
	    "adjusted downwards to 1.0", n_above_1);
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
