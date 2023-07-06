#include <Rcpp.h>
using namespace Rcpp;

NumericVector le_ax_five(NumericMatrix mx);
NumericVector le_ax_lt(NumericMatrix mx, CharacterVector method);
NumericVector le_ax_single(NumericMatrix mx, CharacterVector method);
NumericVector le_const_five(NumericMatrix mx);
NumericVector le_const_lt(NumericMatrix mx);
NumericVector le_const_single(NumericMatrix mx);

//' Calculate life expectancy from mortality rates
//'
//' Workhorse function for R function .lifeexp
//' 
//' @param mx Mortality rates.
//' @param age_groups Type of age groups used.
//' @param method Method for converting calculating
//' life expectancy.
//'
//' @noRd
// [[Rcpp::export]]
NumericVector le(NumericMatrix mx,
		 CharacterVector age_groups,
		 CharacterVector method) {
  CharacterVector choices_age = CharacterVector::create("lt",
                                                        "single",
                                                        "five");
  CharacterVector choices_method = CharacterVector::create("const",
							   "mid",
							   "CD-Female",
							   "CD-Male",
							   "HMD-Female",
							   "HMD-Male");
  // --- check inputs ---
  // 'mx' has at least one row
  if (mx.nrow() == 0)
    stop("'mx' has 0 rows");
  // 'age_groups' has length 1
  if (age_groups.length() != 1)
    stop("'age_groups' has length %d", age_groups.length());
  // 'age_groups' has valid value
  if (is_na(match(age_groups, choices_age))[0])
    stop("unexpected value for 'age_groups' : %s", age_groups);
  // 'method' has length 1
  if (method.length() != 1)
    stop("'method' has length %d", method.length());
  // 'method' has valid value
  if (is_na(match(method, choices_method))[0])
    stop("unexpected value for 'method' : %s", method);
  // --- call appropriate helper function (or raise error) ---
  int n_val = mx.ncol();
  NumericVector ans(n_val);
  if (n_val == 0)
    return(ans);
  if (age_groups[0] == "lt") {
    if (method[0] == "const")
      ans = le_const_lt(mx);
    else if ((method[0] == "mid") ||
	     (method[0] == "CD-Female") ||
	     (method[0] == "CD-Male"))
      ans = le_ax_lt(mx, method);
    else
      stop("unexpected combination of 'age_groups' [%s] and 'method' [%s]",
	   age_groups, method);
  }
  else if (age_groups[0] == "single") {
    if (method[0] == "const")
      ans = le_const_single(mx);
    else
      ans = le_ax_single(mx, method);
  }
  else {
    if (method[0] == "const")
      ans = le_const_five(mx);
    else if (method[0] == "mid")
      ans =  le_ax_five(mx);
    else
      stop("unexpected combination of 'age_groups' [%s] and 'method' [%s]",
	   age_groups, method);
  }
  return ans;
}
	

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
//' With this method, the probability of dying, qx,
//' can exceed 1. \code{le_ax_five}
//' adjusts the probability
//' downwards.
//'
//' @param mx A matrix of mortality rates,
//' using 5-year age groups. 
//'
//' @return A vector of life expectancies,
//' with length equal to mx.nrow().
//'
//' @noRd
// [[Rcpp::export]]
NumericVector le_ax_five(NumericMatrix mx) {
  int n_age = mx.nrow();
  int n_val = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double lifeexp = 0;
    double l_prev = 1;
    double l_curr = 0;
    double L_curr;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_age, i_val);
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
  return ans;
}


// HAS_TESTS
//' Calculate life expectancy based on
//' 'ax' and abridged life table age groups
//'
//' Calculate life expectancy using ax-based
//' methods with abridged life table age groups, ie
//' 0, 1-4, 5-9, ...., A+. 
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
//' \code{le_ax_lt} adjusts the estimated
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
// [[Rcpp::export]]
NumericVector le_ax_lt(NumericMatrix mx, CharacterVector method) {
  int n_age = mx.nrow();
  int n_val = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double lifeexp = 0;
    double l_prev = 1;
    double l_curr = 0;
    double L_curr;
    double m0 = mx(0, i_val);
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_age, i_val);
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
	  if (method[0] == "mid")
	    a0 = 0.5;
	  else if (method[0] == "CD-Female")
	    a0 = (m0 >= 0.107) ? 0.35 : (0.053 + 2.8 * m0);
	  else if (method[0] == "CD-Male")
	    a0 = (m0 >= 0.107) ? 0.33 : (0.045 + 2.684 * m0);
	  else
	    stop("unexpected value for 'method' : %s", method);
	  double q0 = is_m_finite ? (m0 / (1 + (1 - a0) * m0)) : 1;
	  l_curr =  (1 - q0) * l_prev;
	  L_curr = l_curr + a0 * (l_prev - l_curr);
	  if (!is_m_finite) {
	    lifeexp += L_curr;
	    break;
	  }
	}
	else if (is_age_1_4) {
	  double a1;
	  if (method[0] == "mid")
	    a1 = 2;
	  else if (method[0] == "CD-Female")
	    a1 = (m0 >= 0.107) ? 1.361 : (1.522 - 1.518 * m0);
	  else if (method[0] == "CD-Male")
	    a1 = (m0 >= 0.107) ? 1.352 : (1.651 - 2.816 * m0);
	  else
	    stop("unexpected value for 'method' : %s", method);
	  double q1 = is_m_finite ? (4 * m_curr / (1 + (4 - a1) * m_curr)) : 1;
	  l_curr = (1 - q1) * l_prev;
	  L_curr = 4 * l_curr + a1 * (l_prev - l_curr);
	  if (!is_m_finite) {
	    lifeexp += L_curr;
	    break;
	  }
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
  return ans;
}


// HAS_TESTS
//' Calculate life expectancy based on
//' 'ax' and single-year age groups
//'
//' Calculate life expectancy using ax-based
//' methods with single-year age groups.
//'
//' Argument \code{index_method} can be 1 (= "mid")
//' 2 (= "CD-Female"), 3 (= "CD-Male"),
//' 4 (= "HMD-Female"), 5 (= "HMD-Male").
//' value for \code{index_method} affects
//' the way that a0 is calculated.
//' 'ax' equals 0.5 in all other age groups, apart from
//' A+.
//'
//' With this method, the probability of dying,
//' qx, can exceed 1.
//' \code{le_ax_lt} adjusts the estimated
//' probability downwards, with a warning.
//'
//' @param mx A matrix of mortality rates,
//' using single-year age groups.
//' @param index_method Index number for method
//' for calculating 'a0'.
//'
//' @return A vector of life expectancies,
//' with length equal to mx.nrow().
//'
//' @noRd
// [[Rcpp::export]]
NumericVector le_ax_single(NumericMatrix mx, CharacterVector method) {
  int n_age = mx.nrow();
  int n_val = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double lifeexp = 0;
    double l_prev = 1;
    double l_curr = 0;
    double L_curr;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_age, i_val);
      if (NumericVector::is_na(m_curr)) {
	lifeexp = NA_REAL;
	break;
      }
      if (m_curr < 0)
	stop("'mx' has negative value [%s]", m_curr);
      if (is_closed_age_group) {
	bool is_age_0 = i_age == 0;
	bool is_m_finite = is_finite(NumericVector::create(m_curr))[0];
	if (is_age_0) {
	  double m0 = m_curr;
	  double a0;
	  if (method[0] == "mid")
	    a0 = 0.5;
	  else if (method[0] == "CD-Female")
	    a0 = (m_curr >= 0.107) ? 0.35 : (0.053 + 2.8 * m_curr);
	  else if (method[0] == "CD-Male")
	    a0 = (m_curr >= 0.107) ? 0.33 : (0.045 + 2.684 * m_curr);
	  else if (method[0] == "HMD-Female")
	    a0 = ((m0 >= 0.06891) ? 0.31411 : (m0 >= 0.01724 ? (0.04667 + 3.88089 * m0) : (0.14903 - 2.05527 * m0)));
	  else if (method[0] == "HMD-Male")
	    a0 = (m0 >= 0.08307) ? 0.29915 : (m0 >= 0.023 ? (0.02832 + 3.26021 * m0) : (0.14929 - 1.99545 * m0));
	  else
	    stop("unexpected value for 'method' : %s", method);
	  double q0 = is_m_finite ? (m0 / (1 + (1 - a0) * m0)) : 1;
	  l_curr =  (1 - q0) * l_prev;
	  L_curr = l_curr + a0 * (l_prev - l_curr);
	  if (!is_m_finite) {
	    lifeexp += L_curr;
	    break;
	  }
	}
	else {
	  bool q_curr_inside_limit = (m_curr < 2);
	  if (q_curr_inside_limit) {
	    double q_curr = m_curr / (1 + 0.5 * m_curr);
	    l_curr = (1 - q_curr) * l_prev;
	    L_curr = 0.5 * (l_prev + l_curr);
	  }
	  else {
	    L_curr = 0.5 * l_prev;
	    lifeexp += L_curr;
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
// [[Rcpp::export]]
NumericVector le_const_five(NumericMatrix mx) {
  int n_age = mx.nrow();
  int n_val = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double L_curr;
    double l_prev = 1;
    double l_curr = 0;
    double lifeexp = 0;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_age, i_val);
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
// [[Rcpp::export]]
NumericVector le_const_lt(NumericMatrix mx) {
  int n_age = mx.nrow();
  int n_val = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double L_curr;
    double l_prev = 1;
    double l_curr = 0;
    double lifeexp = 0;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      int n_curr = (i_age == 0) ? 1 : ((i_age == 1) ? 4 : 5);
      double m_curr = mx(i_age, i_val);
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
// [[Rcpp::export]]
NumericVector le_const_single(NumericMatrix mx) {
  int n_age = mx.nrow();
  int n_val = mx.ncol();
  NumericVector ans(n_val);
  for (int i_val = 0; i_val < n_val; i_val++) {
    double L_curr;
    double l_prev = 1;
    double l_curr = 0;
    double lifeexp = 0;
    for (int i_age = 0; i_age < n_age; i_age++) {
      bool is_closed_age_group = i_age < n_age - 1;
      double m_curr = mx(i_age, i_val);
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
