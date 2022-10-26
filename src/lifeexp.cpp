#include <Rcpp.h>
using namespace Rcpp;

NumericVector lifeexp_ax_five(NumericMatrix mx);
NumericVector lifeexp_ax_lt(NumericMatrix mx, CharacterVector method);
NumericVector lifeexp_ax_single(NumericMatrix mx, CharacterVector method);
NumericVector lifeexp_const_five(NumericMatrix mx);
NumericVector lifeexp_const_lt(NumericMatrix mx);
NumericVector lifeexp_const_single(NumericMatrix mx);

//' Calculate life expectancy from mortality rates
//'
//' Given mortality rates, a description of the age groups,
//' and a calculation method, derive life expectancies.
//'
//' Mortality rates \code{mx} are held in a matrix,
//' with one set of age-specific rates per row.
//'
//' There are three choices for argument \code{age_groups}:
//' \describe{
//'   \item{\code{"lt"}}{Life table age groups
//'         \code{"0", "1-4", "5-9", \dots, "A+"}}
//'   \item{\code{"single"}}{\code{"0", "1", "2", \dots, "A+"}}
//'   \item{\code{"five"}}{\code{"0-4", "5-9", \dots, "A+"}}
//' }
//' The last interval is always assumed to be open, meaning that
//' it includes everyone over a certain age.
//'
//' There are six choices for the \code{method} argument.
//' Each method makes different assumptions about
//' the shape of mortality rates, the survival curve,
//' or the average of deaths within each age interval:
//' \describe{
//'   \item{\code{"const"}}{Mortality rates are constant
//'         within each age group; equivalently, the
//'         life table function \code{lx} is an exponential
//'         curve within each age interval.}
//'   \item{\code{"mid"}}{On average, people die half way
//'         through each age group; equivalently, the
//'         life table function \code{lx} is a straight
//'         line within each age group.}
//'   \item{\code{"CD-Female"}, \code{"CD-Male"}}{As for
//'         \code{"mid"}, except that the average age at
//'         which infants die (and, if \code{age_groups} is
//'         \code{"lt"}, the average age at which
//'         children aged 1-4 die), is determined by
//'         formulas developed by Coale and Demeny (1983),
//'         and reported in Preston et al (2001).}
//'   \item{\code{"HMD-Female"}, \code{"HMD-Male"}}{The
//'         approach used in the Human Mortality Database.
//'         As for \code{"mid"}, except that the average
//'         age at which infants die is determined by
//'         formulas developed by Andreev and Kingkade (2015),
//'         and reported in Wilmoth et al (2019).}
//' }
//' Methods \code{"const"} and \code{"mid"} can be used
//' with any sex/gender.
//' Methods \code{"CD-Female"} and \code{"HMD-Female"}
//' should only be used for mortality rates for females,
//' and \code{"CD-Female"} and \code{"HMD-Male"}
//' should only be used for mortality rates for males.
//'
//' Some methods cannot be applied to some ways of
//' defininge age groups. The permitted combinations are:
//' 
//' |                      | \code{"lt"} | \code{"single"} | \code{"five"} |
//' |:---------------------|-------------|-----------------|---------------|
//' | \code{"const"}       | Yes         | Yes             | Yes           |
//' | \code{"mid"}         | Yes         | Yes             | Yes           |
//' | \code{"CD-Female"}   | Yes         | Yes             |               |
//' | \code{"CD-Male"}     | Yes         | Yes             |               |
//' | \code{"HMD-Female"}  |             | Yes             |               |
//' | \code{"HMD-Male"}    |             | Yes             |               |
//'
//'
//' @param mx Mortality rates. A matrix of non-negative values.
//' Must have at least one column.
//' @param age_groups Type of age groups used. Choices are
//' \code{"lt"}, \code{"single"}, and \code{"five"}.
//' @param method Method for converting calculating
//' life expectancy. Choices are
//' \code{"const"}, \code{"mid"},
//' \code{"CD-Female"}, \code{"CD-Male"},
//' \code{"HMD-Female"}, and \code{"HMD-Male"}.
//'
//' @return A numeric vector with length \code{nrow(mx)}.
//'
//' @examples
//' mx <- matrix(c(0.010, 0.002, 0.070, 0.200,
//'                0.011, 0.003, 0.072, 0.210),
//'              nrow = 2)
//'
//' lifeexp(mx, age_groups = "lt", method = "mid")
//' lifeexp(mx, age_groups = "lt", method = "CD-Female")
//' lifeexp(mx, age_groups = "single", method = "CD-Female")
//' lifeexp(mx, age_groups = "single", method = "const")
//' @md
//'
//' @export
// [[Rcpp::export]]
NumericVector lifeexp(NumericMatrix mx,
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
  // 'mx' has at least one column
  if (mx.ncol() == 0)
    stop("'mx' has 0 columns");
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
  int n_val = mx.nrow();
  NumericVector ans(n_val);
  if (n_val == 0)
    return(ans);
  if (age_groups[0] == "lt") {
    if (method[0] == "const")
      ans = lifeexp_const_lt(mx);
    else if ((method[0] == "mid") ||
	     (method[0] == "CD-Female") ||
	     (method[0] == "CD-Male"))
      ans = lifeexp_ax_lt(mx, method);
    else
      stop("unexpected combination of 'age_groups' [%s] and 'method' [%s]",
	   age_groups, method);
  }
  else if (age_groups[0] == "single") {
    if (method[0] == "const")
      ans = lifeexp_const_single(mx);
    else
      ans = lifeexp_ax_single(mx, method);
  }
  else {
    if (method[0] == "const")
      ans = lifeexp_const_five(mx);
    else if (method[0] == "mid")
      ans =  lifeexp_ax_five(mx);
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
// [[Rcpp::export]]
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
// [[Rcpp::export]]
NumericVector lifeexp_ax_lt(NumericMatrix mx, CharacterVector method) {
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
//' \code{lifeexp_ax_lt} adjusts the estimated
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
NumericVector lifeexp_ax_single(NumericMatrix mx, CharacterVector method) {
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
	    n_q_outside_limit += (m_curr > 2);
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
// [[Rcpp::export]]
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
// [[Rcpp::export]]
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
// [[Rcpp::export]]
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
