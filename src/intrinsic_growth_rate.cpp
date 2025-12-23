#include <cpp11.hpp>
#include <cmath>
#include <limits>

using cpp11::doubles;

[[cpp11::register]]
double intrinsic_growth_rate_cpp11(doubles mx,
				   doubles Lx,
				   doubles age_mid,
				   int max_iter,
				   double tol,
				   double deriv_tol) {
  const int n = mx.size(); // assume mx, Lx, age_mid same length
  // calculate initial value r0 = log(nrr) / mean_age_repr
  double nrr = 0.0;
  for (int i = 0; i < n; ++i) {
    nrr += mx[i] * Lx[i];
  }
  if (!std::isfinite(nrr) || nrr <= 0.0) {
    cpp11::stop("nrr = sum(mx * Lx) must be positive and finite.");
  }
  double mean_age_num = 0.0;
  for (int i = 0; i < n; ++i) {
    mean_age_num += age_mid[i] * mx[i] * Lx[i];
  }
  double mean_age = mean_age_num / nrr;
  if (!std::isfinite(mean_age) || mean_age <= 0.0) {
    cpp11::stop("mean age of reproduction must be positive and finite.");
  }
  double r0 = std::log(nrr) / mean_age;
  // Newton-Raphson
  for (int iter = 0; iter < max_iter; ++iter) {
    double f = -1.0;
    double fp = 0.0;
    for (int i = 0; i < n; ++i) {
      const double a = age_mid[i];
      const double w = mx[i] * Lx[i];
      const double ea = std::exp(-r0 * a);
      f += ea * w;
      fp += (-a) * ea * w;
    }
    if (!std::isfinite(fp) || std::abs(fp) < deriv_tol) {
      cpp11::stop("failed to converge: derivative too small / non-finite.");
    }
    const double r1 = r0 - f / fp;
    if (!std::isfinite(r1)) {
      cpp11::stop("failed to converge: non-finite update.");
    }
    if (std::abs(r1 - r0) < tol) {
      return r1;
    }
    r0 = r1;
  }
  cpp11::stop("failed to converge: max_iter reached.");
  return std::numeric_limits<double>::quiet_NaN();
}
