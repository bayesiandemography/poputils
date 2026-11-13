// Wrappers for the functions in inst/include/poputils/logit_normal.hpp.
// Called by the exported developer functions in R/logit_normal.R,
// which check and recycle inputs.

#include "cpp11.hpp"
#include "cpp11/doubles.hpp"
#include <poputils/logit_normal.hpp>
#include <cmath>
using namespace cpp11;
namespace writable = cpp11::writable;


// HAS_TESTS
[[cpp11::register]]
writable::doubles logit_normal_mean_cpp11(doubles mu, doubles sigma) {
  const int n = mu.size();
  writable::doubles ans(n);
  for (int i = 0; i < n; i++) {
    double m = mu[i];
    double s = sigma[i];
    ans[i] = poputils::logit_normal_mean(m, s);
  }
  return ans;
}


// Checks that the fixed number of Newton steps reached the root.

// HAS_TESTS
[[cpp11::register]]
writable::doubles logit_normal_solve_mu_cpp11(doubles mean, doubles sigma, int n_iter) {
  const int n = mean.size();
  writable::doubles ans(n);
  for (int i = 0; i < n; i++) {
    double target = mean[i];
    double s = sigma[i];
    double mu = poputils::logit_normal_solve_mu(target, s, n_iter);
    double m, m_c, d;
    poputils::logit_normal_detail::logit_normal_sums(mu, s, m, m_c, d);
    double err = std::log(m) - std::log(m_c) - (std::log(target) - std::log(1 - target));
    if (!std::isfinite(mu) || std::abs(err) > 1e-8)
      cpp11::stop("failed to converge (element %d).", i + 1);
    ans[i] = mu;
  }
  return ans;
}
