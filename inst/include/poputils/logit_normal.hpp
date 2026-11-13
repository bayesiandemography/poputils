// logit_normal.hpp
//
// Mean of a logit-normal distribution, and the location parameter
// that gives a required mean.
//
// If X ~ N(mu, sigma^2), then P = invlogit(X) has a logit-normal
// distribution. Its mean has no closed form. Here it is calculated
// by Gauss-Hermite quadrature with a fixed set of 30 nodes:
//
//   E[P] = sum_k w_k invlogit(mu + sigma z_k),
//
// where z_k and w_k are nodes and weights for the standard normal
// (weights sum to 1). The relative error is below 1e-7 for
// sigma <= 2 and below 1e-3 for sigma <= 4, for any mu.
//
// Header-only and templated on the number type, so the same code
// serves poputils' own C++ (doubles, via cpp11) and other packages'
// C++, including TMB templates (automatic-differentiation types).
// Use from another package with 'LinkingTo: poputils' and
//
//   #include <poputils/logit_normal.hpp>
//
// No value-dependent branching
//   TMB records the operations once and reuses the record, so the
//   location is found with a fixed number of Newton steps, with no
//   test for convergence. Starting from the probit approximation
//   mu0 = logit(mean) * sqrt(1 + pi sigma^2 / 8), 4 steps reach
//   machine precision for means between 1e-6 and 1 - 1e-6 and
//   sigma <= 4. Callers working with doubles can check the result.
//
// Requirements are not checked: callers check inputs. sigma >= 0;
// 0 < mean < 1; |mu| + 10 sigma well below 700, so that exp() does
// not overflow.

#ifndef POPUTILS_LOGIT_NORMAL_HPP
#define POPUTILS_LOGIT_NORMAL_HPP

#include <cmath>

namespace poputils {

namespace logit_normal_detail {

const int n_gh = 30;

// Gauss-Hermite nodes for the standard normal.
const double gh_z[n_gh] = {
    -9.7062359973595154,
    -8.680837722732214,
    -7.8250517443528125,
    -7.055396866960292,
    -6.339997686869598,
    -5.6623818500828715,
    -5.0126005964865135,
    -4.3840203658980519,
    -3.7718944231592344,
    -3.1726346394204015,
    -2.5834021002292715,
    -2.0018586129564309,
    -1.4260056583741125,
    -0.85407335171097287,
    -0.28443876073621061,
    0.28443876073621061,
    0.85407335171097287,
    1.4260056583741125,
    2.0018586129564309,
    2.5834021002292715,
    3.1726346394204015,
    3.7718944231592344,
    4.3840203658980519,
    5.0126005964865135,
    5.6623818500828715,
    6.339997686869598,
    7.055396866960292,
    7.8250517443528125,
    8.680837722732214,
    9.7062359973595154
};

// Gauss-Hermite weights for the standard normal (sum to 1).
const double gh_w[n_gh] = {
    1.6408070081179493e-21,
    1.5855609449662895e-17,
    1.6240801299723703e-14,
    4.573425871326076e-12,
    5.1784594671897443e-10,
    2.8821751540476406e-08,
    8.909088868621229e-07,
    1.6579981630673304e-05,
    0.00019651294398482949,
    0.0015447073398660762,
    0.0082957475577233528,
    0.031111770183501447,
    0.082786836715621967,
    0.15804695320902049,
    0.21799997181557715,
    0.21799997181557715,
    0.15804695320902049,
    0.082786836715621967,
    0.031111770183501447,
    0.0082957475577233528,
    0.0015447073398660762,
    0.00019651294398482949,
    1.6579981630673304e-05,
    8.909088868621229e-07,
    2.8821751540476406e-08,
    5.1784594671897443e-10,
    4.573425871326076e-12,
    1.6240801299723703e-14,
    1.5855609449662895e-17,
    1.6408070081179493e-21
};

// Quadrature sums at location mu and scale sigma:
// 'mean' = E[P], 'mean_c' = E[1 - P] (summed separately, so that
// both are accurate near 0), 'dens' = E[P (1 - P)] = dE[P]/dmu.
template <class T>
void logit_normal_sums(const T& mu, const T& sigma, T& mean, T& mean_c, T& dens) {
  using std::exp;
  mean = 0;
  mean_c = 0;
  dens = 0;
  for (int k = 0; k < n_gh; k++) {
    T x = mu + sigma * gh_z[k];
    T p = 1 / (1 + exp(-x));
    T q = 1 / (1 + exp(x));
    mean += gh_w[k] * p;
    mean_c += gh_w[k] * q;
    dens += gh_w[k] * p * q;
  }
}

} // namespace logit_normal_detail


// Mean of invlogit(X), X ~ N(mu, sigma^2).
template <class T>
T logit_normal_mean(const T& mu, const T& sigma) {
  T mean, mean_c, dens;
  logit_normal_detail::logit_normal_sums(mu, sigma, mean, mean_c, dens);
  return mean;
}


// Location mu such that invlogit(X), X ~ N(mu, sigma^2), has mean
// 'mean'. Newton steps on logit(E[P]) - logit(mean), 'n_iter' of
// them, from the probit approximation.
template <class T>
T logit_normal_solve_mu(const T& mean, const T& sigma, int n_iter) {
  using std::log;
  using std::sqrt;
  const double pi = 3.14159265358979323846;
  T target = log(mean) - log(1 - mean);
  T mu = target * sqrt(1 + pi * sigma * sigma / 8);
  for (int i = 0; i < n_iter; i++) {
    T m, m_c, d;
    logit_normal_detail::logit_normal_sums(mu, sigma, m, m_c, d);
    T f = log(m) - log(m_c) - target;
    T fp = d * (1 / m + 1 / m_c);
    mu = mu - f / fp;
  }
  return mu;
}

} // namespace poputils

#endif
