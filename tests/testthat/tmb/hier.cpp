// Test model for the TMB layer (R/tmb.R): normal hierarchical model.
// With log_sd_u and log_sd_y fixed (map), the posterior of (mu, u) is
// exactly Gaussian, so the Laplace approximation and TMB's joint
// precision are exact.
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_IVECTOR(group);
  PARAMETER(mu);
  PARAMETER(log_sd_u);
  PARAMETER(log_sd_y);
  PARAMETER_VECTOR(u);
  Type ans = 0;
  ans -= dnorm(u, mu, exp(log_sd_u), true).sum();
  for (int i = 0; i < y.size(); i++)
    ans -= dnorm(y[i], u[group[i]], exp(log_sd_y), true);
  ans -= dnorm(log_sd_u, Type(0), Type(1), true);
  ans -= dnorm(log_sd_y, Type(0), Type(1), true);
  return ans;
}
