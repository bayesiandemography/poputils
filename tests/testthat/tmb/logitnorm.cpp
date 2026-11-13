// Test template: logit_normal.hpp with automatic differentiation.
// Objective: the location giving a logit-normal mean of 'mean'.
#include <TMB.hpp>
#include <poputils/logit_normal.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  PARAMETER(mean);
  PARAMETER(sigma);
  Type mu = poputils::logit_normal_solve_mu(mean, sigma, 10);
  Type m = poputils::logit_normal_mean(mu, sigma);
  REPORT(mu);
  REPORT(m);
  Type mean_check = m;
  REPORT(mean_check);
  Type ans = mu;
  return ans;
}
