#include "eigen1.h"
#include "util.h"
#include <R.h>

bool power_iteration(int n, const double *m, int max_iterations,
                     double tolerance, double *x) {
  double *new_x = (double*) R_alloc(n, sizeof(double));
  for (int i = 0; i < max_iterations; ++i) {
    mult_mv(n, m, x, new_x);
    double norm_new_x = norm2(n, new_x);
    double err = 0.0;
    for (int j = 0; j < n; ++j) {
      double prev = x[j];
      x[j] = new_x[j] / norm_new_x;
      err += fabs(x[j] - prev);
    }
    if (err < tolerance) {
      return true;
    }
  }
  return false;
}

double raleigh_quotient(int n, const double *m, const double *x) {
  // We might need to provide this as workspace generally
  double *mx = (double*)R_alloc(n, sizeof(double));
  mult_mv(n, m, x, mx);
  return dot(n, mx, x) / dot(n, x, x);
}

double eigen_power_iteration(int n, const double *m, int max_iterations,
                             double tolerance, double *x) {
  power_iteration(n, m, max_iterations, tolerance, x);
  return raleigh_quotient(n, m, x);
}
