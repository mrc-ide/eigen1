#include "eigen1.h"
#include "util.h"
#include <R.h>

bool power_iteration(int n, const double *m, int niter, double error,
                     double *x) {
  double *new_x = (double*) R_alloc(n, sizeof(double));
  for (int i = 0; i < niter; ++i) {
    mult_mv(n, m, x, new_x);
    double norm_new_x = norm2(n, new_x);
    double err = 0;
    for (int j = 0; j < n; ++j) {
      double prev = x[j];
      x[j] = new_x[j] / norm_new_x;
      err += fabs(x[j] - prev);
    }
    if (err < error) {
      return true;
    }
  }
  return false;
}
