#include "util.h"

#include <R.h>
#define USE_FC_LEN_T
#include <Rconfig.h>
#include <R_ext/BLAS.h>
#ifndef FCONE
#define FCONE
#endif

void mult_mv(int n, const double *A, const double *v, double *out) {
  const char *trans = "N";
  int one = 1;
  double alpha = 1.0;
  double beta = 0.0;
  F77_CALL(dgemm)(trans, trans, &n, &one, &n, &alpha,
                  A, &n, v, &n, &beta, out, &n FCONE FCONE);
}

double norm2(int n, const double *x) {
  int one = 1;
  return F77_CALL(dnrm2)(&n, x, &one);
}

double dot(int n, const double *a, const double *b) {
  int one = 1;
  return F77_CALL(ddot)(&n, a, &one, b, &one);
}
