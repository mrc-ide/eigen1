#define R_NO_REMAP
#define STRICT_R_HEADERS

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>

#include "eigen1.h"

int scalar_integer(SEXP x, const char * name) {
  if (Rf_length(x) != 1) {
    Rf_error("Expected a length 1 vector for '%s'", name);
  }
  int ret = 0;
  if (TYPEOF(x) == INTSXP) {
    ret = INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP) {
    ret = REAL(x)[0];
  } else {
    Rf_error("Expected an integer for '%s'", name);
  }
  return ret;
}

double scalar_double(SEXP x, const char * name) {
  if (Rf_length(x) != 1) {
    Rf_error("Expected a length 1 vector for '%s'", name);
  }
  double ret = 0.0;
  if (TYPEOF(x) == INTSXP) {
    ret = INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP) {
    ret = REAL(x)[0];
  } else {
    Rf_error("Expected an number for '%s'", name);
  }
  return ret;
}

SEXP r_eigen_power_iteration(SEXP r_m, SEXP r_max_iterations, SEXP r_tolerance,
                             SEXP r_initial) {
  const int n = Rf_nrows(r_m);
  if (Rf_ncols(r_m) != n) {
    Rf_error("Expected 'm' to be square");
  }
  const double *m = REAL(r_m);
  const int max_iterations = scalar_integer(r_max_iterations, "max_iterations");
  const double tolerance = scalar_double(r_tolerance, "tolerance");

  double *x = (double*)R_alloc(n, sizeof(double));
  if (r_initial == R_NilValue) {
    GetRNGstate();
    for (int i = 0; i < n; ++i) {
      x[i] = unif_rand();
    }
    PutRNGstate();
  } else {
    if (Rf_length(r_initial) != n) {
      Rf_error("Expected a vector of length %d for 'initial'", n);
    }
    memcpy(x, REAL(r_initial), n * sizeof(double));
  }

  SEXP dim = Rf_getAttrib(r_m, R_DimSymbol);
  const int nd = Rf_length(dim);
  int n_matrices = 1;
  if (nd == 3) {
    n_matrices = INTEGER(dim)[2];
  } else if (nd != 2) {
    Rf_error("Expected 'm' to be a matrix or 3d array");
  }

  SEXP ret = PROTECT(Rf_allocVector(REALSXP, n_matrices));
  const int len = n * n;
  for (int i = 0; i < n_matrices; ++i) {
    REAL(ret)[i] = eigen_power_iteration(n, m + i * len,
                                         max_iterations, tolerance, x);
  }

  UNPROTECT(1);
  return ret;
}

static const R_CallMethodDef call_methods[] = {
  {"Ceigen_power_iteration", (DL_FUNC) &r_eigen_power_iteration, 4},
  {NULL,                     NULL,                               0}
};

void R_init_eigen1(DllInfo *info) {
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
#endif
}
