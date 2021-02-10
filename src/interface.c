#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>

#include "eigen1.h"

int scalar_integer(SEXP x, const char * name) {
  if (length(x) != 1) {
    Rf_error("Expected a length 1 vector for %s", name);
  }
  int ret = 0;
  if (TYPEOF(x) == INTSXP) {
    ret = INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP) {
    ret = REAL(x)[0];
  } else {
    Rf_error("Expected an integer for %s", name);
  }
  return ret;
}

double scalar_double(SEXP x, const char * name) {
  if (length(x) != 1) {
    Rf_error("Expected a length 1 vector for %s", name);
  }
  double ret = 0.0;
  if (TYPEOF(x) == INTSXP) {
    ret = INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP) {
    ret = REAL(x)[0];
  } else {
    Rf_error("Expected an number for %s", name);
  }
  return ret;
}

SEXP r_power_iteration(SEXP r_m, SEXP r_max_iterations, SEXP r_error,
                       SEXP r_initial) {
  const int n = nrows(r_m);
  const int max_iterations = scalar_integer(r_max_iterations, "max_iterations");
  const double error = scalar_double(r_error, "error");
  SEXP ret = PROTECT(allocVector(REALSXP, n));
  double *cret = REAL(ret);
  if (r_initial == R_NilValue) {
    GetRNGstate();
    for (int i = 0; i < n; ++i) {
      cret[i] = unif_rand();
    }
    PutRNGstate();
  } else {
    memcpy(cret, REAL(r_initial), n * sizeof(double));
  }

  power_iteration(n, REAL(r_m), max_iterations, error, REAL(ret));
  UNPROTECT(1);
  return ret;
}

SEXP r_raleigh_quotient(SEXP r_m, SEXP r_v) {
  const int n = nrows(r_m);
  return ScalarReal(raleigh_quotient(n, REAL(r_m), REAL(r_v)));
}

SEXP r_eigen_power_iteration(SEXP r_m, SEXP r_max_iterations, SEXP r_error,
                             SEXP r_initial) {
  const int n = nrows(r_m);
  const int max_iterations = scalar_integer(r_max_iterations, "max_iterations");
  const double error = scalar_double(r_error, "error");

  double *x = (double*)R_alloc(n, sizeof(double));
  if (r_initial == R_NilValue) {
    GetRNGstate();
    for (int i = 0; i < n; ++i) {
      x[i] = unif_rand();
    }
    PutRNGstate();
  } else {
    memcpy(x, REAL(r_initial), n * sizeof(double));
  }

  const double *m = REAL(r_m);
  return ScalarReal(eigen_power_iteration(n, m, max_iterations, error, x));
}

static const R_CallMethodDef call_methods[] = {
  {"Cpower_iteration",       (DL_FUNC) &r_power_iteration,       4},
  {"Craleigh_quotient",      (DL_FUNC) &r_raleigh_quotient,      2},
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
