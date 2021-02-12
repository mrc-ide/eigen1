#include <stdbool.h>
bool power_iteration(int n, const double *m, int max_iterations,
                     double tolerance, double *x);
double raleigh_quotient(int n, const double *m, const double *x);
double eigen_power_iteration(int n, const double *m, int max_iterations,
                             double tolerance, double *x);
