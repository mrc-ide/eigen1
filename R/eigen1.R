##' @useDynLib eigen1, .registration = TRUE
eigen1_util_power_iteration <- function(m, max_iterations = 100,
                                   tol = sqrt(.Machine$double.eps),
                                   initial = NULL) {
  .Call(Cpower_iteration, m, max_iterations, tol, initial)
}

eigen1_power_iteration <- function(m, max_iterations = 100,
                                   tol = sqrt(.Machine$double.eps),
                                   initial = NULL) {
  .Call(Ceigen_power_iteration, m, max_iterations, tol, initial)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
