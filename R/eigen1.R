##' Compute the leading eigenvalue for a square matrix
##'
##' @title Compute leading eigenvalue
##'
##' @param m A matrix or 3d array
##'
##' @param ... Additional arguments passed to the underlying method
##'
##' @param method Select the method to use. Currently only
##'   "power_iteration" is supported.
##'
##' @return A scalar real (if `m` is a matrix) or a vector with length
##'   `dim(m)[3]` if `m` was a 3d array
##'
##' @export
##' @useDynLib eigen1, .registration = TRUE
##' @examples
##' m <- diag(runif(10))
##' eigen1::eigen1(m)
##' max(eigen(m)$values)
eigen1 <- function(m, ..., method = "power_iteration") {
  switch(method,
         power_iteration = eigen1_power_iteration(m, ...),
        stop(sprintf("Unknown method '%s'", method)))
}


##' @rdname eigen1
##' @param max_iterations Integer, giving the number of iterations before
##'   giving up
##'
##' @param tolerance Number, giving the required tolerance
##'
##' @param initial An optional initial guess at the eigenvector. If
##'   omitted we use a random vector
eigen1_power_iteration <- function(m, max_iterations = 100,
                                   tolerance = sqrt(.Machine$double.eps),
                                   initial = NULL) {
  .Call(Ceigen_power_iteration, m, max_iterations, tolerance, initial)
}


`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
