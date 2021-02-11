##' Compute the leading eigenvalue for a square matrix
##'
##' This function exposes two different methods for computing the
##' leading eigenvalue of a matrix. The "base" method simply uses
##' [eigen] but allows 3d arrays (returning a vector of leading
##' eigenvalues). The "power_iteration" method uses the power
##' iteration method which will work well if there is a significant
##' difference between the first and second eigenvalues.
##'
##' @title Compute leading eigenvalue
##'
##' @param m A matrix or 3d array
##'
##' @param ... Additional arguments passed to the underlying method
##'   (unused arguments are silently ignored)
##'
##' @param method Select the method to use. Currently only
##'   "power_iteration" and "base" are supported
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
         base = eigen1_base(m, ...),
         stop(sprintf("Unknown method '%s'", method)))
}


##' @rdname eigen1
##' @export
##' @param max_iterations Integer, giving the number of iterations before
##'   giving up
##'
##' @param tolerance Number, giving the required tolerance
##'
##' @param initial An optional initial guess at the eigenvector. If
##'   omitted we use a random vector
##'
##' @param ... Ignored arguments
eigen1_power_iteration <- function(m, max_iterations = 100,
                                   tolerance = sqrt(.Machine$double.eps),
                                   initial = NULL, ...) {
  .Call(Ceigen_power_iteration, m, max_iterations, tolerance, initial)
}


##' @rdname eigen1
##' @export
eigen1_base <- function(m, ...) {
  f <- function(m) {
    ev <- eigen(m, symmetric = FALSE, only.values = TRUE)$values
    if (is.complex(ev)) {
      ev[Im(ev) != 0] <- -Inf
      ev <- as.numeric(ev)
    }
    max(ev)
  }
  nd <- length(dim(m))
  if (nd == 2L) {
    ret <- f(m)
  } else if (nd == 3L) {
    ret <- apply(m, 3, f)
  } else {
    stop("Expected 'm' to be a matrix or 3d array")
  }
  ret
}


`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}
