r_power_iteration <- function(m, n = 100, error = sqrt(.Machine$double.eps),
                              initial = NULL) {
  x <- initial %||% runif(nrow(m))
  for (i in seq_len(n)) {
    new_x <- m %*% x
    new_x <- new_x / norm(new_x, "2")
    err <- sum(abs(x - new_x))
    message(err)
    if (err < error) {
      attr(new_x, "iterations") <- i
      return(new_x)
    }
    x <- new_x
  }
  stop("Did not converge")
}


r_raleigh_quotient <- function(m, x) {
  drop((drop(m %*% x) %*% x) / drop(x %*% x))
}


r_leading_eigenvalue <- function(m) {
  r_raleigh_quotient(m, drop(r_power_iteration(m)))
}
