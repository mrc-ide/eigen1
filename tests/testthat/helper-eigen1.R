r_power_iteration <- function(m, n = 100, error = 1e-4, initial = NULL) {
  x <- initial %||% runif(nrow(m))
  for (i in seq_len(n)) {
    new_x <- m %*% x
    new_x <- new_x / norm(new_x, "2")
    if (sum(abs(x - new_x)) < error) {
      return(new_x)
    }
    x <- new_x
  }
  stop("Did not converge")
}


raleigh_quotient <- function(m, x) {
  drop((drop(m %*% x) %*% x) / drop(x %*% x))
}


leading_eigenvalue <- function(m) {
  raleigh_quotient(m, drop(r_power_iteration(m)))
}
