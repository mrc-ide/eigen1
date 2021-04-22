context("eigen1")

test_that("Compute eigenvalue of random matrix", {
  set.seed(2)
  m <- matrix(runif(25), 5, 5)
  cmp <- eigen(m)$values[[1]]
  expect_equal(eigen1(m), cmp)
})


test_that("Graceful handling of max_iterations", {
  m <- diag(10)
  expect_error(eigen1(m, max_iterations = "one"),
               "Expected an integer for 'max_iterations'")
  expect_error(eigen1(m, max_iterations = 1:2),
               "Expected a length 1 vector for 'max_iterations'")
  expect_identical(eigen1(m, max_iterations = 1L), 1.0)
  expect_identical(eigen1(m, max_iterations = 1.0), 1.0)
})


test_that("Graceful handling of tolernace", {
  m <- diag(10)
  expect_error(eigen1(m, tolerance = "very small"),
               "Expected an number for 'tolerance'")
  expect_error(eigen1(m, tolerance = 1:2),
               "Expected a length 1 vector for 'tolerance'")
  expect_identical(eigen1(m, tolerance = 1L), 1.0)
  expect_identical(eigen1(m, tolerance = 1.0), 1.0)
})


test_that("validate that the matrix is an appropriate shape", {
  set.seed(2)
  m <- matrix(runif(25), 5, 5)
  expect_error(eigen1(m, method = "mystery"),
               "Unknown method 'mystery'")
  expect_error(eigen1(m[-3, ]),
               "Expected 'm' to be square")
})


test_that("validate that initial conditions, if given, are correct size", {
  set.seed(2)
  m <- matrix(runif(25), 5, 5)
  v <- runif(5)
  expect_equal(eigen1(m, initial = v), eigen1(m, initial = NULL))
  expect_error(eigen1(m, initial = v[-1]),
               "Expected a vector of length 5 for 'initial'")
})


test_that("can compute vector of eigenvalues from array", {
  m <- replicate(3, random_symmetric(5))
  cmp <- apply(m, 3, function(x) max(eigen(x)$values))
  expect_equal(eigen1(m), cmp)
})


test_that("validate the size of an array", {
  m <- replicate(3, random_symmetric(5))
  expect_error(eigen1(aperm(m)),
               "Expected 'm' to be square")
  expect_error(eigen1(array(m, c(dim(m), 1))),
               "Expected 'm' to be a matrix or 3d array")
})


test_that("base method agrees with R", {
  set.seed(2)
  m <- matrix(runif(25), 5, 5)
  expect_identical(eigen1(m, method = "base"),
                   cmp <- eigen(m, FALSE, TRUE)$values[[1]])
})


test_that("base method agrees on 3d array", {
  m <- replicate(3, random_symmetric(5))
  cmp <- apply(m, 3, function(x) max(eigen(x, FALSE, TRUE)$values))
  expect_identical(eigen1(m, method = "base"), cmp)
})


test_that("base method requires matrix or 3d array", {
  m <- replicate(3, random_symmetric(5))
  expect_error(eigen1(c(m), method = "base"),
               "Expected 'm' to be a matrix or 3d array")
  expect_error(eigen1(array(m, c(dim(m), 1)), method = "base"),
               "Expected 'm' to be a matrix or 3d array")
})


test_that("base method copes with non-real eigenvalues", {
  set.seed(1)
  for (i in 1:10) {
    m <- matrix(runif(100), 10, 10)
    cmp <- eigen(m)$values
    if (is.complex(cmp)) {
      break
    }
  }
  expect_equal(
    eigen1(m, method = "base"),
    max(as.numeric(cmp[Im(cmp) == 0])))
  expect_equal(eigen1(m, method = "power_iteration"),
               eigen1(m, method = "base"))
})


test_that("can cope with zero matrices", {
  m <- array(0, c(76, 76, 459))
  expect_equal(
    eigen1::eigen1(m, max_iterations = 1e3, tolerance = 1e-6,
                   method = "power_iteration"),
    rep(0, 459))
})
