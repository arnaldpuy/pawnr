context("pawn matrices")

N <- 100; Nc <- 50; n <- 10; k <- 3

test_that("Output is a matrix", {
  A <- pawn_matrix(N, Nc, n, k)
  expect_is(A, "matrix")
})

test_that("Sample size", {
  A <- pawn_matrix(N, Nc, n, k)
  expect_equal(nrow(A), N + Nc * n * k)
})
