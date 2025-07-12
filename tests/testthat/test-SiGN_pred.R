test_that("SiGN_pred returns correct values for standard input", {
  # Equal reinforcement and delay reduction → 0.5
  expect_equal(SiGN_pred(r1 = 1, r2 = 1, dr1 = 0.5, dr2 = 0.5), 0.5)

  # Different reinforcement and delay reduction → 0.8
  expect_equal(SiGN_pred(r1 = 2, r2 = 1, dr1 = 0.4, dr2 = 0.2), 0.8)

  # Lower DR for alt1 → closer to 0
  expect_equal(SiGN_pred(r1 = 1, r2 = 1, dr1 = 0.2, dr2 = 0.8), 0.2)
})

test_that("SiGN_pred handles edge cases with DR sign reversal", {
  # dr1 > 0, dr2 < 0 → force prediction to 1
  expect_equal(SiGN_pred(r1 = 1, r2 = 1, dr1 = 0.5, dr2 = -0.1), 1)

  # dr1 < 0, dr2 > 0 → force prediction to 0
  expect_equal(SiGN_pred(r1 = 1, r2 = 1, dr1 = -0.1, dr2 = 0.5), 0)
})

test_that("SiGN_pred is vectorised", {
  r1 <- c(1, 1)
  r2 <- c(1, 1)
  dr1 <- c(0.5, -0.1)
  dr2 <- c(0.5, 0.5)

  result <- SiGN_pred(r1, r2, dr1, dr2)
  expect_equal(result, c(0.5, 0))
})

test_that("SiGN_pred handles zero delay reductions", {
  # If both DRs are 0, result is NaN due to 0/0
  result <- suppressWarnings(SiGN_pred(1, 1, 0, 0))
  expect_true(is.nan(result))
})
