# The point of these tests is that the hand-written statistics in R/stats_tests.R
# agree with independent implementations. The reports show the equations; the
# tests prove the code applies them.

test_that("skewness and kurtosis match the moments package", {
  skip_if_not_installed("moments")
  set.seed(42)
  x <- rgamma(2000, shape = 2, rate = 1)

  expect_equal(skewness(x), moments::skewness(x), tolerance = 1e-8)
  expect_equal(kurtosis(x), moments::kurtosis(x), tolerance = 1e-8)
  expect_equal(kurtosis(x, excess = TRUE), moments::kurtosis(x) - 3, tolerance = 1e-8)
})

test_that("a normal sample has skewness near zero and kurtosis near three", {
  set.seed(1)
  x <- rnorm(200000)

  expect_lt(abs(skewness(x)), 0.02)
  expect_lt(abs(kurtosis(x) - 3), 0.05)
})

test_that("jarque_bera matches tseries on the same sample", {
  skip_if_not_installed("tseries")
  set.seed(7)
  x <- c(rnorm(1500), rt(500, df = 3))

  ours <- jarque_bera(x)
  theirs <- tseries::jarque.bera.test(x)

  expect_equal(ours$statistic, unname(theirs$statistic), tolerance = 1e-6)
  expect_equal(ours$p_value, unname(theirs$p.value), tolerance = 1e-6)
})

test_that("jarque_bera rejects a fat-tailed sample and spares a normal one", {
  set.seed(11)

  fat_tailed <- jarque_bera(rt(5000, df = 3))
  expect_true(fat_tailed$reject_normality)
  expect_gt(fat_tailed$excess_kurtosis, 1)

  normal <- jarque_bera(rnorm(5000))
  expect_false(normal$reject_normality)
})

test_that("mean_zero_test matches stats::t.test", {
  set.seed(3)
  x <- rnorm(1000, mean = 0.001, sd = 0.02)

  ours <- mean_zero_test(x)
  theirs <- stats::t.test(x, mu = 0)

  expect_equal(ours$statistic, unname(theirs$statistic), tolerance = 1e-10)
  expect_equal(ours$p_value, theirs$p.value, tolerance = 1e-10)
  expect_equal(c(ours$conf_low, ours$conf_high), as.numeric(theirs$conf.int), tolerance = 1e-10)
})

test_that("mean_zero_test detects a real drift and ignores a spurious one", {
  set.seed(5)

  # Large drift relative to noise: should reject.
  expect_true(mean_zero_test(rnorm(5000, mean = 0.01, sd = 0.02))$reject_zero_mean)

  # No drift: should not reject.
  expect_false(mean_zero_test(rnorm(5000, mean = 0, sd = 0.02))$reject_zero_mean)
})

test_that("jarque_bera_table returns one row per series, in order", {
  set.seed(13)
  table <- jarque_bera_table(list(
    normal = rnorm(500),
    fat = rt(500, df = 3),
    skewed = rgamma(500, shape = 1)
  ))

  expect_equal(nrow(table), 3)
  expect_equal(table$series, c("normal", "fat", "skewed"))
})
