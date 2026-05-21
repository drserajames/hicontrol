test_that("log_num converts standard titres to log2 scale", {
  expect_equal(log_num("10"),   0)
  expect_equal(log_num("20"),   1)
  expect_equal(log_num("40"),   2)
  expect_equal(log_num("80"),   3)
  expect_equal(log_num("160"),  4)
  expect_equal(log_num("320"),  5)
})

test_that("log_num handles left-censored values at half the detection limit", {
  # "<10" treated as 5 (half of 10), so log2(5/10) = log2(0.5) = -1
  expect_equal(suppressWarnings(log_num("<10")), -1)
  # "<20" treated as 10, so log2(10/10) = 0
  expect_equal(suppressWarnings(log_num("<20")),  0)
  # "<40" treated as 20, so log2(20/10) = 1
  expect_equal(suppressWarnings(log_num("<40")),  1)
})

test_that("log_num handles mixed censored and non-censored vectors", {
  result <- suppressWarnings(log_num(c("40", "80", "<10", "160")))
  expect_equal(result, c(2, 3, -1, 4))
})

test_that("log_num returns a numeric vector of the same length as input", {
  result <- suppressWarnings(log_num(c("40", "<10", "160")))
  expect_type(result, "double")
  expect_length(result, 3)
})

test_that("log_num handles all-censored input", {
  result <- suppressWarnings(log_num(c("<10", "<10", "<20")))
  expect_equal(result, c(-1, -1, 0))
})

test_that("log_num handles a single non-censored value", {
  expect_equal(log_num("40"), 2)
})
