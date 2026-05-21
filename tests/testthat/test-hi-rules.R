# ---------------------------------------------------------------------------
# hi_rules — output structure
# ---------------------------------------------------------------------------

test_that("hi_rules returns a list of two elements", {
  dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
  result <- hi_rules(dat, centre = 4, threshold = 1)
  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("hi_rules[[1]] is a list of exactly 8 rule results", {
  dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
  result <- hi_rules(dat, centre = 4, threshold = 1)
  expect_type(result[[1]], "list")
  expect_length(result[[1]], 8)
})

test_that("hi_rules[[2]] is a numeric vector of length 8", {
  dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
  result <- hi_rules(dat, centre = 4, threshold = 1)
  expect_type(result[[2]], "double")
  expect_length(result[[2]], 8)
})

test_that("hi_rules false-positive rates are all in [0, 1]", {
  dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
  result <- hi_rules(dat, centre = 4, threshold = 1)
  expect_true(all(result[[2]] >= 0))
  expect_true(all(result[[2]] <= 1))
})


# ---------------------------------------------------------------------------
# hi_rules — individual rule detection
# ---------------------------------------------------------------------------

test_that("hi_rules rule 1: flags a point >= 3 SD from centre", {
  # dat[4] = 10 is 10 SD above centre = 0
  dat <- c(0, 0, 0, 10, 0, 0, 0, 0, 0, 0)
  result <- hi_rules(dat, centre = 0, threshold = 1)
  expect_true(4 %in% result[[1]][[1]])
})

test_that("hi_rules rule 1: rate is 1/n for a single 3-SD violation", {
  dat <- c(0, 0, 0, 10, 0, 0, 0, 0, 0, 0)
  result <- hi_rules(dat, centre = 0, threshold = 1)
  expect_equal(result[[2]][1], 1 / length(dat))
})

test_that("hi_rules rule 5: flags 25 consecutive identical values", {
  dat <- rep(0, 26)
  result <- hi_rules(dat, centre = 0, threshold = 1)
  expect_true(length(result[[1]][[5]]) > 0)
})

test_that("hi_rules rule 7: flags a monotone trend of 4+ points", {
  # Increasing trend from index 3: 1, 2, 3, 4, 5
  dat <- c(0, 0, 1, 2, 3, 4, 5, 0, 0, 0)
  result <- hi_rules(dat, centre = 0, threshold = 1)
  expect_true(length(result[[1]][[7]]) > 0)
})

test_that("hi_rules rule 8: flags a single-step difference >= 3 units", {
  # |dat[3] - dat[4]| = 5 >= 3
  dat <- c(0, 0, 0, 5, 0, 0, 0, 0, 0, 0)
  result <- hi_rules(dat, centre = 0, threshold = 1)
  expect_true(3 %in% result[[1]][[8]])
})

test_that("hi_rules returns zero rate for rules that do not fire", {
  # Short, tightly clustered data near centre — no rules should fire
  dat <- c(0, 0.1, -0.1, 0, 0.1, -0.1, 0, 0.1, -0.1, 0)
  result <- hi_rules(dat, centre = 0, threshold = 5)
  expect_equal(result[[2]][1], 0)  # rule 1: no points >= 3 SD
  expect_equal(result[[2]][8], 0)  # rule 8: no large steps
})


# ---------------------------------------------------------------------------
# hi_rules2 — output structure
# ---------------------------------------------------------------------------

test_that("hi_rules2 returns a list of two elements", {
  dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
  result <- hi_rules2(dat, centre = 4, threshold = 1)
  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("hi_rules2[[2]] contains non-negative integer-valued counts", {
  dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
  result <- hi_rules2(dat, centre = 4, threshold = 1)
  expect_true(all(result[[2]] >= 0))
  expect_true(all(result[[2]] == floor(result[[2]])))
})


# ---------------------------------------------------------------------------
# hi_rules vs hi_rules2 consistency
# ---------------------------------------------------------------------------

test_that("hi_rules and hi_rules2 produce identical rule result lists", {
  dat <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
  r1 <- hi_rules(dat, centre = 4, threshold = 1)
  r2 <- hi_rules2(dat, centre = 4, threshold = 1)
  expect_equal(r1[[1]], r2[[1]])
})

test_that("hi_rules proportion equals hi_rules2 count divided by n (rule 1)", {
  # One 3-SD violation in a 10-point series
  dat <- c(0, 0, 0, 10, 0, 0, 0, 0, 0, 0)
  r1 <- hi_rules(dat, centre = 0, threshold = 1)
  r2 <- hi_rules2(dat, centre = 0, threshold = 1)
  expect_equal(r1[[2]][1], r2[[2]][1] / length(dat))
})

test_that("hi_rules proportion equals hi_rules2 count divided by n (rule 8)", {
  # One large step in a 10-point series
  dat <- c(0, 0, 0, 5, 0, 0, 0, 0, 0, 0)
  r1 <- hi_rules(dat, centre = 0, threshold = 1)
  r2 <- hi_rules2(dat, centre = 0, threshold = 1)
  expect_equal(r1[[2]][8], r2[[2]][8] / length(dat))
})
