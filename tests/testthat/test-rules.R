# ---------------------------------------------------------------------------
# rule_xyz
# ---------------------------------------------------------------------------

test_that("rule_xyz detects a single point above the upper boundary", {
  # centre=5, threshold=1, z=3 → upper=8, lower=2
  dat <- c(5, 5, 10, 5, 5)
  result <- rule_xyz(dat, centre = 5, threshold = 1, x = 1, y = 1, z = 3)
  expect_true(3 %in% result)
})

test_that("rule_xyz detects a single point below the lower boundary", {
  dat <- c(5, 5, 0, 5, 5)
  result <- rule_xyz(dat, centre = 5, threshold = 1, x = 1, y = 1, z = 3)
  expect_true(3 %in% result)
})

test_that("rule_xyz returns a zero-length vector when no violations", {
  # 7 is exactly 2 SD above centre=5 — below z=3 boundary
  dat <- c(5, 5, 7, 5, 5)
  result <- rule_xyz(dat, centre = 5, threshold = 1, x = 1, y = 1, z = 3)
  expect_length(result, 0)
})

test_that("rule_xyz returns NULL when data shorter than window y", {
  dat <- c(5, 5)
  result <- rule_xyz(dat, centre = 5, threshold = 1, x = 1, y = 3, z = 2)
  expect_null(result)
})

test_that("rule_xyz detects 2-of-3 windows above the boundary", {
  # centre=5, threshold=1, z=2 → upper=7; dat[1]=8 and dat[2]=8 both > 7
  dat <- c(8, 8, 5, 5, 5)
  result <- rule_xyz(dat, centre = 5, threshold = 1, x = 2, y = 3, z = 2)
  expect_true(1 %in% result)
})

test_that("rule_xyz does not fire when only 1 of 3 points exceeds boundary", {
  dat <- c(8, 5, 5, 5, 5)
  result <- rule_xyz(dat, centre = 5, threshold = 1, x = 2, y = 3, z = 2)
  expect_length(result, 0)
})

test_that("rule_xyz detects violations on both sides of centre", {
  # centre=5, threshold=1, z=3: upper=8, lower=2
  # dat[2]=10 (above), dat[4]=0 (below)
  dat <- c(5, 10, 5, 0, 5)
  result <- rule_xyz(dat, centre = 5, threshold = 1, x = 1, y = 1, z = 3)
  expect_true(2 %in% result)
  expect_true(4 %in% result)
})


# ---------------------------------------------------------------------------
# rule_trend
# ---------------------------------------------------------------------------

test_that("rule_trend detects a monotone increasing sequence", {
  dat <- c(1, 2, 3, 4, 5)
  result <- rule_trend(dat, n = 4)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 3)
  expect_equal(unname(result[1, 1]), 1)   # starts at index 1
  expect_equal(unname(result[2, 1]), 5)   # covers all 5 points
  expect_equal(unname(result[3, 1]), 1)   # direction: +1 = increasing
})

test_that("rule_trend detects a monotone decreasing sequence", {
  dat <- c(5, 4, 3, 2, 1)
  result <- rule_trend(dat, n = 4)
  expect_true(is.matrix(result))
  expect_equal(unname(result[1, 1]), 1)
  expect_equal(unname(result[2, 1]), 5)
  expect_equal(unname(result[3, 1]), -1)  # direction: -1 = decreasing
})

test_that("rule_trend returns NULL for non-trending data", {
  dat <- c(1, 3, 2, 4, 3, 5)
  result <- rule_trend(dat, n = 4)
  expect_null(result)
})

test_that("rule_trend returns NULL when data is shorter than n", {
  dat <- c(1, 2, 3)
  result <- rule_trend(dat, n = 4)
  expect_null(result)
})

test_that("rule_trend does not flag a trend shorter than n", {
  # Increasing for only 3 points, need n=4
  dat <- c(1, 2, 3, 2, 1)
  result <- rule_trend(dat, n = 4)
  expect_null(result)
})

test_that("rule_trend detects a trend embedded in otherwise flat data", {
  # Increasing from index 3: 1, 2, 3, 4, 5
  dat <- c(5, 5, 1, 2, 3, 4, 5, 5)
  result <- rule_trend(dat, n = 4)
  expect_true(!is.null(result))
  expect_true(3 %in% result[1, ])
})

test_that("rule_trend returns a 3-row matrix (start, length, direction)", {
  dat <- c(1, 2, 3, 4, 5)
  result <- rule_trend(dat, n = 4)
  expect_equal(nrow(result), 3)
})


# ---------------------------------------------------------------------------
# rule_noC
# ---------------------------------------------------------------------------

test_that("rule_noC detects a run of points outside zone C", {
  # |dat - 0| > 1 for first 6 points
  dat <- c(3, 4, 3, 4, 3, 4, 0)
  result <- rule_noC(dat, centre = 0, threshold = 1, n = 6)
  expect_true(is.matrix(result))
  expect_equal(unname(result[1, 1]), 1)
  expect_equal(unname(result[2, 1]), 6)
})

test_that("rule_noC returns NULL when all points are inside zone C", {
  dat <- c(0, 0.5, -0.5, 0)
  result <- rule_noC(dat, centre = 0, threshold = 1, n = 3)
  expect_null(result)
})

test_that("rule_noC returns NULL when the outside run is shorter than n", {
  # Only 2 consecutive points outside zone C
  dat <- c(3, 4, 0, 0, 0, 0)
  result <- rule_noC(dat, centre = 0, threshold = 1, n = 4)
  expect_null(result)
})

test_that("rule_noC uses strict inequality: points exactly at threshold are not flagged", {
  # |1 - 0| = 1, which is NOT > 1
  dat <- c(1, 1, 1, 1, 1, 1)
  result <- rule_noC(dat, centre = 0, threshold = 1, n = 5)
  expect_null(result)
})

test_that("rule_noC returns NULL when data is shorter than n", {
  dat <- c(3, 4, 3)
  result <- rule_noC(dat, centre = 0, threshold = 1, n = 5)
  expect_null(result)
})

test_that("rule_noC detects multiple separate runs", {
  # Two runs of length 4 outside zone C, separated by a point inside
  dat <- c(3, 4, 3, 4, 0, 3, 4, 3, 4)
  result <- rule_noC(dat, centre = 0, threshold = 1, n = 3)
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 2)
  expect_true(1 %in% result[1, ])
  expect_true(6 %in% result[1, ])
})


# ---------------------------------------------------------------------------
# rule_onlyC
# ---------------------------------------------------------------------------

test_that("rule_onlyC detects a run of points inside zone C", {
  dat <- c(0.5, -0.5, 0.3, -0.3, 0.5, 5)
  result <- rule_onlyC(dat, centre = 0, threshold = 1, n = 5)
  expect_true(is.matrix(result))
  expect_equal(unname(result[1, 1]), 1)
  expect_equal(unname(result[2, 1]), 5)
})

test_that("rule_onlyC returns NULL when no long run inside zone C", {
  dat <- c(0.5, -0.5, 5, 0.5, -0.5)
  result <- rule_onlyC(dat, centre = 0, threshold = 1, n = 3)
  expect_null(result)
})

test_that("rule_onlyC detects a run embedded in the middle of data", {
  dat <- c(5, 0.5, -0.5, 0.3, -0.3, 0.5, 5)
  result <- rule_onlyC(dat, centre = 0, threshold = 1, n = 5)
  expect_true(!is.null(result))
  expect_equal(unname(result[1, 1]), 2)
  expect_equal(unname(result[2, 1]), 5)
})

test_that("rule_onlyC treats points exactly at the threshold as inside zone C", {
  # |1 - 0| = 1, which is NOT > 1, so these points are inside zone C
  dat <- c(1, -1, 1, -1, 1)
  result <- rule_onlyC(dat, centre = 0, threshold = 1, n = 5)
  expect_true(!is.null(result))
})

test_that("rule_onlyC returns NULL when data is shorter than n", {
  dat <- c(0.5, -0.5, 0.3)
  result <- rule_onlyC(dat, centre = 0, threshold = 1, n = 5)
  expect_null(result)
})


# ---------------------------------------------------------------------------
# rule_alt
# ---------------------------------------------------------------------------

test_that("rule_alt detects a perfect alternating sequence", {
  dat <- c(1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1)
  result <- rule_alt(dat, n = 10)
  expect_true(is.matrix(result))
  expect_equal(unname(result[1, 1]), 1)
  expect_equal(unname(result[2, 1]), 11)
})

test_that("rule_alt detects a sequence of exactly n alternating points", {
  dat <- c(1, 5, 1, 5, 1, 5, 1, 5, 1, 5)
  result <- rule_alt(dat, n = 10)
  expect_true(!is.null(result))
  expect_equal(unname(result[1, 1]), 1)
  expect_equal(unname(result[2, 1]), 10)
})

test_that("rule_alt returns NULL for n-1 alternating points", {
  dat <- c(1, 5, 1, 5, 1, 5, 1, 5, 1)  # 9 points — data shorter than n=10
  result <- rule_alt(dat, n = 10)
  expect_null(result)
})

test_that("rule_alt returns NULL for monotone increasing data", {
  dat <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  result <- rule_alt(dat, n = 10)
  expect_null(result)
})

test_that("rule_alt returns NULL for data shorter than n", {
  dat <- c(1, 5, 1, 5, 1)
  result <- rule_alt(dat, n = 10)
  expect_null(result)
})

test_that("rule_alt detects alternating pattern in the middle of flat data", {
  dat <- c(3, 3, 3, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 3, 3)
  result <- rule_alt(dat, n = 10)
  expect_true(!is.null(result))
})

test_that("rule_alt works for irregular alternating magnitudes", {
  # Up-down direction alternates even if magnitudes differ
  dat <- c(1, 4, 2, 5, 1, 6, 2, 4, 1, 5, 2)
  result <- rule_alt(dat, n = 10)
  expect_true(!is.null(result))
  expect_equal(unname(result[1, 1]), 1)
})


# ---------------------------------------------------------------------------
# rule_nodiff
# ---------------------------------------------------------------------------

test_that("rule_nodiff detects a run of identical consecutive values", {
  dat <- c(1, 2, 2, 2, 2, 2, 3)
  result <- rule_nodiff(dat, n = 5)
  expect_true(is.matrix(result))
  expect_equal(unname(result[1, 1]), 2)  # run starts at index 2
  expect_equal(unname(result[2, 1]), 5)  # 5 identical values (2, 2, 2, 2, 2)
})

test_that("rule_nodiff detects exactly n identical values", {
  dat <- c(1, 2, 2, 2, 2, 2)
  result <- rule_nodiff(dat, n = 5)
  expect_true(!is.null(result))
  expect_equal(unname(result[1, 1]), 2)
  expect_equal(unname(result[2, 1]), 5)
})

test_that("rule_nodiff returns NULL when run is one short of n", {
  # 4 identical values, but n = 5
  dat <- c(1, 2, 2, 2, 2, 3)
  result <- rule_nodiff(dat, n = 5)
  expect_null(result)
})

test_that("rule_nodiff returns NULL for strictly different values", {
  dat <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  result <- rule_nodiff(dat, n = 5)
  expect_null(result)
})

test_that("rule_nodiff returns NULL when data is shorter than n", {
  dat <- c(2, 2, 2)
  result <- rule_nodiff(dat, n = 5)
  expect_null(result)
})

test_that("rule_nodiff flags a run at the start of the vector", {
  dat <- c(3, 3, 3, 3, 3, 1)
  result <- rule_nodiff(dat, n = 5)
  expect_true(!is.null(result))
  expect_equal(unname(result[1, 1]), 1)
  expect_equal(unname(result[2, 1]), 5)
})


# ---------------------------------------------------------------------------
# rule_diff
# ---------------------------------------------------------------------------

test_that("rule_diff detects a step >= d", {
  # |dat[2] - dat[3]| = |2 - 5| = 3 >= 3
  dat <- c(1, 2, 5, 6, 7)
  result <- rule_diff(dat, d = 3)
  expect_equal(result, 2)
})

test_that("rule_diff returns zero-length vector when no large differences", {
  dat <- c(1, 2, 3, 4, 5)
  result <- rule_diff(dat, d = 3)
  expect_length(result, 0)
})

test_that("rule_diff detects multiple large differences", {
  dat <- c(1, 6, 1, 6, 1)
  result <- rule_diff(dat, d = 3)
  expect_equal(result, c(1, 2, 3, 4))
})

test_that("rule_diff flags a difference of exactly d (boundary inclusive)", {
  dat <- c(1, 4)
  result <- rule_diff(dat, d = 3)
  expect_equal(result, 1)
})

test_that("rule_diff does not flag a difference just below d", {
  dat <- c(1, 3.9, 1)
  result <- rule_diff(dat, d = 3)
  expect_length(result, 0)
})

test_that("rule_diff detects negative steps of magnitude >= d", {
  dat <- c(5, 1, 5)
  result <- rule_diff(dat, d = 3)
  expect_equal(result, c(1, 2))
})


# ---------------------------------------------------------------------------
# rule_noCrelax
# ---------------------------------------------------------------------------

test_that("rule_noCrelax detects a run at or beyond the zone C boundary", {
  dat <- c(3, 1, 3, 1, 3, 1, 0)
  result <- rule_noCrelax(dat, centre = 0, threshold = 1, n = 6)
  expect_true(is.matrix(result))
  expect_equal(unname(result[1, 1]), 1)
  expect_equal(unname(result[2, 1]), 6)
})

test_that("rule_noCrelax includes points exactly at the threshold", {
  # |1 - 0| = 1 >= 1, so these ARE included (unlike strict rule_noC)
  dat <- c(1, 1, 1, 1, 1, 0)
  result <- rule_noCrelax(dat, centre = 0, threshold = 1, n = 5)
  expect_true(!is.null(result))
})

test_that("rule_noC and rule_noCrelax differ on boundary points", {
  dat <- c(1, 1, 1, 1, 1, 0)
  expect_null(rule_noC(dat, centre = 0, threshold = 1, n = 5))
  expect_true(!is.null(rule_noCrelax(dat, centre = 0, threshold = 1, n = 5)))
})

test_that("rule_noCrelax returns NULL when run is too short", {
  dat <- c(3, 4, 0, 0, 0, 0)
  result <- rule_noCrelax(dat, centre = 0, threshold = 1, n = 5)
  expect_null(result)
})

test_that("rule_noCrelax returns NULL when data is shorter than n", {
  dat <- c(3, 4, 3)
  result <- rule_noCrelax(dat, centre = 0, threshold = 1, n = 5)
  expect_null(result)
})
