# Plot functions are tested for correct return values and absence of errors.
# A null PDF device is opened for each test to avoid interactive graphics windows.

dat_base <- c(2, 4, 6, 8, 6, 4, 2, 4, 6, 8)
n_length  <- c(1, 3, 8, 9, 25, 10, 4, 2)


# ---------------------------------------------------------------------------
# plot_nelson
# ---------------------------------------------------------------------------

test_that("plot_nelson returns NULL invisibly and does not error", {
  res <- hi_rules(dat_base, centre = 4, threshold = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  result <- plot_nelson(dat_base, res[[1]], centre = 4, threshold = 1,
                        n_length = n_length)
  expect_null(result)
})

test_that("plot_nelson handles data with no rule violations", {
  # Very large threshold ensures no rules fire
  dat <- c(0, 0.1, -0.1, 0, 0.1, -0.1, 0, 0.1, -0.1, 0)
  res <- hi_rules(dat, centre = 0, threshold = 100)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(
    plot_nelson(dat, res[[1]], centre = 0, threshold = 100,
                n_length = n_length)
  )
})

test_that("plot_nelson handles a rule-1 violation (3-SD point)", {
  # Gradual approach to the peak keeps all steps < 3, so rule 1 fires but not 8
  dat <- c(4, 4, 4, 5, 7.5, 5, 4, 4, 4, 4)
  res <- hi_rules(dat, centre = 4, threshold = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(
    plot_nelson(dat, res[[1]], centre = 4, threshold = 1,
                n_length = n_length)
  )
})

test_that("plot_nelson handles a rule-8 violation (large step)", {
  # |dat[3] - dat[4]| = 5 >= 3, so rule 8 fires (vector result, not matrix)
  dat <- c(0, 0, 0, 5, 0, 0, 0, 0, 0, 0)
  res <- hi_rules(dat, centre = 0, threshold = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(
    plot_nelson(dat, res[[1]], centre = 0, threshold = 1,
                n_length = n_length)
  )
})


# ---------------------------------------------------------------------------
# plot_hi
# ---------------------------------------------------------------------------

test_that("plot_hi returns NULL invisibly and does not error", {
  res <- hi_rules(dat_base, centre = 4, threshold = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  result <- plot_hi(dat_base, res[[1]], centre = 4, threshold = 1,
                    n_length = n_length)
  expect_null(result)
})

test_that("plot_hi handles data with no rule violations", {
  dat <- c(0, 0.1, -0.1, 0, 0.1, -0.1, 0, 0.1, -0.1, 0)
  res <- hi_rules(dat, centre = 0, threshold = 100)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(
    plot_hi(dat, res[[1]], centre = 0, threshold = 100,
            n_length = n_length)
  )
})

test_that("plot_hi handles a rule-8 violation (large step)", {
  dat <- c(0, 0, 0, 5, 0, 0, 0, 0, 0, 0)
  res <- hi_rules(dat, centre = 0, threshold = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(
    plot_hi(dat, res[[1]], centre = 0, threshold = 1,
            n_length = n_length)
  )
})


# ---------------------------------------------------------------------------
# plot_hi2
# ---------------------------------------------------------------------------

test_that("plot_hi2 returns NULL invisibly and does not error", {
  res <- hi_rules2(dat_base, centre = 4, threshold = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  result <- plot_hi2(dat_base, res[[1]], centre = 4, threshold = 1,
                     n_length = n_length, ttl_len = length(dat_base))
  expect_null(result)
})

test_that("plot_hi2 handles data with no rule violations", {
  dat <- c(0, 0.1, -0.1, 0, 0.1, -0.1, 0, 0.1, -0.1, 0)
  res <- hi_rules2(dat, centre = 0, threshold = 100)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(
    plot_hi2(dat, res[[1]], centre = 0, threshold = 100,
             n_length = n_length, ttl_len = length(dat))
  )
})

test_that("plot_hi2 handles ttl_len larger than the data length", {
  res <- hi_rules2(dat_base, centre = 4, threshold = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(
    plot_hi2(dat_base, res[[1]], centre = 4, threshold = 1,
             n_length = n_length, ttl_len = 50)
  )
})
