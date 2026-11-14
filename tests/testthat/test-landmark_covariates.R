test_that("landmark_covariates() summarises each derived column", {
  out <- landmark_covariates(lm_cov(lookback = 30))

  expect_s3_class(out, "tbl_df", exact = FALSE)
  expect_named(
    out,
    c("column", "source", "method", "include_at_landmark", "lookback",
      "random_slope", "adjust", "n_rows", "n_missing", "median_lag", "max_lag")
  )
  expect_equal(out$column, c("treatment", "biomarker"))
  expect_equal(out$method, c("locf", "locf"))
  expect_equal(out$lookback, c(30, 30))
  expect_equal(out$n_rows, c(20L, 20L))
  expect_equal(out$n_missing, c(sum(is.na(lm_cov(lookback = 30)$treatment)),
                                sum(is.na(lm_cov(lookback = 30)$biomarker))))
  expect_true(all(out$max_lag <= 30))
})

test_that("landmark_covariates() follows filter() and select()", {
  lm <- lm_cov()

  only_60 <- landmark_covariates(dplyr::filter(lm, .landmark == 60))
  expect_equal(only_60$n_rows, c(7L, 7L))
  expect_equal(only_60$n_missing, c(4L, 4L))
  # biomarker lags at s = 60: id 6 (15), id 7 (60), id 10 (0)
  expect_equal(only_60$max_lag[2], 60)

  dropped <- landmark_covariates(dplyr::select(lm, -treatment))
  expect_equal(dropped$column, "biomarker")
})

test_that("landmark_covariates() reports LME options", {
  sim <- sim_lme()
  lm <- add_landmark_covariates(
    sim$lm, sim$visits, pid, day,
    lme = marker, lme_args = list(random_slope = TRUE, adjust = ~ age),
    progress = FALSE
  )
  out <- landmark_covariates(lm)
  expect_equal(out$method, "lme")
  expect_true(out$random_slope)
  expect_equal(out$adjust, "~age")
  expect_true(is.na(out$median_lag))
})

test_that("landmark_lme_fits() returns one row per variable and landmark", {
  sim <- sim_lme()
  sim$visits$marker2 <- sim$visits$marker * 2
  lm <- add_landmark_covariates(
    sim$lm, sim$visits, pid, day,
    locf = marker, lme = c(m_lme = marker, marker2), progress = FALSE
  )
  fits <- landmark_lme_fits(lm)

  expect_equal(fits$column, rep(c("m_lme", "marker2"), each = 2))
  expect_equal(fits$source, rep(c("marker", "marker2"), each = 2))
  expect_equal(fits$.landmark, c(100, 200, 100, 200))
  expect_equal(fits$status, rep("ok", 4))
  expect_true(all(purrr::map_lgl(fits$model, \(m) inherits(m, "merMod"))))
  expect_named(fits, names(lm_empty_lme_fits()))

  # Same as the stored diagnostics
  expect_equal(
    fits[fits$column == "m_lme", -(1:2)],
    lm_covariate_info(lm)$details[[2]]
  )
})

test_that("landmark_lme_fits() keeps failed fits with a NULL model", {
  expect_warning(
    lm <- lm_cov(locf = treatment, lme = biomarker, lme_args = list(min_subjects = 3))
  )
  fits <- landmark_lme_fits(lm)
  expect_equal(fits$status[fits$.landmark == 30], "failed")
  expect_null(fits$model[[which(fits$.landmark == 30)]])
})

test_that("both accessors return empty tibbles when there is nothing to report", {
  base <- lm_spec()
  expect_equal(nrow(landmark_covariates(base)), 0L)
  expect_named(landmark_covariates(base), names(landmark_covariates(lm_cov())))
  expect_equal(landmark_lme_fits(base), lm_empty_lme_fits())
  expect_equal(landmark_lme_fits(lm_cov()), lm_empty_lme_fits())
})

test_that("both accessors require an intact landmark_tbl", {
  expect_error(landmark_covariates(subjects), "landmark_tbl")
  expect_error(landmark_lme_fits(dplyr::select(lm_cov(), -.status)), "\\.status")
})
