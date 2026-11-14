# Expected values use landmarks 30/60/90, horizon 180 and the `visits`
# fixture (spec section 4.2). Risk sets: s = 30 -> ids 3-10, s = 60 -> ids
# 4-10, s = 90 -> ids 6-10. Only ids 6, 7 and 10 have visits.
#
#   id  day  treatment  biomarker
#    6    0          0         25
#    6   45          1         20
#    6   90          1         17
#    7    0          0         23
#    7   50          0         NA
#    7  120          1         18
#   10   60          1         24
#   10  140          1         20

# Value of `col` for one subject at one landmark.
val <- function(lm, id, s, col) lm[[col]][lm$.id == id & lm$.landmark == s]

# LOCF values -----------------------------------------------------------------

test_that("LOCF values match hand-computed values", {
  lm <- lm_cov()
  at <- function(s) lm[lm$.landmark == s, ]

  # s = 30: only visits at day 0 qualify
  expect_equal(at(30)$treatment, c(NA, NA, NA, 0, 0, NA, NA, NA))
  expect_equal(at(30)$biomarker, c(NA, NA, NA, 25, 23, NA, NA, NA))

  # s = 60: id 6 uses day 45; id 10's visit on day 60 is used (t = s)
  expect_equal(at(60)$treatment, c(NA, NA, 1, 0, NA, NA, 1))
  expect_equal(at(60)$biomarker, c(NA, NA, 20, 23, NA, NA, 24))

  # s = 90: id 6's visit on day 90 is used (t = s)
  expect_equal(at(90)$treatment, c(1, 0, NA, NA, 1))
  expect_equal(at(90)$biomarker, c(17, 23, NA, NA, 24))
})

test_that("each variable is carried forward separately", {
  lm <- lm_cov()
  # id 7, s = 60: the day-50 visit has treatment but a missing biomarker, so
  # treatment comes from day 50 and biomarker from day 0.
  expect_equal(val(lm, 7, 60, "treatment"), 0)
  expect_equal(val(lm, 7, 60, "biomarker"), 23)

  audit <- lm_covariate_info(lm)$details
  bm  <- audit[[2]]
  trt <- audit[[1]]
  expect_equal(trt$.obs_time[trt$.id == 7 & trt$.landmark == 60], 50)
  expect_equal(bm$.obs_time[bm$.id == 7 & bm$.landmark == 60], 0)
})

test_that("include_at_landmark = FALSE uses only measurements before the landmark", {
  lm <- lm_cov(include_at_landmark = FALSE)

  # id 10's first visit is on day 60, so nothing qualifies at s = 60
  expect_equal(val(lm, 10, 60, "biomarker"), NA_real_)
  expect_equal(val(lm, 10, 60, "treatment"), NA_real_)
  # id 6 at s = 90 falls back to day 45
  expect_equal(val(lm, 6, 90, "biomarker"), 20)
  # Values strictly before the landmark are unaffected
  expect_equal(val(lm, 10, 90, "biomarker"), 24)
  expect_equal(val(lm, 6, 60, "biomarker"), 20)
})

test_that("measurements after the landmark are never used", {
  lm <- lm_cov()
  audit <- dplyr::bind_rows(lm_covariate_info(lm)$details)
  used <- audit[!is.na(audit$.obs_time), ]
  expect_true(all(used$.obs_time <= used$.landmark))

  strict <- lm_cov(include_at_landmark = FALSE)
  audit <- dplyr::bind_rows(lm_covariate_info(strict)$details)
  used <- audit[!is.na(audit$.obs_time), ]
  expect_true(all(used$.obs_time < used$.landmark))

  # Adding later measurements with extreme values changes nothing
  late <- dplyr::bind_rows(
    visits,
    tibble::tibble(patient_id = c(6L, 7L, 10L), visit_day = c(91, 91, 91),
                   treatment = 99, biomarker = 999)
  )
  expect_equal(
    tibble::as_tibble(lm_cov(long = late)),
    tibble::as_tibble(lm)
  )
})

test_that("measurements from another subject are never used", {
  # Subject 3 has no visits; a visit for subject 4 must not leak to subject 3
  long <- dplyr::bind_rows(
    visits,
    tibble::tibble(patient_id = 4L, visit_day = 10, treatment = 1, biomarker = 50)
  )
  lm <- lm_cov(long = long)
  expect_equal(val(lm, 3, 30, "biomarker"), NA_real_)
  expect_equal(val(lm, 4, 30, "biomarker"), 50)
})

test_that("old measurements are carried forward and their lag is recorded", {
  lm <- lm_cov()
  bm <- lm_covariate_info(lm)$details[[2]]
  lag_of <- function(id, s) bm$.lag[bm$.id == id & bm$.landmark == s]

  expect_equal(lag_of(7, 90), 90)   # day-0 value carried to s = 90
  expect_equal(lag_of(10, 60), 0)   # measured at the landmark
  expect_equal(lag_of(6, 60), 15)
  expect_equal(lag_of(3, 30), NA_real_)
})

test_that("row order of the longitudinal data does not matter", {
  set.seed(1)
  shuffled <- visits[sample(nrow(visits)), ]
  expect_identical(lm_cov(long = shuffled), lm_cov())
})

test_that("measurements before time zero are allowed", {
  long <- dplyr::bind_rows(
    visits,
    tibble::tibble(patient_id = 3L, visit_day = -14, treatment = 0, biomarker = 30)
  )
  lm <- lm_cov(long = long)
  expect_equal(val(lm, 3, 30, "biomarker"), 30)
})

# Output structure --------------------------------------------------------------

test_that("risk sets, outcomes and class are unchanged; columns are appended", {
  base <- lm_spec()
  lm <- lm_cov(base)

  expect_s3_class(lm, "landmark_tbl")
  expect_named(lm, c(names(base), "treatment", "biomarker"))
  expect_equal(nrow(lm), nrow(base))
  expect_identical(lm[names(base)] |> tibble::as_tibble(), tibble::as_tibble(base))

  # The configuration from landmark_data() is kept
  cfg_old <- landmark_config(base)
  cfg_new <- landmark_config(lm)
  expect_equal(cfg_new[names(cfg_old)], cfg_old)
})

test_that("subjects without measurements stay in the data with NA", {
  lm <- lm_cov()
  expect_equal(sum(is.na(lm$biomarker)), 12)
  expect_true(all(c(3, 4, 5, 8, 9) %in% lm$.id[is.na(lm$biomarker)]))
})

test_that("column types and factor levels are preserved", {
  long <- visits |>
    dplyr::mutate(
      arm   = factor(c("A", "B", "B", "A", "A", "B", "B", "B"), levels = c("A", "B", "C")),
      site  = c("x", "y", "y", "x", "x", "z", "y", "y"),
      dose  = as.integer(treatment * 10),
      flag  = treatment == 1,
      seen  = as.Date("2020-01-01") + visit_day
    )
  lm <- lm_cov(long = long, locf = c(arm, site, dose, flag, seen))

  expect_s3_class(lm$arm, "factor")
  expect_equal(levels(lm$arm), c("A", "B", "C"))
  expect_type(lm$site, "character")
  expect_type(lm$dose, "integer")
  expect_type(lm$flag, "logical")
  expect_s3_class(lm$seen, "Date")

  expect_equal(as.character(val(lm, 6, 60, "arm")), "B")
  expect_equal(val(lm, 7, 90, "site"), "x")
  expect_equal(val(lm, 10, 90, "dose"), 10L)
})

test_that("tidyselect helpers and multiple variables work", {
  by_helper <- lm_cov(locf = tidyselect::where(is.double) & !visit_day)
  expect_identical(by_helper, lm_cov())

  by_string <- lm_cov(locf = c("treatment", "biomarker"))
  expect_identical(by_string, lm_cov())
})

test_that("a filtered landmark_tbl is handled", {
  only_60 <- dplyr::filter(lm_spec(), .landmark == 60)
  lm <- lm_cov(only_60)
  expect_equal(nrow(lm), 7)
  expect_equal(lm$biomarker, c(NA, NA, 20, 23, NA, NA, 24))
})

test_that("an empty landmark and empty longitudinal data are handled", {
  base <- suppressWarnings(lm_spec(landmarks = c(30, 500)))
  expect_equal(nrow(lm_cov(base)), nrow(base))

  empty <- visits[0, ]
  lm <- lm_cov(long = empty)
  expect_true(all(is.na(lm$biomarker)))
  expect_type(lm$biomarker, "double")
})

test_that("grouped longitudinal data are ungrouped first", {
  grouped <- dplyr::group_by(visits, patient_id)
  expect_identical(lm_cov(long = grouped), lm_cov())
})

test_that("user columns with internal-looking names don't break anything", {
  long <- dplyr::rename(visits, .obs_time = biomarker)
  lm <- lm_cov(long = long, locf = .obs_time)
  expect_equal(val(lm, 6, 90, ".obs_time"), 17)
})

# Same-time conflicts and unmatched IDs ------------------------------------------

test_that("conflicting values at the same time error and list the subjects and times", {
  long <- dplyr::bind_rows(
    visits,
    tibble::tibble(patient_id = c(6L, 10L), visit_day = c(45, 60),
                   treatment = c(1, 1), biomarker = c(21, 25))
  )
  expect_error(lm_cov(long = long), class = "tidylandmark_error_conflict")
  expect_error(lm_cov(long = long), "id 6 at time 45")
  expect_error(lm_cov(long = long), "id 10 at time 60")
  expect_error(lm_cov(long = long), "biomarker")

  # The conflict is only in biomarker; treatment alone is fine
  expect_no_error(lm_cov(long = long, locf = treatment))
})

test_that("exact duplicates and a duplicate with a missing value are not conflicts", {
  exact <- dplyr::bind_rows(visits, visits[2, ])
  expect_identical(lm_cov(long = exact), lm_cov())

  with_na <- dplyr::bind_rows(
    visits,
    tibble::tibble(patient_id = 6L, visit_day = 45, treatment = NA, biomarker = NA)
  )
  expect_identical(lm_cov(long = with_na), lm_cov())
})

test_that("rows for subjects not in the landmark data are ignored and counted", {
  long <- dplyr::bind_rows(
    visits,
    tibble::tibble(patient_id = c(1L, 99L, 99L), visit_day = c(0, 0, 10),
                   treatment = 1, biomarker = 1)
  )
  expect_message(lm <- lm_cov(long = long), "Ignored 3 rows")
  expect_identical(lm, lm_cov())

  expect_no_message(lm_cov())
})

# Names, collisions and chaining -------------------------------------------------

test_that("names accepts a {.col}/{.method} template; renaming in the selection also works", {
  lm <- lm_cov(names = "{.col}_{.method}")
  expect_true(all(c("treatment_locf", "biomarker_locf") %in% names(lm)))

  lm <- lm_cov(locf = c(trt = treatment, bm = biomarker))
  expect_true(all(c("trt", "bm") %in% names(lm)))
  expect_equal(lm$bm, lm_cov()$biomarker)

  # A renamed column keeps its name; the template applies to the rest
  lm <- lm_cov(locf = c(trt = treatment, biomarker), names = "{.col}_x")
  expect_true(all(c("trt", "biomarker_x") %in% names(lm)))
})

test_that("existing columns are never overwritten", {
  base <- lm_spec()

  # Baseline column with the same name as a longitudinal variable
  long <- dplyr::mutate(visits, age = 1)
  expect_error(lm_cov(base, long = long, locf = age), "already exist")
  expect_no_error(lm_cov(base, long = long, locf = c(age_visit = age)))

  # Reserved names
  expect_error(lm_cov(locf = c(.time = biomarker)), "already exist")

  # A second call for the same variable needs new names
  once <- lm_cov(locf = biomarker)
  expect_error(lm_cov(once, locf = biomarker), "already exist")
  expect_error(lm_cov(once, locf = biomarker), "method")
})

test_that("invalid names are rejected", {
  expect_error(lm_cov(names = c("a", "b")), "single string")
  expect_error(lm_cov(names = "same"), "single string")
  expect_error(lm_cov(names = NA_character_), "single string")
  expect_error(lm_cov(names = 1), "single string")
  expect_error(lm_cov(locf = c(x = treatment, x = biomarker)), "unique")
})

test_that("calls can be chained and provenance accumulates", {
  lm <- lm_spec() |>
    lm_cov(locf = treatment) |>
    lm_cov(locf = biomarker, names = "{.col}_prior", include_at_landmark = FALSE) |>
    lm_cov(locf = biomarker)

  info <- lm_covariate_info(lm)
  expect_equal(info$column, c("treatment", "biomarker_prior", "biomarker"))
  expect_equal(info$source, c("treatment", "biomarker", "biomarker"))
  expect_equal(info$include_at_landmark, c(TRUE, FALSE, TRUE))

  # The same marker, two ways
  expect_equal(val(lm, 6, 90, "biomarker"), 17)
  expect_equal(val(lm, 6, 90, "biomarker_prior"), 20)
})

test_that("provenance records method, source, missingness and the audit trail", {
  info <- lm_covariate_info(lm_cov())

  expect_s3_class(info, "tbl_df")
  expect_named(
    info,
    c("column", "source", "method", "include_at_landmark", "lookback",
      "random_slope", "adjust", "n_missing", "details")
  )
  expect_equal(info$method, c("locf", "locf"))
  expect_equal(info$n_missing, c(12L, 12L))
  expect_equal(info$lookback, c(NA_real_, NA_real_))

  audit <- info$details[[1]]
  expect_named(audit, c(".id", ".landmark", ".obs_time", ".lag"))
  expect_equal(nrow(audit), 20)

  expect_null(lm_covariate_info(lm_spec()))
})

test_that("dropping a derived column and re-adding it leaves one record", {
  lm <- lm_cov(locf = biomarker) |>
    dplyr::select(-biomarker) |>
    lm_cov(locf = biomarker)
  expect_equal(lm_covariate_info(lm)$column, "biomarker")
})

# Printing and tidy() ------------------------------------------------------------

test_that("print() lists derived covariates that are still present", {
  lm <- lm_cov(locf = treatment) |>
    lm_cov(locf = c(bm_prior = biomarker), include_at_landmark = FALSE)

  expect_output(
    print(lm),
    "Derived covariates: treatment \\(LOCF\\), bm_prior \\(LOCF of biomarker, t < s\\)"
  )

  dropped <- dplyr::select(lm, -treatment)
  expect_output(print(dropped), "Derived covariates: bm_prior")
  expect_false(any(grepl("treatment", lm_header(dropped, landmark_config(dropped)))))

  expect_false(any(grepl("Derived", lm_header(lm_spec(), landmark_config(lm_spec())))))
})

test_that("tidy() still reports outcome counts after adding covariates", {
  expect_equal(tidy(lm_cov()), tidy(lm_spec()))
})

# Input validation -----------------------------------------------------------------

test_that("landmark_data must be an intact landmark_tbl", {
  expect_error(
    add_landmark_covariates(subjects, visits, patient_id, visit_day, locf = biomarker),
    "landmark_tbl"
  )
  broken <- dplyr::select(lm_spec(), -.status)
  expect_error(lm_cov(broken), "\\.status")
})

test_that("longitudinal_data and required arguments are validated", {
  expect_error(
    add_landmark_covariates(lm_spec(), list(a = 1), a, a, locf = a),
    "data frame"
  )
  expect_error(
    add_landmark_covariates(lm_spec(), visits, patient_id),
    "must be supplied"
  )
  expect_error(
    add_landmark_covariates(lm_spec(), visits, nope, visit_day, locf = biomarker),
    "exist"
  )
  expect_error(
    add_landmark_covariates(lm_spec(), visits, patient_id, patient_id, locf = biomarker),
    "different columns"
  )
})

test_that("locf and lme must select covariate columns", {
  expect_error(lm_cov(locf = c(biomarker, visit_day)), "must not include")
  expect_error(lm_cov(locf = patient_id), "must not include")
  expect_error(lm_cov(locf = tidyselect::starts_with("zzz")), "at least one")
  expect_error(lm_cov(locf = NULL, lme = visit_day), "must not include")
  expect_error(lm_cov(locf = NULL), "at least one of|Select variables")
})

test_that("flags are validated", {
  expect_error(lm_cov(include_at_landmark = "yes"), "TRUE.*FALSE")
  expect_error(
    add_landmark_covariates(lm_spec(), visits, patient_id, visit_day,
                            locf = biomarker, progress = NA),
    "TRUE.*FALSE"
  )
})

test_that("id and time contents are validated", {
  d <- visits; d$patient_id[1] <- NA
  expect_error(lm_cov(long = d), "missing values")

  d <- visits; d$visit_day[1] <- NA
  expect_error(lm_cov(long = d), "missing values")

  d <- visits; d$visit_day[1] <- Inf
  expect_error(lm_cov(long = d), "finite")

  d <- visits; d$visit_day <- as.character(d$visit_day)
  expect_error(lm_cov(long = d), "numeric column")
})

test_that("ID types must match exactly", {
  d <- visits; d$patient_id <- as.double(d$patient_id)
  expect_error(lm_cov(long = d), "same type")

  d <- visits; d$patient_id <- as.character(d$patient_id)
  expect_error(lm_cov(long = d), "same type")

  # Matching character IDs work
  s <- subjects; s$patient_id <- as.character(s$patient_id)
  d <- visits;   d$patient_id <- as.character(d$patient_id)
  lm <- lm_cov(lm_spec(s), long = d)
  expect_equal(val(lm, "6", 90, "biomarker"), 17)
})

# LOCF lookback ------------------------------------------------------------------

test_that("lookback drops carried-forward values older than the window", {
  lm <- lm_cov(lookback = 30)

  # id 6 at s = 60: day-45 value is 15 days old, kept
  expect_equal(val(lm, 6, 60, "biomarker"), 20)
  # id 7 at s = 90: treatment from day 50 (40 days) and biomarker from day 0
  # (90 days) are both too old
  expect_equal(val(lm, 7, 90, "treatment"), NA_real_)
  expect_equal(val(lm, 7, 90, "biomarker"), NA_real_)
  # id 10 at s = 90: day-60 value is exactly 30 days old, kept (s - t <= lookback)
  expect_equal(val(lm, 10, 90, "biomarker"), 24)

  # The audit trail shows no measurement for dropped values
  bm <- lm_covariate_info(lm)$details[[2]]
  expect_true(is.na(bm$.obs_time[bm$.id == 7 & bm$.landmark == 90]))
  expect_true(all(bm$.lag <= 30, na.rm = TRUE))
  expect_equal(lm_covariate_info(lm)$lookback, c(30, 30))
})

test_that("lookback = 0 keeps only measurements taken at the landmark", {
  lm <- lm_cov(lookback = 0)
  expect_equal(val(lm, 10, 60, "biomarker"), 24)
  expect_equal(val(lm, 6, 90, "biomarker"), 17)
  expect_equal(sum(!is.na(lm$biomarker)), 2)

  # ...so with include_at_landmark = FALSE nothing qualifies
  expect_true(all(is.na(lm_cov(lookback = 0, include_at_landmark = FALSE)$biomarker)))
})

test_that("a lookback longer than any lag changes nothing", {
  long_window <- lm_cov(lookback = 1e6)
  expect_equal(long_window$biomarker, lm_cov()$biomarker)
  expect_equal(long_window$treatment, lm_cov()$treatment)
})

test_that("lookback is validated and only allowed with locf variables", {
  expect_error(lm_cov(lookback = -1), "non-negative")
  expect_error(lm_cov(lookback = c(10, 20)), "single")
  expect_error(lm_cov(lookback = NA_real_), "single")
  expect_error(lm_cov(lookback = Inf), "NULL")
  expect_error(lm_cov(lookback = "30"), "single")
  expect_error(lm_cov(locf = NULL, lme = biomarker, lookback = 30), "only applies to")
})

# LME ----------------------------------------------------------------------------

lme_cov <- function(sim, lme = marker, ...) {
  add_landmark_covariates(
    sim$lm, sim$visits, id = pid, time = day,
    lme = {{ lme }}, progress = FALSE, ...
  )
}

test_that("random-intercept predictions match a direct lmer() fit", {
  sim <- sim_lme()
  lm <- lme_cov(sim)

  for (s in c(100, 200)) {
    ref <- ref_lme(sim, s)
    at_s <- lm[lm$.landmark == s, ]
    expect_equal(at_s$marker[match(names(ref), at_s$.id)], unname(ref), tolerance = 1e-6)
  }
})

test_that("random-slope predictions match a direct lmer() fit", {
  sim <- sim_lme()
  lm <- lme_cov(sim, lme_args = list(random_slope = TRUE))

  ref <- ref_lme(sim, 200, random_slope = TRUE, time_unit = 100)
  at_s <- lm[lm$.landmark == 200, ]
  expect_equal(at_s$marker[match(names(ref), at_s$.id)], unname(ref), tolerance = 1e-5)
  expect_true(lm_covariate_info(lm)$random_slope)
})

test_that("adjusted predictions match a direct lmer() fit", {
  sim <- sim_lme()

  lm <- lme_cov(sim, lme_args = list(adjust = ~ age))
  ref <- ref_lme(sim, 200, rhs = "+ age")
  at_s <- lm[lm$.landmark == 200, ]
  expect_equal(at_s$marker[match(names(ref), at_s$.id)], unname(ref), tolerance = 1e-6)
  expect_equal(lm_covariate_info(lm)$adjust, "~age")

  # Interactions with time use `.t`
  lm <- lme_cov(sim, lme_args = list(adjust = ~ grp * .t))
  ref <- ref_lme(sim, 200, rhs = "+ grp * tt")
  at_s <- lm[lm$.landmark == 200, ]
  expect_equal(at_s$marker[match(names(ref), at_s$.id)], unname(ref), tolerance = 1e-6)
})

test_that("include_at_landmark = FALSE drops measurements at the landmark from the fit", {
  sim <- sim_lme()
  # Put a visit for everyone exactly on day 200
  extra <- tibble::tibble(pid = sim$subjects$pid, day = 200, marker = 100)
  sim$visits <- dplyr::bind_rows(sim$visits, extra)

  lm <- lme_cov(sim, include_at_landmark = FALSE)
  ref <- ref_lme(sim, 200, include_at_landmark = FALSE)
  at_s <- lm[lm$.landmark == 200, ]
  expect_equal(at_s$marker[match(names(ref), at_s$.id)], unname(ref), tolerance = 1e-6)
  expect_true(all(at_s$marker < 50, na.rm = TRUE))
})

test_that("later measurements never affect the fit", {
  sim <- sim_lme()
  base <- lme_cov(sim)
  sim$visits <- dplyr::bind_rows(
    sim$visits,
    tibble::tibble(pid = sim$subjects$pid, day = 201, marker = 1e4)
  )
  expect_equal(lme_cov(sim)$marker, base$marker)
})

test_that("subjects without history get NA; a single measurement is enough", {
  sim <- sim_lme()
  lm <- lme_cov(sim)

  # Subject 1's first visit is after both landmarks
  expect_true(all(is.na(lm$marker[lm$.id == 1])))
  # Subject 2's single visit (day 50) gives a prediction at both landmarks
  expect_false(any(is.na(lm$marker[lm$.id == 2])))
  expect_equal(sum(is.na(lm$marker)), 2)
})

test_that("subjects with missing adjustment values get NA and are left out of the fit", {
  sim <- sim_lme()
  sim$lm$age[sim$lm$.id == 3] <- NA
  lm <- lme_cov(sim, lme_args = list(adjust = ~ age))
  expect_true(all(is.na(lm$marker[lm$.id == 3])))

  diag <- lm_covariate_info(lm)$details[[1]]
  expect_equal(diag$n_subjects, c(28L, 28L))  # 30 minus subjects 1 and 3
})

test_that("diagnostics record sample sizes, variance components and the model", {
  sim <- sim_lme()
  diag <- lm_covariate_info(lme_cov(sim))$details[[1]]

  expect_named(
    diag,
    c(".landmark", "status", "message", "n_eligible", "n_subjects", "n_obs",
      "median_obs", "time_min", "time_max", "time_scale", "slope",
      "var_intercept", "var_slope", "var_residual", "singular", "model")
  )
  expect_equal(diag$.landmark, c(100, 200))
  expect_equal(diag$status, c("ok", "ok"))
  expect_equal(diag$n_eligible, c(30L, 30L))
  expect_equal(diag$n_subjects, c(29L, 29L))
  expect_true(all(purrr::map_lgl(diag$model, \(m) inherits(m, "merMod"))))
  expect_true(all(is.na(diag$var_slope)))

  # Slope is reported per original time unit and matches lmer on unscaled time
  obs <- sim$visits[sim$visits$day <= 200, ]
  obs$tt <- obs$day - 200
  direct <- lme4::lmer(marker ~ tt + (1 | pid), data = obs)
  expect_equal(diag$slope[2], unname(lme4::fixef(direct)["tt"]), tolerance = 1e-6)
  expect_equal(diag$var_residual[2], sigma(direct)^2, tolerance = 1e-6)
})

test_that("too few subjects fails that fit only, with NA values and a warning", {
  # Spec fixture: biomarker history exists for 2 subjects at s = 30 and
  # 3 subjects at s = 60 and 90.
  expect_warning(
    lm <- lm_cov(locf = treatment, lme = biomarker, lme_args = list(min_subjects = 3)),
    "did not fit cleanly"
  )
  diag <- lm_covariate_info(lm)$details[[2]]
  expect_equal(diag$status[1], "failed")
  expect_match(diag$message[1], "2 subject")
  expect_true(all(is.na(lm$biomarker[lm$.landmark == 30])))

  # Other landmarks and the LOCF variable are unaffected
  expect_false(all(is.na(lm$biomarker[lm$.landmark == 60])))
  expect_equal(lm$treatment, lm_cov(locf = treatment)$treatment)
})

test_that("random slopes need enough subjects with repeated measurements", {
  # 7 subjects have history (subject 1 has none), but subject 2 has only
  # one measurement, so only 6 can inform a slope.
  sim <- sim_lme(n = 8)
  expect_warning(
    lm <- lme_cov(sim, lme_args = list(random_slope = TRUE, min_subjects = 7)),
    "2\\+ measurements"
  )
  expect_true(all(lm_covariate_info(lm)$details[[1]]$status == "failed"))
})

test_that("lmer() errors are caught and reported per fit", {
  sim <- sim_lme()
  sim$lm$grp <- factor("a")  # a one-level factor can't be a fixed effect
  expect_warning(
    lm <- lme_cov(sim, lme_args = list(adjust = ~ grp)),
    "contrasts"
  )
  diag <- lm_covariate_info(lm)$details[[1]]
  expect_equal(diag$status, c("failed", "failed"))
  expect_true(all(is.na(lm$marker)))
})

test_that("singular fits are flagged, not hidden", {
  sim <- sim_lme()
  # Pure noise with no between-subject variation; with this seed the
  # random-intercept variance is estimated as exactly zero at both landmarks.
  set.seed(2)
  sim$visits$marker <- 5 + stats::rnorm(nrow(sim$visits), sd = 1)
  expect_warning(lm <- lme_cov(sim), "singular")

  diag <- lm_covariate_info(lm)$details[[1]]
  expect_true(all(diag$singular))
  expect_true(all(diag$status == "warning"))
  expect_false(any(is.na(lm$marker[lm$.id != 1])))
})

test_that("LOCF and LME versions of the same marker can sit side by side", {
  sim <- sim_lme()
  lm <- add_landmark_covariates(
    sim$lm, sim$visits, pid, day,
    locf = marker, lme = marker, names = "{.col}_{.method}", progress = FALSE
  )
  expect_true(all(c("marker_locf", "marker_lme") %in% names(lm)))
  expect_equal(lm_covariate_info(lm)$method, c("locf", "lme"))

  # Without distinct names the two collide
  expect_error(
    add_landmark_covariates(sim$lm, sim$visits, pid, day,
                            locf = marker, lme = marker, progress = FALSE),
    "different name"
  )
})

test_that("print() shows LME options", {
  sim <- sim_lme()
  lm <- lme_cov(sim, lme = c(m_rs = marker),
                lme_args = list(random_slope = TRUE, adjust = ~ age))
  expect_output(print(lm), "m_rs \\(LME of marker, random slope, adjusted ~age\\)")
})

test_that("lme variables must be continuous numeric", {
  long <- dplyr::mutate(
    visits,
    arm = factor(ifelse(treatment == 1, "B", "A")),
    site = "x",
    on_trt = treatment == 1
  )
  expect_error(lm_cov(long = long, locf = NULL, lme = arm), "numeric")
  expect_error(lm_cov(long = long, locf = NULL, lme = site), "numeric")
  expect_error(lm_cov(long = long, locf = NULL, lme = on_trt), "numeric")
  expect_error(lm_cov(locf = NULL, lme = treatment), "binary")
})

test_that("lme_args is validated", {
  sim <- sim_lme()
  expect_error(lme_cov(sim, lme_args = list(TRUE)), "named list")
  expect_error(lme_cov(sim, lme_args = "slope"), "named list")
  expect_error(lme_cov(sim, lme_args = list(randomslope = TRUE)), "unknown")
  expect_error(lme_cov(sim, lme_args = list(random_slope = NA)), "TRUE.*FALSE")
  expect_error(lme_cov(sim, lme_args = list(REML = "yes")), "TRUE.*FALSE")
  expect_error(lme_cov(sim, lme_args = list(min_subjects = 1)), "at least 2")
  expect_error(lme_cov(sim, lme_args = list(min_subjects = 2.5)), "whole number")
  expect_error(lme_cov(sim, lme_args = list(control = list())), "lmerControl")
  expect_error(lme_cov(sim, lme_args = list(adjust = "age")), "one-sided formula")
  expect_error(lme_cov(sim, lme_args = list(adjust = y ~ age)), "one-sided formula")
  expect_error(lme_cov(sim, lme_args = list(adjust = ~ height)), "height")
  expect_error(lme_cov(sim, lme_args = list(adjust = ~ .status)), "leak")

  # lme_args without lme variables
  expect_error(lm_cov(lme_args = list(random_slope = TRUE)), "only applies to")
})

test_that("lmerControl() options are passed through", {
  sim <- sim_lme()
  ctrl <- lme4::lmerControl(optimizer = "Nelder_Mead")
  lm <- lme_cov(sim, lme_args = list(control = ctrl))
  model <- lm_covariate_info(lm)$details[[1]]$model[[1]]
  expect_equal(model@optinfo$optimizer, "Nelder_Mead")
})

test_that("the bundled PBC example works end to end", {
  v <- transform(pbcseq_visits, log_bili = log(bili))
  lm <- landmark_data(pbcseq_baseline, id, futime, status, landmarks = c(365, 730), horizon = 1825)
  expect_message(
    out <- add_landmark_covariates(
      lm, v, id, day, locf = albumin, lme = log_bili,
      lme_args = list(adjust = ~ age), progress = FALSE
    ),
    "Ignored"
  )
  expect_equal(nrow(out), nrow(lm))
  expect_equal(lm_covariate_info(out)$details[[2]]$status, c("ok", "ok"))
})
