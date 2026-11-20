# Shared fixtures. testthat sources every helper-*.R file before running tests.

# Subject-level example.
subjects <- tibble::tibble(
  patient_id    = 1:10,
  followup_days = c(20, 30, 48, 65, 85, 110, 145, 190, 250, 270),
  event         = c(1, 2, 1, 2, 0, 1, 2, 1, 0, 1),
  age           = c(12, 15, 11, 17, 9, 14, 13, 16, 10, 15),
  sex           = rep(c("F", "M"), 5)
)

# Convenience wrapper so tests only state what they vary.
lm_spec <- function(data = subjects, landmarks = c(30, 60, 90), horizon = 180) {
  landmark_data(
    data,
    id = patient_id, time = followup_days, status = event,
    landmarks = landmarks, horizon = horizon
  )
}

# Longitudinal example from spec section 4.2. IDs are integer to match
# `subjects$patient_id` (types are never coerced).
visits <- tibble::tibble(
  patient_id = c(6L, 6L, 6L, 7L, 7L, 7L, 10L, 10L),
  visit_day  = c(0, 45, 90, 0, 50, 120, 60, 140),
  treatment  = c(0, 1, 1, 0, 0, 1, 1, 1),
  biomarker  = c(25, 20, 17, 23, NA, 18, 24, 20)
)

# Landmark data (30/60/90, horizon 180) with covariates from `visits`.
# By default both variables use LOCF; pass `locf = NULL, lme = ...` for LME.
lm_cov <- function(lm = lm_spec(), long = visits, locf = c(treatment, biomarker), ...) {
  add_landmark_covariates(
    lm, long,
    id = patient_id, time = visit_day, locf = {{ locf }}, progress = FALSE, ...
  )
}

# Simulated data for mixed models: everyone is followed to day 1000, so all
# subjects are at risk at landmarks 100 and 200. Marker trajectories have
# subject-specific intercepts and slopes and depend on age.
#   * subject 1's first visit is on day 250: no history at either landmark.
#   * subject 2 has a single visit, on day 50.
sim_lme <- function(n = 30, seed = 42) {
  set.seed(seed)
  subj <- tibble::tibble(
    pid    = seq_len(n),
    futime = 1000,
    status = rep(c(0, 1), length.out = n),
    age    = round(stats::runif(n, 40, 70)),
    grp    = factor(rep(c("a", "b"), length.out = n))
  )
  b0 <- stats::rnorm(n, sd = 1.5)
  b1 <- stats::rnorm(n, sd = 0.004)
  long <- tidy_expand(subj, days = seq(0, 300, by = 30))
  long <- long[stats::runif(nrow(long)) > 0.25 | long$day == 0, ]
  long <- long[!(long$pid == 1 & long$day < 250), ]
  long <- long[!(long$pid == 2 & long$day != 0), ]
  long$day[long$pid == 2] <- 50
  long$marker <- 10 + 0.05 * long$age + b0[long$pid] +
    (0.01 + b1[long$pid]) * long$day + stats::rnorm(nrow(long), sd = 0.5)
  list(
    subjects = subj,
    visits   = long[c("pid", "day", "marker")],
    lm       = landmark_data(subj, pid, futime, status, landmarks = c(100, 200))
  )
}

tidy_expand <- function(subj, days) {
  tibble::tibble(
    pid = rep(subj$pid, each = length(days)),
    age = rep(subj$age, each = length(days)),
    day = rep(days, times = nrow(subj))
  )
}

# Reference: fit lmer() directly and predict at the landmark. `time_unit`
# rescales time; random-slope fits on raw days fail to converge, so those
# references use 100-day units (predictions at the landmark don't depend on it).
ref_lme <- function(sim, s, rhs = "", random_slope = FALSE,
                    include_at_landmark = TRUE, time_unit = 1) {
  obs <- sim$visits[
    (if (include_at_landmark) sim$visits$day <= s else sim$visits$day < s) &
      !is.na(sim$visits$marker), ]
  obs <- merge(obs, sim$subjects[c("pid", "age", "grp")], by = "pid")
  obs$tt <- (obs$day - s) / time_unit
  re <- if (random_slope) "(1 + tt | pid)" else "(1 | pid)"
  f <- stats::as.formula(paste("marker ~ tt", rhs, "+", re))
  fit <- lme4::lmer(f, data = obs)
  nd <- sim$subjects[sim$subjects$pid %in% obs$pid, c("pid", "age", "grp")]
  nd$tt <- 0
  stats::setNames(unname(stats::predict(fit, newdata = nd, re.form = NULL)), nd$pid)
}
