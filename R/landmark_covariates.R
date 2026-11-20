#' Inspect how derived covariates were made
#'
#' @description
#' `landmark_covariates()` returns one row per covariate added by
#' [add_landmark_covariates()], with the method and options used and how
#' complete it is in the current rows.
#'
#' `landmark_lme_fits()` returns one row per mixed model (each `lme` variable
#' at each landmark), with fit status, sample sizes, variance components and
#' the fitted [lme4::lmer()] object in the `model` column.
#'
#' Only covariates still present in `x` are included, so columns removed with
#' `dplyr::select()` drop out.
#'
#' @param x A `landmark_tbl` returned by [add_landmark_covariates()].
#'
#' @returns
#' `landmark_covariates()`: a tibble with columns `column`, `source`,
#' `method`, `include_at_landmark`, `lookback`, `random_slope`, `adjust`,
#' `n_rows`, `n_missing`, and, for LOCF, `median_lag` and `max_lag` (time
#' between each carried-forward measurement and its landmark). Counts and lags
#' are computed on the current rows, so they follow `dplyr::filter()`.
#'
#' `landmark_lme_fits()`: a tibble with columns `column`, `source`,
#' `.landmark`, `status` (`"ok"`, `"warning"` or `"failed"`), `message`,
#' `n_eligible`, `n_subjects`, `n_obs`, `median_obs`, `time_min`, `time_max`,
#' `time_scale`, `slope` (per original time unit), `var_intercept`,
#' `var_slope`, `var_residual`, `singular` and `model` (`NULL` for failed
#' fits). In each model, time (`.t`) is centred at the landmark and divided by
#' `time_scale`.
#'
#' Both return a zero-row tibble with the same columns when there is nothing
#' to report.
#'
#' @seealso [add_landmark_covariates()]
#' @examples
#' visits_pbc <- transform(pbcseq_visits, log_bili = log(bili))
#' pbc_lm <- pbcseq_baseline |>
#'   landmark_data(id, futime, status, landmarks = c(365, 730), horizon = 1825) |>
#'   add_landmark_covariates(
#'     visits_pbc, id, day,
#'     locf = albumin, lme = log_bili, lme_args = list(adjust = ~ age)
#'   )
#'
#' landmark_covariates(pbc_lm)
#'
#' fits <- landmark_lme_fits(pbc_lm)
#' fits
#' summary(fits$model[[1]])
#' @export
landmark_covariates <- function(x) {
  lm_check_intact(x, call = rlang::current_env())
  info <- lm_present_covariates(x)

  keys <- tibble::tibble(.id = x$.id, .landmark = x$.landmark)
  lag_stats <- purrr::map2(info$method, info$details, \(method, audit) {
    if (method != "locf") {
      return(c(NA_real_, NA_real_))
    }
    # The audit covers the rows at creation; keep the ones still present.
    lags <- dplyr::semi_join(audit, keys, by = c(".id", ".landmark"))$.lag
    if (all(is.na(lags))) {
      return(c(NA_real_, NA_real_))
    }
    c(stats::median(lags, na.rm = TRUE), max(lags, na.rm = TRUE))
  })

  tibble::tibble(
    column              = info$column,
    source              = info$source,
    method              = info$method,
    include_at_landmark = info$include_at_landmark,
    lookback            = info$lookback,
    random_slope        = info$random_slope,
    adjust              = info$adjust,
    n_rows              = rep(nrow(x), nrow(info)),
    n_missing           = purrr::map_int(info$column, \(col) sum(is.na(x[[col]]))),
    median_lag          = purrr::map_dbl(lag_stats, 1),
    max_lag             = purrr::map_dbl(lag_stats, 2)
  )
}

#' @rdname landmark_covariates
#' @export
landmark_lme_fits <- function(x) {
  lm_check_intact(x, call = rlang::current_env())
  info <- lm_present_covariates(x)
  info <- info[info$method == "lme", , drop = FALSE]

  fits <- purrr::pmap(
    list(info$column, info$source, info$details),
    \(column, source, details) {
      tibble::add_column(details, column = column, source = source, .before = 1)
    }
  )
  out <- purrr::list_rbind(fits)
  if (nrow(out) == 0L) {
    return(lm_empty_lme_fits())
  }
  out
}

#' Provenance rows for covariates still in the data (empty tibble if none)
#' @noRd
lm_present_covariates <- function(x) {
  info <- lm_covariate_info(x)
  if (is.null(info)) {
    return(tibble::tibble(
      column = character(), source = character(), method = character(),
      include_at_landmark = logical(), lookback = double(),
      random_slope = logical(), adjust = character(),
      n_missing = integer(), details = list()
    ))
  }
  info[info$column %in% names(x), , drop = FALSE]
}

#' Zero-row result of landmark_lme_fits()
#' @noRd
lm_empty_lme_fits <- function() {
  tibble::tibble(
    column = character(), source = character(), .landmark = double(),
    status = character(), message = character(), n_eligible = integer(),
    n_subjects = integer(), n_obs = integer(), median_obs = double(),
    time_min = double(), time_max = double(), time_scale = double(),
    slope = double(), var_intercept = double(), var_slope = double(),
    var_residual = double(), singular = logical(), model = list()
  )
}
