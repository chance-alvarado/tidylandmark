# Internal input-validation helpers ------------------------------------------
#
# None of these are exported. Each one takes:
#   * `arg`  - the user-facing argument name, used in error messages, and
#   * `call` - the environment of the user-facing function (e.g.
#              `landmark_data()`), so errors read "Error in `landmark_data()`"
#              instead of naming the internal helper.
# They either return a cleaned value or throw a classed rlang error via cli.

# Column names that tidylandmark creates and needs to function.
lm_reserved_cols <- c(".id", ".landmark", ".time", ".status", ".horizon")

#' Is `x` still a usable landmark_tbl?
#'
#' TRUE when the configuration attribute exists and every reserved column is
#' present. dplyr verbs keep the attribute, but `select()` can drop reserved
#' columns, so both are checked.
#' @noRd
lm_is_intact <- function(x) {
  !is.null(landmark_config(x)) && all(lm_reserved_cols %in% names(x))
}

#' Error unless `x` is an intact landmark_tbl.
#' @noRd
lm_check_intact <- function(x, arg = "x", call = rlang::caller_env()) {
  if (!inherits(x, "landmark_tbl")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a {.cls landmark_tbl}, not {.cls {class(x)}}.",
        "i" = "Create one with {.fn landmark_data}."
      ),
      call = call
    )
  }
  if (!lm_is_intact(x)) {
    missing_cols <- setdiff(lm_reserved_cols, names(x))
    problem <- if (length(missing_cols) > 0L) {
      "Reserved column{?s} {.field {missing_cols}} {?is/are} missing."
    } else {
      "Its {.field landmark_config} attribute is missing."
    }
    cli::cli_abort(
      c(
        "{.arg {arg}} is no longer a valid {.cls landmark_tbl}.",
        "x" = problem,
        "i" = "Avoid dropping reserved columns, or rebuild with {.fn landmark_data}."
      ),
      call = call
    )
  }
  invisible(x)
}

#' Check that label-based column names are usable
#'
#' `new` are the proposed names, `old` the names they replace and `existing`
#' all current column names. Errors if two labels give the same name or a
#' label clashes with another column (e.g. a level called "censored").
#' @noRd
lm_check_label_names <- function(new, old, existing, call = rlang::caller_env()) {
  clash <- unique(c(
    new[duplicated(new)],
    intersect(new, setdiff(existing, old))
  ))
  if (length(clash) > 0L) {
    cli::cli_abort(
      c(
        "Can't name event columns after the {.arg status} levels.",
        "x" = "Column name{?s} {.field {clash}} would not be unique.",
        "i" = "Rename the factor levels, or use {.code status_labels = FALSE}."
      ),
      call = call
    )
  }
  invisible(new)
}

#' Resolve a tidy-select argument to exactly one column name
#'
#' Uses tidyselect so `id`, `time` and `status` accept the same syntax as
#' `vars` in `add_landmark_covariates()`: bare names, strings, or
#' `all_of(x)`. A non-existent column triggers tidyselect's own error.
#' @noRd
lm_select_one <- function(data, quo, arg, call = rlang::caller_env()) {
  pos <- tidyselect::eval_select(
    quo,
    data = data,
    allow_rename = FALSE,
    error_call = call
  )
  if (length(pos) != 1L) {
    cli::cli_abort(
      "{.arg {arg}} must select exactly one column, not {length(pos)}.",
      call = call
    )
  }
  names(pos)
}

#' Refuse user input if columns `.id`, `.time`, ..., already exist.
#' Necessary so we don't overwrite user data. Will also prevent
#' running `landmark_data()` on an already created `landmark_tbl`.
#' @noRd
lm_check_reserved <- function(data, call = rlang::caller_env()) {
  clash <- intersect(names(data), lm_reserved_cols)
  if (length(clash) > 0L) {
    cli::cli_abort(
      c(
        "{.arg data} already contains reserved column name{?s} {.field {clash}}.",
        "i" = "Rename the conflicting column(s) before calling {.fn landmark_data}."
      ),
      call = call
    )
  }
  invisible(data)
}

#' Check for missing data in user-supplied data.
#' @noRd
lm_check_no_na <- function(x, arg, call = rlang::caller_env()) {
  n_na <- sum(is.na(x))
  if (n_na > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must not contain missing values.",
        "x" = "Found {n_na} missing value{?s}."
      ),
      call = call
    )
  }
  invisible(x)
}

#' Check that only one row per subject exists. Longitudinal data will be added
#' via `add_landmark_covariates()`.
#' @noRd
lm_check_unique <- function(x, arg, call = rlang::caller_env()) {
  dup <- unique(x[duplicated(x)])
  if (length(dup) > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must uniquely identify the rows of {.arg data}.",
        "x" = "Duplicated value{?s}: {.val {dup}}."
      ),
      call = call
    )
  }
  invisible(x)
}

#' Ensure time variable is valid.
#' @noRd
lm_check_time <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a numeric column, not {.cls {class(x)}}.",
      call = call
    )
  }
  n_bad <- sum(!is.finite(x) | x < 0)
  if (n_bad > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must contain non-negative, finite values.",
        "x" = "Found {n_bad} negative or infinite value{?s}."
      ),
      call = call
    )
  }
  invisible(x)
}

#' Validate and recode the status column to integer codes
#'
#' Returns a list with `codes` (integer: 0 = censored, 1, 2, ... = event
#' types), `labels` (NULL for numeric input; a named character vector mapping
#' code -> factor level for factor input) and `event_codes` (the event codes
#' that exist in the source coding).
#' @noRd
lm_code_status <- function(x, arg, call = rlang::caller_env()) {
  if (is.factor(x)) {
    lev <- levels(x)
    if (length(lev) < 2L) {
      cli::cli_abort(
        c(
          "A factor {.arg {arg}} must have at least two levels.",
          "i" = "The first level is censoring; later levels are event types."
        ),
        call = call
      )
    }
    labels <- lev
    names(labels) <- seq_along(lev) - 1L
    return(list(
      codes = as.integer(x) - 1L,
      labels = labels,
      event_codes = seq_len(length(lev) - 1L)
    ))
  }

  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a numeric or factor column, not {.cls {class(x)}}.",
        "i" = "Use 0 for censoring and 1, 2, ... for event types."
      ),
      call = call
    )
  }

  is_bad <- !is.finite(x) | x < 0 | x != trunc(x)
  if (any(is_bad)) {
    bad <- unique(x[is_bad])
    cli::cli_abort(
      c(
        "{.arg {arg}} must be coded as 0 (censored) or a positive integer (event type).",
        "x" = "Invalid value{?s}: {.val {bad}}."
      ),
      call = call
    )
  }

  codes <- as.integer(x)
  list(
    codes = codes,
    labels = NULL,
    event_codes = sort(unique(codes[codes > 0L]))
  )
}

#' Validate and normalize the landmark times.
#' @noRd
lm_check_landmarks <- function(x, arg = "landmarks", call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) == 0L) {
    cli::cli_abort(
      "{.arg {arg}} must be a non-empty numeric vector.",
      call = call
    )
  }
  if (!all(is.finite(x))) {
    cli::cli_abort("{.arg {arg}} must be finite and non-missing.", call = call)
  }
  if (any(x < 0)) {
    cli::cli_abort("{.arg {arg}} must be non-negative.", call = call)
  }
  dup <- unique(x[duplicated(x)])
  if (length(dup) > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must not contain duplicate values.",
        "x" = "Duplicated: {.val {dup}}."
      ),
      call = call
    )
  }
  sort(as.double(x))
}

#' Validate user-supplied horizon.
#'
#' Will change to allow multiple horizons in a future update.
#' @noRd
lm_check_horizon <- function(x, arg = "horizon", call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  # `||` short-circuits left to right, so `is.finite(x)` and `x <= 0` are only
  # evaluated once we know `x` is a single number; an NA therefore fails the
  # `is.finite()` test instead of reaching `if (NA)`.
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.code NULL} or a single positive, finite number.",
        "i" = "Use {.code horizon = NULL} for unbounded follow-up."
      ),
      call = call
    )
  }
  as.double(x)
}

#' Validate a single TRUE/FALSE argument (e.g. `progress`).
#'
#' `isTRUE()`/`isFALSE()` accept only a length-1, non-missing logical, so
#' `NA`, `c(TRUE, FALSE)`, `1` and `"yes"` all fail here instead of
#' reaching an `if ()` later.
#' @noRd
lm_check_flag <- function(x, arg, call = rlang::caller_env()) {
  if (!isTRUE(x) && !isFALSE(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
      call = call
    )
  }
  invisible(x)
}

#' Turn a `progress` flag into purrr's `.progress` argument.
#' @noRd
lm_progress_arg <- function(progress, label) {
  if (progress) label else FALSE
}

# Checks used by add_landmark_covariates() -----------------------------------

#' Resolve a covariate selection (`locf` or `lme`)
#'
#' Returns a character vector of source column names whose names are the
#' requested output names: tidyselect renaming (`c(new = old)`) sets the
#' name, otherwise it equals the source name. A NULL argument returns an
#' empty vector; an explicit selection that matches nothing is an error.
#' `exclude` holds the id and time columns, which can't be covariates.
#' @noRd
lm_select_vars <- function(data, quo, exclude, arg,
                           call = rlang::caller_env()) {
  if (rlang::quo_is_null(quo)) {
    return(rlang::set_names(character(0)))
  }
  pos <- tidyselect::eval_select(
    quo,
    data = data,
    allow_rename = TRUE,
    error_call = call
  )
  if (length(pos) == 0L) {
    cli::cli_abort("{.arg {arg}} must select at least one column.", call = call)
  }
  src <- rlang::set_names(base::names(data)[pos], base::names(pos))
  bad <- intersect(src, exclude)
  if (length(bad) > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must not include the {.arg id} or {.arg time} column.",
        "x" = "Selected: {.field {bad}}."
      ),
      call = call
    )
  }
  src
}

#' Work out the names of the derived columns
#'
#' `sel` is a selection from lm_select_vars() and `method` its method.
#' A column renamed in the selection keeps that name. Otherwise `names` is
#' NULL (use the source name) or one template where "{.col}" becomes the
#' source name and "{.method}" the method ("locf" or "lme").
#' @noRd
lm_output_names <- function(sel, method, names, arg = "names",
                            call = rlang::caller_env()) {
  out <- base::names(sel)
  if (is.null(names) || length(sel) == 0L) {
    return(out)
  }
  not_renamed <- out == unname(sel)
  out[not_renamed] <- vapply(
    unname(sel)[not_renamed],
    \(v) gsub("{.method}", method, gsub("{.col}", v, names, fixed = TRUE), fixed = TRUE),
    character(1),
    USE.NAMES = FALSE
  )
  out
}

#' Validate the `names` template
#' @noRd
lm_check_names_template <- function(names, arg = "names",
                                    call = rlang::caller_env()) {
  if (is.null(names)) {
    return(invisible(NULL))
  }
  ok <- is.character(names) && length(names) == 1L && !is.na(names) &&
    (grepl("{.col}", names, fixed = TRUE))
  if (!ok) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.code NULL} or a single string containing {.code {{.col}}}.",
        "i" = "For example {.code names = \"{{.col}}_{{.method}}\"}.",
        "i" = "To name one column, rename it in the selection: {.code lme = c(bili_lme = bili)}."
      ),
      call = call
    )
  }
  invisible(names)
}

#' Derived names must be unique within the call
#' @noRd
lm_check_unique_names <- function(new, call = rlang::caller_env()) {
  dup <- unique(new[duplicated(new)])
  if (length(dup) > 0L) {
    cli::cli_abort(
      c(
        "Each derived column needs a different name.",
        "x" = "Duplicated: {.field {dup}}.",
        "i" = "Use {.code names = \"{{.col}}_{{.method}}\"}, or rename in the selection, e.g. {.code lme = c(bili_lme = bili)}."
      ),
      call = call
    )
  }
  invisible(new)
}

#' Validate the LOCF lookback window
#'
#' NULL means no limit; otherwise one finite, non-negative number.
#' @noRd
lm_check_lookback <- function(x, has_locf, arg = "lookback",
                              call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!has_locf) {
    cli::cli_abort(
      "{.arg {arg}} only applies to {.arg locf} variables, but none were selected.",
      call = call
    )
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.code NULL} or a single non-negative, finite number.",
        "i" = "Use {.code lookback = NULL} for no limit."
      ),
      call = call
    )
  }
  as.double(x)
}

#' Validate `lme_args` and fill in defaults
#'
#' `lm_names` are the columns of the landmark data, where `adjust` variables
#' must live (they are baseline/landmark-level values, so they are known at
#' the prediction time).
#' @noRd
lm_check_lme_args <- function(x, has_lme, lm_names, arg = "lme_args",
                              call = rlang::caller_env()) {
  if (!is.list(x) || (length(x) > 0L && is.null(base::names(x))) ||
      any(base::names(x) == "")) {
    cli::cli_abort(
      "{.arg {arg}} must be a named list, e.g. {.code list(random_slope = TRUE)}.",
      call = call
    )
  }
  if (length(x) > 0L && !has_lme) {
    cli::cli_abort(
      "{.arg {arg}} only applies to {.arg lme} variables, but none were selected.",
      call = call
    )
  }

  defaults <- list(
    random_slope = FALSE,
    adjust       = NULL,
    REML         = TRUE,
    control      = NULL,
    min_subjects = 5L
  )
  unknown <- setdiff(base::names(x), base::names(defaults))
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has unknown element{?s} {.field {unknown}}.",
        "i" = "Allowed: {.field {names(defaults)}}."
      ),
      call = call
    )
  }
  opts <- utils::modifyList(defaults, x, keep.null = TRUE)

  lm_check_flag(opts$random_slope, paste0(arg, "$random_slope"), call)
  lm_check_flag(opts$REML, paste0(arg, "$REML"), call)

  ms <- opts$min_subjects
  if (!is.numeric(ms) || length(ms) != 1L || !is.finite(ms) || ms < 2 || ms != trunc(ms)) {
    cli::cli_abort(
      "{.arg {arg}$min_subjects} must be a single whole number of at least 2.",
      call = call
    )
  }
  opts$min_subjects <- as.integer(ms)

  if (is.null(opts$control)) {
    opts$control <- lme4::lmerControl()
  } else if (!inherits(opts$control, "lmerControl")) {
    cli::cli_abort(
      "{.arg {arg}$control} must be created with {.fn lme4::lmerControl}.",
      call = call
    )
  }

  adjust <- opts$adjust
  if (!is.null(adjust)) {
    if (!rlang::is_formula(adjust, lhs = FALSE)) {
      cli::cli_abort(
        c(
          "{.arg {arg}$adjust} must be a one-sided formula, e.g. {.code ~ age + sex}.",
          "i" = "Variables come from {.arg landmark_data}; {.code .t} is the time since the landmark."
        ),
        call = call
      )
    }
    vars <- setdiff(all.vars(adjust), ".t")
    forbidden <- intersect(vars, c(lm_reserved_cols, ".value"))
    if (length(forbidden) > 0L) {
      cli::cli_abort(
        c(
          "{.arg {arg}$adjust} can't use {.field {forbidden}}.",
          "i" = "Outcome and key columns would leak or carry no information."
        ),
        call = call
      )
    }
    missing_vars <- setdiff(vars, lm_names)
    if (length(missing_vars) > 0L) {
      cli::cli_abort(
        c(
          "{.arg {arg}$adjust} refers to column{?s} not in {.arg landmark_data}: {.field {missing_vars}}.",
          "i" = "Adjustment variables must be baseline columns of {.arg landmark_data} (or covariates added by an earlier call)."
        ),
        call = call
      )
    }
  }
  opts
}

#' LME needs a continuous numeric variable
#' @noRd
lm_check_lme_var <- function(x, var, call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg lme} variable {.field {var}} must be numeric, not {.cls {class(x)}}.",
        "i" = "Use {.arg locf} for categorical variables."
      ),
      call = call
    )
  }
  if (dplyr::n_distinct(x, na.rm = TRUE) == 2L) {
    cli::cli_abort(
      c(
        "{.arg lme} variable {.field {var}} looks binary (two distinct values).",
        "i" = "Gaussian mixed models are for continuous markers; use {.arg locf} instead."
      ),
      call = call
    )
  }
  invisible(x)
}

#' Refuse to overwrite existing columns of the landmark data
#' @noRd
lm_check_collisions <- function(new, existing, call = rlang::caller_env()) {
  clash <- intersect(new, existing)
  if (length(clash) > 0L) {
    cli::cli_abort(
      c(
        "Derived column name{?s} {.field {clash}} already exist{?s/} in {.arg landmark_data}.",
        "i" = "Existing columns are never overwritten. Use {.code names = \"{{.col}}_{{.method}}\"}, or rename in the selection (e.g. {.code lme = c(bili_lme = bili)})."
      ),
      call = call
    )
  }
  invisible(new)
}

#' Measurement times: numeric and finite (negative values allowed)
#' @noRd
lm_check_measure_time <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a numeric column, not {.cls {class(x)}}.",
      call = call
    )
  }
  n_bad <- sum(!is.finite(x))
  if (n_bad > 0L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must contain finite values.",
        "x" = "Found {n_bad} infinite value{?s}."
      ),
      call = call
    )
  }
  invisible(x)
}

#' Subject IDs must match in type; no implicit coercion
#' @noRd
lm_check_id_type <- function(lm_id, long_id, call = rlang::caller_env()) {
  if (!identical(class(lm_id), class(long_id)) || typeof(lm_id) != typeof(long_id)) {
    cli::cli_abort(
      c(
        "{.arg id} in {.arg longitudinal_data} must have the same type as the subject identifier in {.arg landmark_data}.",
        "x" = "{.arg landmark_data} has {.cls {class(lm_id)}} IDs; {.arg longitudinal_data} has {.cls {class(long_id)}} IDs.",
        "i" = "Convert one of them first, e.g. with {.fn as.integer} or {.fn as.character}."
      ),
      call = call
    )
  }
  invisible(long_id)
}

#' Different non-missing values for the same subject and time
#'
#' `meas` has columns .id, .obs_time and .value (missing values removed).
#' @noRd
lm_check_locf_conflicts <- function(meas, var, call = rlang::caller_env()) {
  conflicts <- meas |>
    dplyr::summarise(
      n_values = dplyr::n_distinct(.data$.value),
      .by = c(".id", ".obs_time")
    ) |>
    dplyr::filter(.data$n_values > 1L)

  if (nrow(conflicts) > 0L) {
    where <- paste0("id ", conflicts$.id, " at time ", conflicts$.obs_time)
    cli::cli_abort(
      c(
        "{.arg longitudinal_data} has conflicting values of {.field {var}} recorded at the same time.",
        "x" = "{nrow(conflicts)} subject-time pair{?s} affected: {where}.",
        "i" = "Resolve these first, e.g. by averaging or keeping one visit per time."
      ),
      class = "tidylandmark_error_conflict",
      call = call
    )
  }
  invisible(meas)
}
