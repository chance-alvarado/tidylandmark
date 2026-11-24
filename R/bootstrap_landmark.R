#' Two-stage bootstrap procedure for
#'
#' @param baseline
#' @param pipeline
#' @param n_boot
#'
#' @returns
#' @export
#'
#' @examples
bootstrap_landmark <- function(
  baseline,
  pipeline,

  exponentiate = FALSE,
  conf.int = FALSE,
  conf.level = 0.95,

  n_boot = 200
  ){

  # Get fit in complete data
  fit_full <- baseline |>

    # Call user-defined pipeline on data
    pipeline() |>

    # Use tidy.landmark_fit to get model fits
    tidy(
      exponentiate = exponentiate,
      conf.int = conf.int,
      conf.level = conf.level
    ) |>

    # Select relevant variables
    dplyr::select(

      # Fit invariant measures
      .landmark,
      .model,
      .cause,
      term,

      # Full fit measures
      estimate,
      tidyr::any_of(c('conf.low', 'conf.high')),
      p.value,
      .fit_status,
      .message
    ) |>

    dplyr::rename_with(
      .cols = c(
        estimate,
        tidyr::any_of(c('conf.low', 'conf.high')),
        p.value,
        .fit_status,
        .message
      ),
      .f = \(col) paste0(col, '__full')
    )

  # Iterate and get bootstrap samples and fits
  fit_boot <- purrr::map(

    # Iterate over bootstrap indices
    1:n_boot,

    function(b){

      # From baseline dataset, pull IDs at each landmark and update baseline dataset
      id_sample <-

        # Sample available IDs at each landmark
        purrr::map(
          attributes(baseline)$landmark_config$id_list,
          \(ids){
            sample(
              x = ids,
              size = length(ids),
              replace = TRUE
            )
          }
        ) |>

        # Create tibble of sampled IDs at landmark
        tibble::tibble(
          .id = _,
          .landmark = attributes(baseline)$landmark_config$landmarks,
        ) |>
        tidyr::unnest(cols = .id) |>

        # Create new bootstrap ID (so  latter LME does not infer incorrect grouping)
        dplyr::mutate(.boot_id = 1:dplyr::n()) |>

        # Join to formatted landmark_tbl
        dplyr::left_join(
          baseline,
          by = c('.id', '.landmark'),
          relationship = 'many-to-one'
        ) |>

        # Reset landmark_tbl class
        new_landmark_tbl(
          config = attributes(baseline)$landmark_config
        )

      # Set boot attribute to detect in next function
      attributes(id_sample)$boot <- TRUE

      id_sample |>

        # Call user-defined pipeline on data
        pipeline() |>

        # Use tidy.landmark_fit to get model fits
        tidy(exponentiate = exponentiate) |>

        # Select relevant variables
        dplyr::select(
          .landmark,
          term,
          estimate,
          .fit_status,
          .message
        )

    } |>

      # Do not print messages to console for each bootstrap fit
      suppressMessages()

  ) |>

    # Join bootstrap fits and aggregate
    dplyr::bind_rows() |>

    dplyr::summarize(
      estimates__boot = list(estimate),
      .fit_status__boot = list(.fit_status),
      .message__boot = list(.message),
      .by = c(.landmark, term)
    )

  fit_boot

}
