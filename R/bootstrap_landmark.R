#' Title
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
  n_boot = 10
  ){

  map(

    # Iterate over bootstrap indices
    1:n_boot,

    function(b){

      # From baseline dataset, pull IDs at each landmark and update baseline dataset
      id_sample <-

        # Sample available IDs at each landmark
        purrr:::map(
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
        tibble(
          .id = _,
          .landmark = attributes(baseline)$landmark_config$landmarks,
        ) |>
        tidyr::unnest(cols = c(.id)) |>

        # Create new bootstrap ID (so  latter LME does not infer incorrect grouping)
        dplyr::mutate(.boot_id = 1:n()) |>

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

        # Select restricted columns of relevance
        dplyr::select(
          .landmark, term, estimate
        )

    }
  )
}

bootstrap_landmark(

  baseline = pbc_lm_baseline,

  pipeline = \(data) data |>

    add_landmark_covariates(
      pbcseq_visits, id, day,
      locf = albumin, lme = bili, lme_args = list(adjust = ~ age)
    ) |>

    fit_landmark(~ age + log(bili) + albumin, cause = 2)

)


