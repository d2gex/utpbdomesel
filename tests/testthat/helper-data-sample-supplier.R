data_sample <- readRDS(test_path("test_data", "spp_select_data.rds"))

#' Provides the retention probabilities for lengths 7 and 20 calculated by a particular
#' SELECT model for an unknown species
#'
#' @returns model context to be tested
get_normal_loc_test_data <- function(sample, model) {
  return(list(
    k1 = sample[[model]]$outputs$par[1],
    k2 = sample[[model]]$outputs$par[2],
    midpoints = sample[[model]]$outputs$midLengths,
    # Expected output probability for length 7 and 20 for mesh 1
    m1 = list(
      selectivity_curve = sample[[model]]$outputs$selection_ogive_mat[, 2],
      rel_power = 1,
      proportion = 1
    ),
    # Expected output probability for length 7 and 20 for mesh 2
    m2 = list(
      selectivity_curve = sample[[model]]$outputs$selection_ogive_mat[, 3],
      rel_power = 0.08,
      proportion = 9 / 6
    )
  ))
}
