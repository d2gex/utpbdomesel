#' @title QuasiDomeFromLogisticGenerator Class
#'
#' @description
#' Class that builds a quase-dome shape where the left limb is built by using a two-parameter logistic
#' formula and for the right one, the first maximun retention length class from the left limb logistic funciton
#' and the spread of a symmetric dome-sahpe. Such as spread is from a dome curve that was approximated using
#' the same logistic function as here.
QuasiDomeFromLogisticGenerator <- R6::R6Class("QuasiDomeFromLogisticGenerator", inherit = CurveGenerator, public = list( # nolint

  # @formatter:off
  #' @field lengths vector of length classes
  lengths = NULL,
  #' @field sl50 length at which fish could be caught with a 50% chance
  sl50 = NULL,
  #' @field sl95 length at which fish could be caught with a 90% chance
  sl95 = NULL,
  #' @field k2 right-limb curve spread
  k2 = NULL,
  #' @description
  #' Initialise class QuasiDomeFromLogisticGenerator
  #'
  #' @param lengths vector with the length classes
  #' @param k2 right-limb curve spread
  #' @param sl50 length at which fish could be caught with a 50% chance
  #' @param sl95 length at which fish could be caught with a 90% chance
  #' @export
  # @formatter:on
  initialize = function(lengths, sl50, sl95, k2) {
    super$initialize(lengths)
    self$sl50 <- sl50
    self$sl95 <- sl95
    self$k2 <- k2
  },
  run = function() {
    logistic_curve <- private$generate_logistic_curve_details()
    curve_df <- self$generate_curve_df(curve_details = logistic_curve)
    return(list(
      curve = curve_df,
      sfull = logistic_curve$sfull,
      sfull_offset = logistic_curve$sfull_offset
    ))
  },
  generate_curve_df = function(...) {
    args <- list(...)
    logistic_first_half <- args$curve_details$curve$retention[1:args$curve_details$sfull_offset]
    dome_second_half <- private$generate_second_half_dome(args$curve_details$sfull, args$curve_details$sfull_offset)
    return(data.frame(
      lengths = self$lengths,
      retention = c(logistic_first_half, dome_second_half)
    ))
  }
), private = list(
  generate_logistic_curve_details = function() {
    two_param_log <- TwoParLogisticCurveGenerator$new(self$lengths, self$sl50, self$sl95)
    return(two_param_log$run())
  },
  generate_second_half_dome = function(k1, k1_offset) {
    length_classes <- self$lengths[(k1_offset + 1):length(self$lengths)]
    norm_generator <- NormalFixSpread$new(length_classes, k1, self$k2, 1, 1)
    return(norm_generator$run())
  }
))
