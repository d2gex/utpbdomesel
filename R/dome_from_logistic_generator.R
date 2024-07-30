#' @title DomeFromLogisticCurveGenerator Class
#'
#' @description
#' Class that creates a dome-shape curve ogive given the mode and spread
DomeFromLogisticCurveGenerator <- R6::R6Class("DomeFromLogisticCurveGenerator", inherit = CurveGenerator, public = list( # nolint
  # @formatter:off
  #' @field k1 maximum retention length
  k1 = NULL,
  #' @field k2 curve spread
  k2 = NULL,
  #' @description
  #' Initialise class DomeFromLogisticCurveGenerator
  #'
  #' @param lengths vector of length classes
  #' @param k1 maximum retention length
  #' @param k2 curve spread
  #' @export
  # @formatter:on
  initialize = function(lengths, k1, k2) {
    super$initialize(lengths)
    self$k1 <- k1
    self$k2 <- k2
  },
  run = function() {
    curve_df <- self$generate_curve_df()
    return(list(
      curve = curve_df,
      mode = self$k1,
      sigma = self$k2
    ))
  },
  generate_curve_df = function() {
    normal_loc <- NormalFixSpread$new(
      self$lengths,
      self$k1,
      self$k2,
      mesh_proportion = 1,
      rel_power = 1
    )
    ogive <- normal_loc$run()
    ogive <- ogive[seq_along(self$lengths)]
    return(data.frame(
      lengths = self$lengths,
      retention = ogive
    ))
  }
))
