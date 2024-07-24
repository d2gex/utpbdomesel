#' @title DomeFromLogisticCurveGenerator Class
#'
#' @description
#' Class that creates a dome-shape curve off a logistic one
DomeFromLogisticCurveGenerator <- R6::R6Class("DomeFromLogisticCurveGenerator", inherit = CurveGenerator, public = list( # nolint
  # @formatter:off
  #' @field logistic_sfull length at which a logistic function approaches 1 very rapidly
  logistic_sfull = NULL,
  #' @field dome_spread sigma value of the new calculated dome-shape curve with max retention at logistic_sfull
  dome_spread = NULL,
  #' @description
  #' Initialise class DomeFromLogisticCurveGenerator
  #'
  #' @param lengths vector of length classes
  #' @param logistic_sfull length at which a logistic function approaches 1 very rapidly
  #' @export
  # @formatter:on
  initialize = function(lengths, logistic_sfull) {
    super$initialize(lengths)
    self$logistic_sfull <- logistic_sfull
    self$dome_spread <- private$calculate_sigma()
  },
  run = function() {
    curve_df <- self$generate_curve_df()
    return(list(
      curve = curve_df,
      mode = self$logistic_sfull,
      sigma = self$dome_spread
    ))
  },
  generate_curve_df = function() {
    k1 <- self$logistic_sfull
    k2 <- self$dome_spread
    normal_loc <- NormalFixSpread$new(
      self$lengths,
      k1,
      k2,
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
), private = list(
  calculate_sigma = function() {
    return(abs(self$logistic_sfull - min(self$lengths)) / 3)
  }
))
