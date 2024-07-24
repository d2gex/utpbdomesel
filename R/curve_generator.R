#' @title CurveGenerator Class
#'
#' @description
#' Base class that provides a common structure to the logistic and dome approach to logistic classes
CurveGenerator <- R6::R6Class("CurveGenerator", public = list( # nolint
  # @formatter:off
  #' @field lengths vector of length classes
  lengths = NULL,
  #' @description
  #' Initialise class CurveGenerator
  #'
  #' @param lengths vector of length classes
  #' @export
  # @formatter:on
  initialize = function(lengths) {
    self$lengths <- lengths
  },
  run = function() {
  },
  generate_curve_df = function() {
  }
))
