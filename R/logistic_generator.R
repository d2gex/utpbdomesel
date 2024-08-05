#' @title TwoParLogisticCurveGenerator Class
#'
#' @description
#' Class that generates a logistic ogive from SL50 and SL95 parameters
TwoParLogisticCurveGenerator <- R6::R6Class("TwoParLogisticCurveGenerator", inherit = CurveGenerator, public = list( # nolint
  # @formatter:off
  #' @field sl50 length at which a logistic function reaches 50%
  sl50 = NULL,
  #' @field sl95 length at which a logistic function reaches 95%
  sl95 = NULL,
  #' @description
  #' Initialise class TwoParLogisticCurveGenerator
  #'
  #' @param lengths vector of length classes
  #' @param sl50 length at which a logistic function reaches 50%
  #' @param sl95 length at which a logistic function reaches 95%
  #' @export
  # @formatter:on
  initialize = function(lengths, sl50, sl95) {
    super$initialize(lengths)
    self$sl50 <- sl50
    self$sl95 <- sl95
  },
  run = function() {
    curve_df <- self$generate_curve_df()
    sfull_details <- private$get_sfull_details(curve_df)
    return(list(
      curve = curve_df,
      sfull = sfull_details$sfull,
      sfull_offset = sfull_details$sfull_offset
    ))
  },
  generate_curve_df = function(...) {
    return(
      data.frame(
        lengths = self$lengths,
        retention = (1 / (1 + exp(-log(19) * (self$lengths - self$sl50) / (self$sl95 - self$sl50))))
      )
    )
  }
), private = list(
  get_sfull_details = function(ogive) {
    return(list(
      sfull = ogive$lengths[which(round(ogive$retention, 2) == 1)[1]],
      sfull_offset = which(round(ogive$retention, 2) == 1)[1]
    ))
  }
))
