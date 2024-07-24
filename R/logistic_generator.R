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
    self$sl50 <- ifelse(length(sl50) > 1, mean(sl50, na.rm = TRUE), sl50)
    self$sl95 <- ifelse(length(sl95) > 1, mean(sl95, na.rm = TRUE), sl95)
  },
  run = function() {
    ogive <- self$calculate_ogive()
    sfull <- private$calculate_sfull(ogive)
    return(list(
      logistic_ogive = ogive,
      sfull = sfull,
    ))
  },
  generate_curve_df = function() {
    return(
      data.frame(
        lengths = self$lengths,
        retention = (1 / (1 + exp(-log(19) * (self$lengths - self$sl50) / (self$sl95 - self$sl50))))
      )
    )
  }
), private = list(
  calculate_sfull = function(ogive) {
    return(
      ogive$lengths[which(round(ogive$logistic, 2) == 1)[1]]
    )
  }
))
