#' @title DNormalFromLogisticApproximator Class
#'
#' @description
#' Class that finds the best normal distribution fit given a two-tail logistic functions stuck together and its s100.
#' This piece of code is a version from https://stats.stackexchange.com/a/630701/355585
NormalFromLogisticApproximator <- R6::R6Class("NormalFromLogisticApproximator", public = list( # nolint
  # @formatter:off
  #' @field sl50 length at which fish could be caught with a 50% chance
  sl50 = NULL,
  #' @field sl95 length at which fish could be caught with a 90% chance
  sl95 = NULL,
  #' @field k1 mode parameter as described by the SELECT method
  k1 = NULL,
  #' @field length_increment increments along the x axis for the length classes
  length_increment = NULL,
  #' @field k2_scanned_interval interval from which values are taken to optimise k2
  k2_scanned_interval = NULL,
  #' @field length_classes vector with the length classes
  length_classes = NULL,
  #' @description
  #' Initialise class NormalFromLogisticApproximator
  #'
  #' @param sl50 length at which fish could be caught with a 50% chance
  #' @param sl95 length at which fish could be caught with a 90% chance
  #' @param k1 mode parameter as described by the SELECT method
  #' @param length_increment length of increments used in the k2 optimisation
  #' @param length_classes vector with the length classes
  #' @param k2_scanned_interval interval from which values are taken to optimise k2
  #' @export
  # @formatter:on
  initialize = function(sl50, sl95, k1, length_increment, length_classes, k2_scanned_interval = NULL) {
    self$sl50 <- sl50
    self$sl95 <- sl95
    self$k1 <- k1
    self$length_increment <- length_increment
    self$k2_scanned_interval <- k2_scanned_interval
    self$length_classes <- length_classes
  },
  # @formatter:off
  #' @description
  #' Find the fittest dome-shape curve that approximates another dome curve built from a two-parameter logistic functio
  #'
  #' @export
  # @formatter:on
  run = function() {
    k2_scanned_interval <- self$k2_scanned_interval
    if (is.null(k2_scanned_interval)) {
      k2_scanned_interval <- private$find_k2_search_range()
    }
    ret <- optimize(private$outer_minimization, k2_scanned_interval)
    return(list(
      spread = ret$minimum,
      optimised_func_value = ret$objective
    ))
  }
), private = list(
  normal = function(length_value, k2) {
    normal_loc <- NormalFixSpread$new(
      length_value,
      self$k1,
      k2,
      mesh_proportion = 1,
      rel_power = 1
    )
    return(normal_loc$run())
  },
  #' Build a symmetric curve formed by two tail logistic function. The right limb is just a mirror of the
  #' left-hand side up to the first length that is very close to 1, its maximum retention length.
  emulate_symmetric_dome_from_two_param_logistic = function(length_value) {
    length_limb_size <- self$k1 - min(self$length_classes)
    dist_to_k1 <- length_value - self$k1
    # left limb up to maximum?
    if (length_value <= self$k1) {
      sought_value <- length_value
      # In the symmetric right limb? Fid its counterpart in left limb
    } else if ((length_value > self$k1) & (length_value < self$k1 + length_limb_size)) {
      sought_value <- length_value - (2 * dist_to_k1)
    } else {
      sought_value <- 0
    }
    return(1 / (1 + exp(-log(19) * (sought_value - self$sl50) / (self$sl95 - self$sl50))))
  },
  #' Calculate the range where the most optimal k2 value should be found. It uses the standard deviation concept
  find_k2_search_range = function() {
    k2 <- (self$k1 - min(self$length_classes)) / 3
    interval <- c(0, 2 * k2)
    return(interval)
  },
  inner_maximization = function(length_value, k2) {
    abs(private$normal(length_value, k2) - private$emulate_symmetric_dome_from_two_param_logistic(length_value))
  },
  outer_minimization = function(k2) {
    scanned_length_interval <- seq(min(self$length_classes), max(self$length_classes), by = self$length_increment)
    best <- 0
    for (i in 2:length(scanned_length_interval)) {
      res <- optimize(private$inner_maximization,
        c(scanned_length_interval[i - 1], scanned_length_interval[i]),
        k2 = k2,
        maximum = TRUE
      )$objective
      if (res > best) {
        best <- res
      }
    }
    return(best)
  }
))
