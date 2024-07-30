#' @title SelectivityCurveGeneratorr Class
#'
#' @description
#' Wrapper class that call the millar SELECT method, two parameter logistic funtion and normal-off-logistic fixed
#' function to generate selection curves as requested
#' @export
SelectivityCurveGenerator <- R6::R6Class("SelectivityCurveGenerator", public = list( # nolint

  # @formatter:off
  #' @description
  #' Estimate the SELECT dome-shape curve that best fit the catch data
  #'
  #' @param catch_data dataframe containing all raw data about gear and catches
  #' @param catch_context R6 class holding information about catch_data columns and species' Linf
  #' @param haul_ids list of haul id vectors
  #' @param mesh_sizes name list of mesh sizes in ascending order
  #' @param rel_power name list of mesh relative fishing power in ascending order
  #' @param sel_model vector of models for which selectivity will be calculated
  #' @param num_trials number of trials required to find the most common mode for the SELECT estimates
  # @formatter:on
  select_millar = function(catch_data, catch_context, haul_ids, mesh_sizes, rel_power, sel_models, num_trials) {
    millar_generator <- FittestCommonDomeSelectivityFinder$new(
      catch_data,
      catch_context,
      haul_ids,
      mesh_sizes,
      rel_power,
      sel_models
    )
    return(millar_generator$run(num_trials))
  },
  # @formatter:off
  #' @description
  #' Generates a logistic ogive and caculate the first length that reaches maximum retention.
  #'
  #' @param lengths vector of lengths
  #' @param sl50 vector of selectivity at 50% values - it can containe one single value only
  #' @param sl95 vector of selectivity at 95% values - it can containe one single value only
  #' @returns a named list with the logistic ogive and its maximum retention length
  #' @export
  # @formatter:on
  two_params_logistic = function(lengths, sl50, sl95) {
    two_par_log_gen <- TwoParLogisticCurveGenerator$new(lengths, sl50, sl95)
    return(two_par_log_gen$run())
  },
  # @formatter:off
  #' @description
  #' Generates a normal distribution curve that best fits a given two-parameter logistic function.
  #'
  #' @param lengths vector of lengths
  #' @param sl50 vector of selectivity at 50% values - it can containe one single value only
  #' @param sl95 vector of selectivity at 95% values - it can containe one single value only
  #' @returns a named list with the dome ogive, mode (max retention) and spread
  #' @export
  # @formatter:on
  dome_approach_to_logistic = function(lengths, sl50, sl95, length_increment = 0.1) {
    logistic_curve <- self$two_params_logistic(lengths, sl50, ls95)
    normal_from_logit_builder <- NormalFromLogisticApproximator$new(
      sl50,
      sl95,
      logistic_curve$sfull,
      length_increment,
      lengths
    )
    ret <- normal_from_logit_builder$run()
    dome_from_log_gen <- DomeFromLogisticCurveGenerator$new(lengths, logistic_sfull, ret$spread)
    return(dome_from_log_gen$run())
  }
))
