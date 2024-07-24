#' @title SelectivityCurveGeneratorr Class
#'
#' @description
#' Wrapper class that call the millar SELECT method, two parameter logistic funtion and normal fixed function
#' to generate selection curves as requested
#' @export
SelectivityCurveGenerator <- R6::R6Class("SelectivityCurveGenerator", public = list( # nolint
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
  two_params_logistic = function(lengths, sl50, sl95) {
    two_par_log_gen <- TwoParLogisticCurveGenerator$new(lengths, sl50, sl95)
    return(two_par_log_gen$run())
  },
  dome_approach_to_logistic = function(lengths, logistic_sfull) {
    dome_from_log_gen <- DomeFromLogisticCurveGenerator$new(lengths, logistic_sfull)
    return(dome_from_log_gen$run())
  }
))
