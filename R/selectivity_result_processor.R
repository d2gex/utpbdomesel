SelectivityResultProcessor <- R6::R6Class("SelectivityResultProcessor", inherit = MixinUtilities, public = list( # nolint
  # @formatter:off
  #' @field data named list of SELECT's model results
  data = NULL,
  mesh_names = NULL,
  rel_power = NULL,
  mesh_sizes = NULL,
  rel_mesh_sizes = NULL,
  #' @description
  #' Initialise class DSelectivityResultProcessor
  #'
  #' @param data named list of SELECT's model results
  #' @export
  # @formatter:on
  initialize = function(data, rel_power, mesh_sizes) {
    self$data <- data
    self$mesh_names <- names(rel_power)
    self$rel_power <- unlist(unname(rel_power))
    self$mesh_sizes <- unlist(unname(mesh_sizes))
    self$rel_mesh_sizes <- self$mesh_sizes / min(self$mesh_sizes)
  },
  # @formatter:off
  #' @description
  #' Extract all relevant details of the estimated selectivity curves for all models and gears
  #'
  #' @returns a named list with
  #' a) the relevant input context given to the SELECT method
  #' b) a dataframe with all estimated parameters as presented by SELECT
  #' c) all selectivity ogives per model and gears, including the resulted gear sum
  #' d) al S100 per model and gears, including the resulted gear sum
  #' @export
  # @formatter:o
  extract_sel_details = function() {
    context <- self$extract_input_contexts()
    params_df <- private$extract_params_as_df()
    sel_ogives <- lapply(names(self$data), function(model) {
      private$extract_sel_curves(model)
    })
    names(sel_ogives) <- names(data)
    individual_gear_s100 <- private$calculate_individual_gear_s100(params_df)
    summed_gear_s100 <- private$calculate_summed_gear_s100(sel_ogives)
    s100 <- with(stack(c(individual_gear_s100, summed_gear_s100)), split(values, ind))

    return(list(
      input_context = context,
      params = params_df,
      sel_ogives = sel_ogives,
      s100 = s100
    ))
  },
  extract_input_contexts = function() {
    model <- head(names(self$data), 1)
    return(list(
      length_classes = self$data[[model]]$inputs$midLengths,
      mesh_sizes = setNames(as.list(self$mesh_sizes), self$mesh_names),
      rel_power = setNames(as.list(self$rel_power), self$mesh_names)
    ))
  }
), private = list(
  # @formatter:off
  #' @description
  #' Create a dataframe with the main variables of each SELECT's model result
  #'
  #' @returns a dataframe with a row per model's result
  #' @export
  # @formatter:on
  extract_params_as_df = function() {
    result_df <- NULL
    for (model_name in names(self$data)) {
      model_data <- data.frame(
        model = model_name,
        mode = self$data[[model_name]]$outputs$par[[1]],
        spread = self$data[[model_name]]$outputs$par[[2]],
        mode_conv = self$data[[model_name]]$outputs$estimates[1, "par"],
        spread_conv = self$data[[model_name]]$outputs$estimates[2, "par"],
        deviance = as.numeric(self$data[[model_name]]$outputs$out["Deviance", ]),
        dof = as.numeric(self$data[[model_name]]$outputs$out["d.o.f.", ]),
        mode_se = self$data[[model_name]]$outputs$estimates[1, "s.e."],
        spread_se = self$data[[model_name]]$outputs$estimates[2, "s.e."]
      )
      if (is.null(result_df)) {
        result_df <- self$create_empty_dataframe(names(model_data))
      }
      result_df <- rbind(result_df, model_data)
    }
    return(result_df %>% arrange(deviance))
  },
  # @formatter:off
  #' @description
  #' Create a dataframe with the main variables of each SELECT's model result
  #'
  #' @returns a dataframe with a row per model's result
  #' @export
  # @formatter:on
  extract_sel_curves = function(model) {
    mode <- ifelse(model == "lognorm",
      self$data[[model]]$outputs$estimates[1, "par"],
      self$data[[model]]$outputs$par[[1]]
    )
    spread <- self$data[[model]]$outputs$par[[2]]
    midpoints <- self$data[[model]]$outputs$midLengths
    model_class <- switch(model,
      "norm.loc" = {
        NormalFixSpread
      },
      "norm.sca" = {
        NormalVariableSpread
      },
      "lognorm" = {
        LogNormalVariableSpread
      }
    )
    sel_ogives <- lapply(seq_along(self$rel_power), function(offset) {
      sel_curve <- model_class$new(midpoints, mode, spread, self$rel_mesh_sizes[offset], self$rel_power[offset])
      sel_curve$run()
    })
    sum_offset <- length(sel_ogives) + 1
    sel_ogives[[sum_offset]] <- Reduce(`+`, sel_ogives)
    sel_ogives[[sum_offset]] <- sel_ogives[[sum_offset]] / max(sel_ogives[[sum_offset]])
    names(sel_ogives) <- c(self$mesh_names, "gear_sum")
    return(sel_ogives)
  },
  # @formatter:off
  #' @description
  #' Calculate S100 for all individual gears for every model
  #'
  #' @returns a named lists by gears
  #' @export
  # @formatter:on
  calculate_individual_gear_s100 = function(params_df) {
    gear_model_s100s <- list()
    for (model_name in names(self$data)) {
      if (model_name == "lognorm") {
        mode_col <- "mode_conv"
      } else {
        mode_col <- "mode"
      }
      mode_mesh1 <- as.numeric(params_df %>% dplyr::filter(model == model_name) %>% select_at(.vars = mode_col))
      modes <- mode_mesh1 * self$rel_mesh_sizes
      gear_model_s100s[[model_name]] <- modes
    }
    return(gear_model_s100s)
  },
  # @formatter:off
  #' @description
  #' Calculate the S100 for the curve resulting from summing all gear curves for every model
  #'
  #' @returns a named lists by gears
  #' @export
  # @formatter:on
  calculate_summed_gear_s100 = function(sel_ogive) {
    gear_model_s100s <- list()
    for (model_name in names(self$data)) {
      summed_sel_cruve <- sel_ogive[[model_name]][[length(sel_ogive)]]
      mode_offset <- which(summed_sel_cruve == 1)
      gear_model_s100s[[model_name]] <- self$data[[model_name]]$inputs$midLengths[mode_offset]
    }
    return(gear_model_s100s)
  }
))
