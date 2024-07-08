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
  #' Create a dataframe with the main variables of each SELECT's model result
  #'
  #' @returns a dataframe with a row per model's result
  #' @export
  # @formatter:on
  extract_result_dataframe = function() {
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
  extract_sel_curves = function(model) {
    mode <- self$data[[model]]$outputs$par[[1]]
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
    sel_ogives <- lapply(seq_along(rel_power), function(offset) {
      sel_curve <- model_class$new(midpoints, mode, spread, self$rel_mesh_sizes[offset], self$rel_power[offset])
      sel_curve$run()
    })
    sum_offset <- length(sel_ogives) + 1
    sel_ogives[[sum_offset]] <- Reduce(`+`, sel_ogives)
    sel_ogives[[sum_offset]] <- sel_ogives[[sum_offset]] / max(sel_ogives[[sum_offset]])
    names(sel_ogives) <- c(self$mesh_names, "gear_sum")
    return(sel_ogives)
  }
))
