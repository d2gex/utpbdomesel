SelectivityResultProcessor <- R6::R6Class("SelectivityResultProcessor", inherit = MixinUtilities, public = list( # nolint
  # @formatter:off
  #' @field data named list of SELECT's model results
  data = NULL,
  #' @description
  #' Initialise class DSelectivityResultProcessor
  #'
  #' @param data named list of SELECT's model results
  # @formatter:on
  initialize = function(data) {
    self$data <- data
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
        s100 = self$data[[model_name]]$outputs$par[[1]],
        sigma = self$data[[model_name]]$outputs$par[[2]],
        s100_conv = self$data[[model_name]]$outputs$estimates[1, "par"],
        sigma_conv = self$data[[model_name]]$outputs$estimates[2, "par"],
        deviance = as.numeric(self$data[[model_name]]$outputs$out["Deviance", ]),
        dof = as.numeric(self$data[[model_name]]$outputs$out["d.o.f.", ]),
        s100_se = self$data[[model_name]]$outputs$estimates[1, "s.e."],
        sigma_se = self$data[[model_name]]$outputs$estimates[2, "s.e."]
      )
      if (is.null(result_df)) {
        result_df <- self$create_empty_dataframe(names(model_data))
      }
      result_df <- rbind(result_df, model_data)
    }
    return(result_df %>% arrange(deviance))
  }
))
