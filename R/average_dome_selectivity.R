AverageDomeSelectivity <- R6::R6Class("AverageDomeSelectivity", public = list( # nolint
  sel_detail_vector = NULL,
  initialize = function(sel_detail_vector) {
    self$sel_detail_vector <- sel_detail_vector
  },
  run = function() {
    return(private$calculate_params())
  }
), private = list(
  #' Calculate the average of all parameters by model. To do so binds all rows of all param dataframes
  #' and calculate the average of every column where the mode > 0
  calculate_params = function() {
    return(
      lapply(self$sel_detail_vector, function(sel_detail) { # build list of params dataframes
        sel_detail$params
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(model) %>%
        dplyr::filter(mode > 0) %>%
        dplyr::summarise(across(everything(), mean)) %>%
        dplyr::arrange(deviance)
    )
  }
))
