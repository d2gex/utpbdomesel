AverageDomeSelectivity <- R6::R6Class("AverageDomeSelectivity", public = list( # nolint
  sel_detail_vector = NULL,
  initialize = function(sel_detail_vector) {
    self$sel_detail_vector <- sel_detail_vector
  },
  run = function() {
    # return(private$calculate_params())
    return(private$calculate_ogive())
  }
), private = list(
  # @formatter:off
  #' Build a wide dataframe containing all ogives for all model and gears
  # @formatter:on
  build_ogive_df = function(sel_ogive_list) {
    ogive_length <- length(sel_ogive_list[[1]][[1]]) # All ogives for all model have same length. Get  then first
    self_ogive_df <- purrr::map_df(sel_ogive_list, tibble::as_tibble) # build a df with ogives
    self_ogive_df$model <- unlist(lapply(names(sel_ogive_list), function(x) {
      rep(x, ogive_length)
    })) # Add model
    self_ogive_df$id <- rep(1:ogive_length, length(sel_ogive_list)) # Add unique ind
    self_ogive_df <- self_ogive_df %>% dplyr::select(-gear_sum)
    return(self_ogive_df)
  },
  # @formatter:off
  #' Calculate the average of all parameters by model. To do so binds all rows of all param dataframes
  #' and calculate the average of every column where the mode > 0
  # @formatter:on
  calculate_params = function() {
    return(
      lapply(self$sel_detail_vector, function(sel_detail) { # build list of params dataframes
        sel_detail$params
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(model) %>%
        dplyr::filter(mode > 0) %>%
        dplyr::summarise(across(everything(), mean)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(deviance)
    )
  },
  # @formatter:off
  #' Calculate the average of all ogives per model and gear. Then it sumes each gear for each
  #' model into a column called gear_sum
  # @formatter:on
  calculate_ogive = function() {
    return(
      lapply(self$sel_detail_vector, function(sel_detail) { # build list of params dataframes
        private$build_ogive_df(sel_detail$sel_ogives)
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(model, id) %>%
        dplyr::summarise(across(everything(), mean)) %>%
        dplyr::ungroup() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(gear_sum = sum(dplyr::c_across(-c("id", "model")), na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(model) %>%
        dplyr::mutate(gear_sum = gear_sum / max(gear_sum)) %>%
        dplyr::ungroup()
    )
  }
))
