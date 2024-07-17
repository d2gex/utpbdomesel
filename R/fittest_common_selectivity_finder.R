FittestCommonDomeSelectivityFinder <- R6::R6Class("FittestCommonDomeSelectivityFinder", public = list( # nolint
  # @formatter:off
  #' @field catch_data dataframe containing all raw data about gear and catches
  catch_data = NULL,
  #' @field catch_context R6 class holding information about catch_data columns and species' Linf
  catch_context = NULL,
  #' @field haul_ids list of haul id vectors
  haul_ids = NULL,
  #' @field mesh_sizes name list of mesh sizes in ascending order
  mesh_sizes = NULL,
  #' @field rel_power name list of mesh relative fishing power in ascending order
  rel_power = NULL,
  #' @field sel_model vector of models for which selectivity will be calculated
  sel_models = NULL,
  # @formatter:on
  initialize = function(catch_data, catch_context, haul_ids, mesh_sizes, rel_power, sel_models) {
    self$catch_data <- catch_data
    self$catch_context <- catch_context
    self$haul_ids <- haul_ids
    self$mesh_sizes <- mesh_sizes
    self$rel_power <- rel_power
    self$sel_models <- sel_models
  },
  run = function(num_trials = 50) {
    sel_detail_list <- self$run_trials(num_trials)
    sel_details <- self$fetch_sel_details(sel_detail_list)
    return(sel_details)
  },
  run_trials = function(num_trials = 50) {
    # Calculate the curve many times
    sel_detail_list <- list()
    haul_ids <- sample(self$haul_ids, num_trials)
    for (offset in seq_along(haul_ids)) {
      haul_id_batch <- haul_ids[[offset]]
      haul_id_catch <- self$catch_data %>%
        dplyr::filter(Idlance %in% haul_id_batch)
      utpb_sel_estimator <- UtpbDomeSELECTEstimator$new(
        haul_id_catch,
        self$rel_power,
        self$mesh_sizes,
        self$catch_context,
        self$sel_models
      )
      estimated_curve <- utpb_sel_estimator$estimate_selectivity_by_models(plot = FALSE)
      if (is.null(estimated_curve)) {
        logger::log_error(paste("Unable to estimate any selectivity curve for all models in batch", offset))
      } else {
        sel_proc <- SelectivityResultProcessor$new(estimated_curve, self$rel_power, self$mesh_sizes)
        sel_details <- sel_proc$extract_sel_details()
        sel_detail_list[[length(sel_detail_list) + 1]] <- sel_details
      }
    }
    return(sel_detail_list)
  },
  # @formatter:off
  #' Find the fitest model and return all its selectivity details
  #'
  #' sel_detail_list a list holding the selectivity details of each trial
  # @formatter:on
  fetch_sel_details = function(sel_detail_list) {
    fittest_model <- private$find_fittest_model(sel_detail_list)
    chosen_sel_details <- private$find_selectivity_details(sel_detail_list, fittest_model)
    return(chosen_sel_details)
  }
), private = list(
  # @formatter:off
  #' Choose the trials per model that yield most common modes and within them it selects the fittest one by dividing
  #' the devianve over the degree of freedoms. In sum gets the fittest trial of each model
  #'
  #' data a dataframe holding all parameter details of a single model for all trials
  # @formatter:on
  find_fittest_trial_by_model = function(data) {
    model <- unique(data$model)
    if (model == "lognorm") {
      data <- data %>%
        dplyr::mutate(intervals = cut(mode_conv, floor(min(mode_conv)):ceiling(max(mode_conv))))
    } else {
      data <- data %>%
        dplyr::mutate(intervals = cut(mode, floor(min(mode)):ceiling(max(mode))))
    }
    data <- data %>%
      dplyr::group_by(intervals) %>%
      dplyr::mutate(count = n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(count == max(count)) %>%
      dplyr::mutate(good_fitness = deviance / dof) %>%
      dplyr::filter(good_fitness == min(good_fitness)) %>%
      dplyr::select(-c(intervals, count))

    return(data)
  },
  # @formatter:off
  #' Among the single fittest trial per model, it gets what model is actually the fittest one of all.
  #'
  #' sel_detail_list a list holding the selectivity details of each trial
  # @formatter:on
  find_fittest_model = function(sel_detail_list) {
    params <- purrr::map_df(lapply(seq_along(sel_detail_list), function(offset) {
      params_ <- sel_detail_list[[offset]]$params
      params_$sel_list_offset <- offset
      return(params_)
    }), tibble::as_tibble)
    params <- params %>% dplyr::filter(mode > 0)
    models <- unique(params$model)
    model_fit_list <- lapply(models, function(single_model) {
      data <- params %>% dplyr::filter(model == single_model)
      data <- private$find_fittest_trial_by_model(data)
    })
    params <- purrr::map_df(model_fit_list, tibble::as_tibble)
    params <- params %>%
      dplyr::filter(good_fitness == min(good_fitness)) %>%
      dplyr::select(-good_fitness)
    return(params)
  },
  # @formatter:off
  #' Find the whole selectivity details of the fittest model
  #'
  #' sel_detail_list a list holding the selectivity details of each trial
  #' fittest_params_df param dataframe of the fittest model
  # @formatter:on
  find_selectivity_details = function(sel_detail_list, fittest_params_df) {
    sought_offset <- unique(fittest_params_df$sel_list_offset)
    model <- unique(fittest_params_df$model)
    sel_details <- sel_detail_list[[sought_offset]]
    sel_details$chosen_model <- model
    return(sel_details)
  }
))
