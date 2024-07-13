#' @title DomeSelectivityEstimator Class
#'
#' @description
#' Class that estimate some shape selectivity for a list of gears using SELECT
UtpbDomeSELECTEstimator <- R6::R6Class("UtpbDomeSELECTEstimator", inherit = MixinUtilities, public = list( # nolint
  # @formatter:off
  #' @field length_fq long dataframe with lengths and gears
  length_fq = NULL,
  #' @field fishing_power named list with the relative fishing power of each gear
  fishing_power = NULL,
  #' @field mesh_sizes named list with every mesh size sorted by asceding order
  mesh_sizes = NULL,
  #' @field catch_context R6 object with the context of the catch dataframe
  catch_context = NULL,
  #' @field models vector of model names to be estimated by SELECT
  models = NULL,
  #' @description
  #' Initialise class DomeSelectivityEstimator
  #' @param length_fq long dataframe with lengths and gears
  #' @param fishing_power named list with the relative fishing power of each gear
  #' @param mesh_sizes named list with every mesh size
  #' @param catch_context catch_context R6 object with the context of the catch dataframe
  #' @param models to be estimated as part of the selectivity indirect methods
  #' @param length_fq long dataframe with lengths and gears
  # @formatter:on
  initialize = function(length_fq, fishing_power, mesh_sizes, catch_context, models) {
    self$length_fq <- length_fq
    self$mesh_sizes <- mesh_sizes[order(unlist(mesh_sizes))] # ensure it is ordered from small to large meshes
    self$fishing_power <- unlist(unname(fishing_power[order(names(self$mesh_sizes))])) # same names order as meshes
    self$catch_context <- catch_context
    self$models <- models
  },
  # @formatter:off
  #' @description
  #' Estimate the selectivity curve for a set of gears and for a specific model
  #'
  #' @param min_padding padding that is substracted to the minimum length class
  #' @param up_to_linf boolean flag indicating whether the length classed should be capped
  #' @param plot boolean flag indicating whether the estimated curves and their deviations should be plot or not
  #' @returns a named list with SELECT object result
  #' @export
  # @formatter:on
  estimate_selectivity_by_models = function(min_padding = 0, up_to_linf = TRUE, plot = FALSE) {
    results <- list()
    mesh_sizes <- unlist(unname(self$mesh_sizes))
    for (model in self$models) {
      gears_long_catch_data <- private$build_gear_catch_at_length_long_df(min_padding, up_to_linf)
      gear_select_catch_data <- private$build_select_method_catch_data(gears_long_catch_data)
      tryCatch(
        {
          results[[model]] <- self$estimate_selectivity_curve(model,
            self$fishing_power,
            gear_select_catch_data$gear_catch_matrix,
            mesh_sizes,
            gear_select_catch_data$midpoints,
            plot = plot
          )
        },
        error = function(ex) {
          logger::log_error(paste("unable to provide a result for model", model, ex))
        }
      )
    }
    return(results)
  },
  # @formatter:off
  #' @description
  #' Estimate the selectivity curve for a set of gears and for a specific model
  #'
  #' @param model model used to estimate the curve
  #' @param fishing_power a vectorwith the relative fishing power of gears involved in ascendng order by mesh size
  #' @param gear_catch matrix of catches by gears (same number of rows as length classes)
  #' @param mesh_size a vector with the mesh size in ascending order by mesh size
  #' @param midpoints a vector with all length classes
  #' @param plot boolean flag indicating whether the estimated curve and its deviations should be plot or not
  #' @returns the SELECT model's inputs and object results
  #' @export
  # @formatter:on
  estimate_selectivity_curve = function(model, fishing_power, gear_catch, mesh_size, midpoints, plot = FALSE) {
    select_param <- list(
      midLengths = midpoints,
      meshSizes = mesh_size,
      CatchPerNet_mat = gear_catch
    )
    result <- TropFishR::select_Millar(
      param = select_param,
      rtype = model,
      rel.power = fishing_power,
      x0 = NULL,
      plot = plot
    )
    return(list(
      inputs = select_param,
      outputs = result
    ))
  }
), private = list(
  # @formatter:off
  #' Build a long dataframe with catch at length per gear
  #'
  #' Return a named list of dataframes with the length classes and catches (both per year)
  # @formatter:on
  build_gear_catch_at_length_long_df = function(min_padding = 0, up_to_linf = TRUE) {
    gears_long_catch <- lapply(names(self$mesh_sizes), function(gear_name) {
      gear_catch_data <- self$length_fq %>%
        dplyr::filter(arte_grupo == gear_name)
      if (nrow(gear_catch_data) == 0) {
        stop(paste("Unable to find catch data for gear", gear_name))
      }
      utpb_data_compositor <- utpbwlfq::UtpbDataComposition$new(gear_catch_data, self$catch_context)
      gear_catch_composition <- utpb_data_compositor$build_catch_at_length(self$catch_context$bindwidth,
        self$catch_context$col_prefix,
        min_padding,
        up_to_linf = up_to_linf
      )
      return(gear_catch_composition$catch_long_t)
    })
    names(gears_long_catch) <- names(self$mesh_sizes)
    return(gears_long_catch)
  },
  # @formatter:off
  #' Build the matrix of caches by gear and length classes that SELECT is expecting as well as the vector of
  #' midpoints (length classes)
  #'
  #' gear_data a named list with long dataframes representing yearly length-class catches
  #' Return a named list with the matrix that keeps the catch per length class(rows) and mesh (columns) and its
  #' midpoints
  #' @export
  # @formatter:on
  build_select_method_catch_data = function(gear_data) {
    gears_total_catch <- list()
    new_total_columns <- list()
    for (gear_name in names(gear_data)) {
      total_catch_column <- paste0("total_catch_", gear_name)
      gears_total_catch[[gear_name]] <- gear_data[[gear_name]] %>%
        dplyr::group_by(MeanLength) %>%
        dplyr::summarise(!!total_catch_column := sum(catch))
      new_total_columns[[total_catch_column]] <- 0
    }
    total_catch_df <- gears_total_catch %>%
      purrr::reduce(merge, by = "MeanLength", all = TRUE) %>%
      tidyr::replace_na(new_total_columns)

    return(list(
      gear_catch_matrix = self$df_to_unname_matrix(total_catch_df, from_col = 2),
      midpoints = total_catch_df$MeanLength
    ))
  }
))
