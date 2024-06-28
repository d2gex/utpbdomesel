FishingEffortCalculator <- R6::R6Class("FishingEffortCalculator", public = list( # nolint

  # @formatter:off
  #' @description
  #' Initialise the class.
  #' @param gear_hauls_data a dataframe with the hauls, soaktime and number of panels per haul for all gears involved
  #' @param gear_panel_area_ratio named list of area gear area ratios, with the one having the largest panel set to 1
  #'
  #' @export
  # @formatter:on
  gear_hauls_data = NULL,
  gear_panel_area_ratio = NULL,
  initialize = function(gear_hauls_data, gear_panel_area_ratio) {
    self$gear_hauls_data <- gear_hauls_data
    self$gear_panel_area_ratio <- gear_panel_area_ratio
  },
  # @formatter:off
  #' @description
  #' Get the hauls involveld across all gears that provide the same fishing effort
  #'
  #' @returns a named list containing:
  #'    b) the haul ids involved in the calculation of the fishing power
  #' @export
  # @formatter:on
  get_gear_same_fishing_effort_haul_ids = function() {
    gear_names <- names(self$gear_panel_area_ratio)
    gear_details <- lapply(gear_names, function(x) {
      private$calculate_cum_fishing_effort_random_hauls(x)
    })
    names(gear_details) <- gear_names
    gear_offset <- private$get_offset_least_used_gear(gear_details)
    max_avail_fishing_effort <- tail((gear_details[[gear_offset]])$cum_fishing_effort, 1)
    gear_details <- private$cut_gears_fishing_effort_by_threshold(gear_details, max_avail_fishing_effort)
    # gear_area_ratios <- private$calculate_gear_area_ratios(gear_details)
    used_haul_ids <- private$get_involved_haul_ids(gear_details)
    return(list(
      # gear_area_ratios = gear_area_ratios,
      used_haul_ids = used_haul_ids
    ))
  }
), private = list(
  # @formatter:off
  #' @description
  #' Calculate both the individual fishing effort associated to a single haul and the progressive cumulative sum of
  #' each gear
  #'
  #' @param gear_name name of the gear
  #' @returns dataframe
  # @formatter:on
  calculate_cum_fishing_effort_random_hauls = function(gear_name) {
    gear_fishing_effort <- self$gear_hauls_data %>%
      dplyr::filter(gear == gear_name)

    # randomise rows
    gear_fishing_effort <- gear_fishing_effort[sample(nrow(gear_fishing_effort)), ]

    gear_fishing_effort <- gear_fishing_effort %>%
      dplyr::mutate(id = seq_along(nrow(gear_fishing_effort))) %>%
      dplyr::mutate(soaktime_hours = round(soak_time / 60, 2)) %>%
      dplyr::mutate(area = panels * self$gear_panel_area_ratio[[gear_name]]) %>%
      dplyr::mutate(fishing_effort = soaktime_hours * area) %>%
      dplyr::mutate(cum_fishing_effort = cumsum(fishing_effort))
    return(gear_fishing_effort)
  },
  # @formatter:off
  #' @description
  #' Get the offset of the gear that provides the least fishing effort
  #'
  #' @param gear_fishing_effort array of gears for which cummulative fishing effort has been calculated
  #' @returns offset of the gear with least fishing effort
  # @formatter:on
  get_offset_least_used_gear = function(gear_fishing_effort) {
    return(which.min(lapply(gear_fishing_effort, function(x) {
      max(x$cum_fishing_effort)
    })))
  },
  # @formatter:off
  #' @description
  #' Level off every gear's total fishing effort to that of the gear that is least used to equalise fishing efforts all
  #' across
  #'
  #' @param gears array of gears for which cummulative fishing effort has already been calculated
  #' @param max_fishing_effort maximum fishing effort available across all gears
  #' @returns a named list containing the levelled-off gear dataframes
  # @formatter:on
  cut_gears_fishing_effort_by_threshold = function(gears, max_fishing_effort) {
    return(
      lapply(gears, function(x) {
        x <- x %>% dplyr::filter(cum_fishing_effort <= max_fishing_effort)
      })
    )
  },
  # @formatter:off
  #' @description
  #' Calculate the area ratio of each gear based on the total number of panels and the average panel's area ratio
  #'
  #' @param gears array of gears for which cummulative soaktime has been levelled off
  #' @returns a named list of the area ratios
  # @formatter:on
  calculate_gear_area_ratios = function(gears) {
    gear_ratios <- list()
    for (gear_name in names(gears)) {
      num_panels <- sum(gears[[gear_name]]$panels)
      panel_ratio <- as.numeric(self$gear_area[self$gear_area["gear"] == gear_name, "area_ratio"])
      gear_ratios[[gear_name]] <- num_panels * panel_ratio
    }
    return(gear_ratios)
  },
  # @formatter:off
  #' @description
  #' Get the unique haul ids of each row for all gears involved in the calculation of the fishing power
  #'
  #' @param gears array of gears for which cummulative fishing effort has been levelled off
  #' @returns a vector with haul_ids
  # @formatter:on
  get_involved_haul_ids = function(gears) {
    return(
      unique(
        unlist(
          unname(
            lapply(gears, function(x) {
              x$haul_id
            })
          )
        )
      )
    )
  },
  # @formatter:off
  #' @description
  #' Given the area ratios of each gear it calculate their relative fishing power
  #'
  #' @param area_ratio named list with the area ratios of each gear
  #' @returns named list of relative fishing power
  # @formatter:on
  get_relative_fishing_power = function(area_ratio) {
    max_ratio <- max(unlist(area_ratio))
    return(
      lapply(area_ratio, function(x) {
        round(x / max_ratio, 2)
      })
    )
  }
))
