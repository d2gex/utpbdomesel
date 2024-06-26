GearFishingPowerCalculator <- R6::R6Class("GearFishingPowerCalculator", public = list( # nolint

  # @formatter:off
  #' @description
  #' Initialise the class.
  #' @param catch_wide a dataframe with the catch data in wide format - timestep as columns
  #' @param gears a dataframe holding the information area ration between gears
  #'
  #' @export
  # @formatter:on
  all_gear_soaktime = NULL,
  gear_area = NULL,
  initialize = function(all_gear_soaktime, gear_area) {
    self$all_gear_soaktime <- all_gear_soaktime
    self$gear_area <- gear_area
  },
  # @formatter:off
  #' @description
  #' Calculate the fishing power of each gear through a number of trials. At each trial, all rows of each gear are
  #' randomly shuffled, their soaktime levelled off and ratio areas calculated (number of panes of each haull * panel
  #' ratio). The final ratio area is the average over all trials. In addition, a sequence of haul_ids involved in one
  #' of the trial is returned at random.
  #'
  #' @returns a named list containing:
  #'    a) one of the haul_id sequences returned in a randomly selected trial
  #'    b) the gear's average area ratio through all trials
  #' @export
  # @formatter:on
  calculate_gear_fishing_power_by_trials = function(num_trials = 50) {
    trial_haul_ids <- list()
    trial_area_ratios <- list()
    for (i in 1:num_trials) {
      gear_power_details <- self$calculate_gear_fishing_power()
      trial_haul_ids[[i]] <- gear_power_details$used_haul_ids
      trial_area_ratios[[i]] <- gear_power_details$gear_area_ratios
    }
    # Get randomly one of the sequence of hauls involved in the calculation of the fishing power in one single trial
    haul_ids <- trial_haul_ids[[sample(seq_along(trial_haul_ids), 1)]]
    mean_gear_area_ratios <- as.list(apply(purrr::map_df(trial_area_ratios, tibble::as_tibble), 2, mean))
    relative_fishing_power <- private$get_relative_fishing_power(mean_gear_area_ratios)
    return(list(
      all_haul_ids = trial_haul_ids,
      sample_haul_ids = haul_ids,
      mean_gear_area_ratios = mean_gear_area_ratios,
      relative_fishing_power = relative_fishing_power
    ))
  },
  # @formatter:off
  #' @description
  #' Calculate the fishing power of each gear and also return the haul ids involved in the calculation
  #'
  #' @returns a named list containing:
  #'    a) the area ratios of each gear
  #'    b) the haul ids involved in the calculation of the fishing power
  #' @export
  # @formatter:on
  calculate_gear_fishing_power = function() {
    gear_names <- self$gear_area$gear
    gear_details <- lapply(gear_names, function(x) {
      private$calculate_cum_soaktime_of_random_hauls(x)
    })
    names(gear_details) <- gear_names
    gear_offset <- private$get_offset_least_used_gear(gear_details)
    max_available_soaktime <- tail((gear_details[[gear_offset]])$soaktime_sum, 1)
    gear_details <- private$cut_gears_soaktime_by_threshold(gear_details, max_available_soaktime)
    gear_area_ratios <- private$calculate_gear_area_ratios(gear_details)
    used_haul_ids <- private$get_involved_haul_ids(gear_details)
    return(list(
      gear_area_ratios = gear_area_ratios,
      used_haul_ids = used_haul_ids
    ))
  }
), private = list(
  # @formatter:off
  #' @description
  #' Calculate the cumulative soaktime for a given gear by first randomising the rows of the haul dataframe. This is
  #' required for each trial and due to the fact that the largest gear dataframe has got more hauls than the shortest
  #' one.
  #'
  #' @param gear_name name of the gear
  #' @returns dataframe
  # @formatter:on
  calculate_cum_soaktime_of_random_hauls = function(gear_name) {
    gear_soaktime <- self$all_gear_soaktime %>%
      dplyr::filter(gear == gear_name)

    # randomise rows
    gear_soaktime <- gear_soaktime[sample(nrow(gear_soaktime)), ]

    gear_soaktime <- gear_soaktime %>%
      dplyr::mutate(id = seq_along(nrow(gear_soaktime))) %>%
      dplyr::mutate(soaktime_sum = cumsum(soak_time))
    return(gear_soaktime)
  },
  # @formatter:off
  #' @description
  #' Get the offset of the gear that provides the maximum soaktime. This is the gear least employed - with the
  #' smallest effort.
  #'
  #' @param gears array of gears for which cummulative soaktime has already been calculated
  #' @returns a single list with name of gear and max soaktime
  # @formatter:on
  get_offset_least_used_gear = function(gears) {
    return(which.min(lapply(gears, function(x) {
      max(x$soaktime_sum)
    })))
  },
  # @formatter:off
  #' @description
  #' Level off every gear's total soaktime to that of the gear that is least used to equalise fishing effort
  #'
  #' @param gears array of gears for which cummulative soaktime has already been calculated
  #' @param max_available_soaktime maximum available soaktime across all gears
  #' @returns a named list containing the levelled-off gear dataframes
  # @formatter:on
  cut_gears_soaktime_by_threshold = function(gears, max_available_soaktime) {
    return(
      lapply(gears, function(x) {
        x <- x %>% dplyr::filter(soaktime_sum <= max_available_soaktime)
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
  #' @param gears array of gears for which cummulative soaktime has been levelled off
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
