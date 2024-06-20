GearAreaRatioCalculator <- R6::R6Class("GearAreaRatioCalculator", public = list(

  # @formatter:off
  #' @description
  #' Initialise the class.
  #' @param catch_wide a dataframe with the catch data in wide format - timestep as columns
  #' @param gears a dataframe holding the information area ration between gears
  #'
  #' @export
  # @formatter:on
  gear_soaktime = NULL,
  gear_area = NULL,
  num_repetions = NULL,
  initialize = function(gear_soaktime, gear_area, num_repetions = 1024) {
    self$gear_soaktime <- gear_soaktime
    self$gear_area <- gear_area
    self$num_repetions <- num_repetions
  },
  calculate_area_ratio = function() {
    gear_names <- self$gear_area$gear
    for (i in 1:self$num_repetions) {

    }

  }
), private = list(
  fetch_separate_gear_dataframes = function() {
    gear_names <- self$gear_area$gear
    gear_details <- list()
    for (g_name in gear_names) {
      gear_details[[g_name]] <- private$calculate_cumulative_soaktime(g_name)
    }
    return (gear_details)
  },
  # @formatter:off
  #'@description
  #' Calculate the cumulative soaktime for a given gear by first randomising the rows of the dataframe
  #'
  #' @param gear_name name of the gear
  #' @returns dataframe
  #' @export
  # @formatter:on
  calculate_cumulative_soaktime = function(gear_name) {
    single_gear_soaktime <- self$gear_soaktime %>%
      dplyr::filter(gear == gear_name)

    # ransomise rows
    single_gear_soaktime <- single_gear_soaktime[sample(nrow(gear_data)),]

    single_gear_soaktime <- single_gear_soaktime %>%
      dplyr::mutate(id = 1:nrow(single_gear_soaktime)) %>%
      dplyr::mutate(soaktime_sum = cumsum(soak_time))
    return(single_gear_soaktime)
  },
  # @formatter:off
  #'@description
  #' get the maximum soaktime of the gear that is least employed
  #'
  #' @param gears array of gears for which cummulative soaktime has already been calculated
  #' @returns a single list with name of gear and max soaktime
  #' @export
  # @formatter:on
  get_max_soaktime_least_used_gear = function(gears) {
    return(which.min(lapply(gears, function(x) { max(x$soaktime_sum) })))
  },
  # @formatter:off
  #'@description
  #' Level off every gear's total soaktime to a given value
  #'
  #' @param gears array of gears for which cummulative soaktime has already been calculated
  #' @param soaktime_threshold a named list containing the name and the maximum soaktime of the least used gear
  #' @returns a named list containing the levelled-off gear dataframes
  #' @export
  # @formatter:on
  cut_gears_soaktime_by_threshold = function(gears, soaktime_threshold) {
    return(
      lapply(gears, function(x) {
        x <- x %>% dplyr::filter(soaktime_sum <= soaktime_threshold[[1]])
      })
    )
  },
  # @formatter:off
  #'@description
  #' Calculate the area ratio of each gear based on the total number of panels and the average panel's area ratio
  #'
  #' @param gears array of gears for which cummulative soaktime has been levelled off
  #' @returns a named list of the area ratios
  #' @export
  # @formatter:on
  calculate_gear_area_ratios = function(gears) {
    gear_ratios <- list()
    for (gear_name in names(gears)) {
      num_panels <- sum(gears[[gear_name]]$panels)
      panel_ratio <- self$gear_area['gear' == gear_name, 'area_ratio']
      gear_ratios [[gear_name]] <- num_panels * panel_ratio
    }
    return(gear_ratios)
  },
  # @formatter:off
  #'@description
  #' Get the unique haul ids of each row for all gears involved in the calculation of the fishing power
  #'
  #' @param gears array of gears for which cummulative soaktime has been levelled off
  #' @returns a vector with haul_ids
  #' @export
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
  }
))

source("config.R")
if (!exists('catch')) {
  catch <- readr::read_csv(DB_TALLAS_PARTIALLY_CLEAN_PATH)
}

# This bit is going to be given
gear_soacktime <- readr::read_csv(DB_ARTES_PIEZAS_SOAK_TIME_CLEAN_PATH)
gear_ratio_area <- (readr::read_csv(DB_ARTES_AREA_RATIO_PATH)) %>%
  dplyr::filter(gear %in% c("MIÑOS", "VETAS"))

# Get subsample of gears by species
t.luscus <- gear_soacktime %>%
  dplyr::filter(species == "Trisopterus luscus") %>%
  dplyr::filter(gear %in% gear_ratio_area$gear)


# Calculate the soaktime of each haul for minios by randomising the rows first
t.luscus.minios <- t.luscus %>%
  dplyr::filter(gear == "MIÑOS")
t.luscus.minios <- t.luscus.minios[sample(nrow(t.luscus.minios)),]

t.luscus.minios <- t.luscus.minios %>%
  dplyr::mutate(id = 1:nrow(t.luscus.minios)) %>%
  dplyr::mutate(soaktime_sum = cumsum(soak_time))

# Calculate the soaktime of each haul for vetas by randomising the rows first
t.luscus.vetas <- t.luscus %>% dplyr::filter(gear == "VETAS")
t.luscus.vetas <- t.luscus.vetas[sample(nrow(t.luscus.vetas)),]

t.luscus.vetas <- t.luscus.vetas %>%
  dplyr::mutate(id = 1:nrow(t.luscus.vetas)) %>%
  dplyr::mutate(soaktime_sum = cumsum(soak_time))


# Cut the soaktime of the gears above the maximun soaktime of the gear least employed
t.luscus.minios_cut_soaktime <- t.luscus.minios %>%
  dplyr::filter(soaktime_sum <= max(t.luscus.vetas$soaktime_sum))

# Calculate the ratio among gears' total area
t.luscus.minios_area <- sum(t.luscus.minios_cut_soaktime$panels) * 0.58
t.luscus.vetas_area <- sum(t.luscus.vetas$panels) * 1
t.luscus.minios_ratio <- round(t.luscus.minios_area / t.luscus.vetas_area, 2)

# Fetch the hauls that will be used to calculate the selectivity curve
hauls_id <- unique(c(t.luscus.vetas$haul_id, t.luscus.minios_cut_soaktime$haul_id))


# t.luscus <- merge(t.luscus, gear_ratio_area, by="gear", all = TRUE)

#
# read_csv_input <- function(filename) {
#   return(as.data.frame(
#     readr::read_csv(filename, locale = readr::locale(encoding = 'latin1'))
#   ))
# }
#
# if (!exists('clean_db_data_tallas')) {
#   clean_db_data_tallas <- read_csv_input(DB_TALLAS_PARTIALLY_CLEAN_PATH)
# }
#
# t.luscus <- clean_db_data_tallas %>%
#   dplyr::filter(ESPECIE == "Trisopterus Luscus")