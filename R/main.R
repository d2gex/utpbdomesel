source("config.R")
source("gear_fishing_power_calculator.R")

# # Lengths caught per haul, gear and species
# if (!exists('catch')) {
#   catch <- readr::read_csv(DB_TALLAS_PARTIALLY_CLEAN_PATH)
# }

# Number of panels per gear and haul
gear_soacktime <- readr::read_csv(DB_ARTES_PIEZAS_SOAK_TIME_CLEAN_PATH)

# Average area ratio by gear and panel
gear_ratio_area <- (readr::read_csv(DB_ARTES_AREA_RATIO_PATH)) %>%
  dplyr::filter(gear %in% c("MIÑOS", "VETAS"))

t.lusucus_gear_soaktime <- gear_soacktime %>%
  dplyr::filter(species == "Trisopterus luscus") %>%
  dplyr::filter(gear %in% gear_ratio_area$gear)

g_fishing_power <- GearFishingPowerCalculator$new(t.lusucus_gear_soaktime, gear_ratio_area)
g_fishing_power$calculate_gear_fishing_power_by_trials()



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