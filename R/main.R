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
result <- g_fishing_power$calculate_gear_fishing_power_by_trials()
