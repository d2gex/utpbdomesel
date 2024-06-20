library ("magrittr")
DATA_PATH <- "../../../repos_data/utpblbm/data"
QGIS_PATH <- "../../../repos_data/utpblbm/qgis"
EXTRA_DATA_PATH <- "../data"
DATA_SENSITIVE_PATH <- file.path(DATA_PATH, 'sensitive')
OLD_DBS_PATH <- file.path(DATA_SENSITIVE_PATH, 'consulta_utpb_2018')
NEW_DBS_PATH <- file.path(DATA_SENSITIVE_PATH, 'consulta_utpb_2023')
NEW_CLEAN_DBS_PATH <- file.path(NEW_DBS_PATH, 'clean')
DB_TALLAS_PARTIALLY_CLEAN_PATH <- file.path(NEW_CLEAN_DBS_PATH, 'clean_db_tallas_partial.csv')
DB_ARTES_PIEZAS_SOAK_TIME_CLEAN_PATH <- file.path(NEW_CLEAN_DBS_PATH, 'clean_db_soaktime_piezas.csv')
DB_ARTES_AREA_RATIO_PATH <- file.path(NEW_CLEAN_DBS_PATH, 'area_ratio_gears.csv')