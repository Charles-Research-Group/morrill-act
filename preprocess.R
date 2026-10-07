# library(sf)
# library(dplyr)
# library(tidyr)
# 
# sf_use_s2(FALSE)
# 
# # ============================================================
# # 0. SETUP
# # ============================================================
# 
# dir.create(
#   "data/preprocessed",
#   showWarnings = FALSE,
#   recursive = TRUE
# )
# 
# 
# # ============================================================
# # 1. LOAD ORIGINAL DATA
# # ============================================================
# 
# raw <- st_read(
#   "data/landgrabu-data/shapes/Parcel_Polygons.shp",
#   quiet = TRUE
# )
# 
# # Force uniform 2D geometry
# raw <- st_zm(raw, drop = TRUE, what = "ZM")
# 
# parcels_csv <- read.csv(
#   "data/landgrabu-data/csvs/Parcels.csv",
#   stringsAsFactors = FALSE
# )
# 
# raw$MTRSA_LG <- as.character(raw$MTRSA_LG)
# parcels_csv$MTRSA_LG <- as.character(parcels_csv$MTRSA_LG)
# pdt_csv <- read.csv("data/Parcels_withPDT.csv", stringsAsFactors = FALSE)
# 
# 
# # ============================================================
# # 2. JOIN METADATA
# # ============================================================
# 
# parcel_data_original <- raw |>
#   left_join(
#     parcels_csv,
#     by = "MTRSA_LG",
#     relationship = "many-to-many"
#   ) |>
#   left_join(
#     pdt_csv |> select(MTRSA_LG, Present_Day_Tribes),
#     by = "MTRSA_LG",
#     relationship = "many-to-many"
#   )
# 
# parcel_data_original <- parcel_data_original |>
#   separate_rows(Present_Day_Tribes, sep = ";") |>
#   mutate(Present_Day_Tribes = trimws(Present_Day_Tribes))
# 
# # ============================================================
# # 2B. CLEAN TEXT ENCODING
# # ============================================================
# 
# parcel_data_original <- parcel_data_original |>
#   mutate(
#     University = iconv(University, from = "", to = "UTF-8", sub = ""),
#     Present_Day_Tribes = iconv(Present_Day_Tribes, from = "", to = "UTF-8", sub = "")
#   )
# 
# cat("Joined rows:", nrow(parcel_data_original), "\n")
# 
# 
# # ============================================================
# # 3. CREATE parcel_data_small
# # ============================================================
# 
# parcel_data_small <- parcel_data_original |>
#   transmute(
#     MTRSA_LG = MTRSA_LG,
#     University.x = gsub('/', ' & ', University),   # also folds in the earlier university fix
#     Present_Day_Tribes = Present_Day_Tribes,
#     geometry = geometry
#   )
# 
# saveRDS(
#   parcel_data_small,
#   "data/preprocessed/parcel_data_small.rds",
#   compress = "xz"
# )
# 
# cat(
#   "parcel_data_small saved:",
#   nrow(parcel_data_small),
#   "rows\n"
# )
# 
# 
# # ============================================================
# # 4. MAKE AGGREGATION COPY
# # ============================================================
# 
# aggregate_data <- parcel_data_small
# 
# 
# # ============================================================
# # 5. REMOVE THE 4 KNOWN BROKEN PARCELS
# # ============================================================
# 
# validity <- st_is_valid(aggregate_data)
# 
# bad <- which(
#   !is.na(validity) & !validity
# )
# 
# cat("\n========================================\n")
# cat("REMOVING BROKEN PARCELS\n")
# cat("========================================\n")
# 
# cat("Broken parcels:", length(bad), "\n")
# 
# if (length(bad) > 0) {
#   
#   print(
#     data.frame(
#       row = bad,
#       MTRSA_LG = aggregate_data$MTRSA_LG[bad],
#       reason = st_is_valid(
#         aggregate_data[bad, ],
#         reason = TRUE
#       )
#     )
#   )
#   
#   aggregate_data <- aggregate_data[-bad, ]
# }
# 
# cat(
#   "Remaining parcels:",
#   nrow(aggregate_data),
#   "\n"
# )
# 
# 
# # ============================================================
# # 6. KEEP ONLY PARCELS WITH UNIVERSITY DATA
# # ============================================================
# 
# aggregate_data <- aggregate_data |>
#   filter(
#     !is.na(University.x),
#     trimws(University.x) != ""
#   )
# 
# cat(
#   "Parcels with university:",
#   nrow(aggregate_data),
#   "\n"
# )
# 
# 
# # ============================================================
# # 7. TRANSFORM TO EPSG:4326
# # ============================================================
# 
# cat("\nTransforming to EPSG:4326...\n")
# 
# aggregate_data_4326 <- st_transform(aggregate_data, 4326)
# 
# cat("Transformation successful.\n")
# 
# 
# # ============================================================
# # 8. SAVE PARCELS BY UNIVERSITY
# # ============================================================
# #
# # Each parcel remains its own polygon.
# # No st_union() or st_combine().
# #
# 
# parcel_by_university <- aggregate_data_4326 |>
#   filter(
#     !is.na(University.x),
#     trimws(University.x) != ""
#   ) |>
#   select(
#     MTRSA_LG,
#     University.x,
#     Present_Day_Tribes,
#     geometry
#   )
# 
# saveRDS(
#   parcel_by_university,
#   "data/preprocessed/parcel_by_university.rds",
#   compress = "xz"
# )
# 
# cat(
#   "Saved parcel_by_university.rds:",
#   nrow(parcel_by_university),
#   "rows\n"
# )
# 
# 
# # ============================================================
# # 9. SAVE PARCELS BY TRIBE
# # ============================================================
# 
# parcel_by_tribe <- aggregate_data_4326 |>
#   filter(
#     !is.na(Present_Day_Tribes),
#     trimws(Present_Day_Tribes) != ""
#   ) |>
#   select(
#     MTRSA_LG,
#     University.x,
#     Present_Day_Tribes,
#     geometry
#   )
# 
# saveRDS(
#   parcel_by_tribe,
#   "data/preprocessed/parcel_by_tribe.rds",
#   compress = "xz"
# )
# 
# cat(
#   "Saved parcel_by_tribe.rds:",
#   nrow(parcel_by_tribe),
#   "rows\n"
# )
# 
# 
# # ============================================================
# # 10. SAVE PARCELS BY UNIVERSITY + TRIBE
# # ============================================================
# 
# parcel_by_university_tribe <- aggregate_data_4326 |>
#   filter(
#     !is.na(University.x),
#     trimws(University.x) != "",
#     !is.na(Present_Day_Tribes),
#     trimws(Present_Day_Tribes) != ""
#   ) |>
#   select(
#     MTRSA_LG,
#     University.x,
#     Present_Day_Tribes,
#     geometry
#   )
# 
# saveRDS(
#   parcel_by_university_tribe,
#   "data/preprocessed/parcel_by_university_tribe.rds",
#   compress = "xz"
# )
# 
# cat(
#   "Saved parcel_by_university_tribe.rds:",
#   nrow(parcel_by_university_tribe),
#   "rows\n"
# )
# 
# 
# # ============================================================
# # 11. SAVE UNIVERSITY LIST
# # ============================================================
# 
# university_list <- aggregate_data_4326 |>
#   st_drop_geometry() |>
#   filter(
#     !is.na(University.x),
#     trimws(University.x) != ""
#   ) |>
#   distinct(University.x) |>
#   arrange(University.x)
# 
# saveRDS(
#   university_list,
#   "data/preprocessed/university_list.rds",
#   compress = "xz"
# )
# 
# cat(
#   "Saved university_list.rds:",
#   nrow(university_list),
#   "universities\n"
# )
# 
# 
# # ============================================================
# # 12. SAVE TRIBE LIST
# # ============================================================
# 
# tribe_list <- aggregate_data_4326 |>
#   st_drop_geometry() |>
#   filter(
#     !is.na(Present_Day_Tribes),
#     trimws(Present_Day_Tribes) != ""
#   ) |>
#   distinct(Present_Day_Tribes) |>
#   arrange(Present_Day_Tribes)
# 
# saveRDS(
#   tribe_list,
#   "data/preprocessed/tribe_list.rds",
#   compress = "xz"
# )
# 
# cat(
#   "Saved tribe_list.rds:",
#   nrow(tribe_list),
#   "tribes\n"
# )
# 
# 
# # ============================================================
# # 13. SAVE UNIVERSITY + TRIBE LIST
# # ============================================================
# 
# university_tribe_list <- aggregate_data_4326 |>
#   st_drop_geometry() |>
#   filter(
#     !is.na(University.x),
#     trimws(University.x) != "",
#     !is.na(Present_Day_Tribes),
#     trimws(Present_Day_Tribes) != ""
#   ) |>
#   distinct(
#     University.x,
#     Present_Day_Tribes
#   ) |>
#   arrange(
#     University.x,
#     Present_Day_Tribes
#   )
# 
# saveRDS(
#   university_tribe_list,
#   "data/preprocessed/university_tribe_list.rds",
#   compress = FALSE
# )
# 
# cat(
#   "Saved university_tribe_list.rds:",
#   nrow(university_tribe_list),
#   "university/tribe combinations\n"
# )


library(sf)
library(dplyr)

ok <- st_read('data/reservation-shapes/OklahomaTribalStatisticalAreas.shp')
res_shapes <- readRDS('data/preprocessed/reservations.rds')   # whichever file you currently have

add_both_areas <- function(res, ok, tribe_name) {
  tmpl <- res %>% filter(TRIBE_NAME == tribe_name) %>% slice(1)
  stopifnot(nrow(tmpl) == 1)
  
  new <- ok %>%
    filter(TRIBE_NAME == tribe_name) %>%
    st_transform(st_crs(res)) %>%
    st_make_valid()
  stopifnot(nrow(new) >= 1)
  
  rows <- tmpl[rep(1, nrow(new)), ]
  st_geometry(rows) <- st_cast(st_geometry(new), 'MULTIPOLYGON')
  
  res %>%
    filter(TRIBE_NAME != tribe_name) %>%
    bind_rows(rows)
}

res_shapes <- res_shapes %>%
  add_both_areas(ok, 'The Muscogee (Creek) Nation') %>%
  add_both_areas(ok, 'Delaware Nation, Oklahoma')

# sanity check: should show 2 rows each
res_shapes %>%
  filter(TRIBE_NAME %in% c('The Muscogee (Creek) Nation', 'Delaware Nation, Oklahoma')) %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  select(TRIBE_NAME, area_km2)

saveRDS(res_shapes, 'data/preprocessed/reservations_clean2.rds')