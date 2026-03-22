library(sf)
library(dplyr)
sf_use_s2(FALSE)

# ---------- Helper ----------
prep_polygons <- function(x) {
  x |> st_make_valid() |> st_transform(4326)
}

# ---------- Ensure output dir exists ----------
dir.create("data/preprocessed",
           showWarnings = FALSE,
           recursive = TRUE)

# ---------- Parcels ----------
parcels <- st_read("data/landgrabu-data/shapes/Parcel_Polygons.shp", quiet = TRUE)
parcels_csv <- read.csv("data/landgrabu-data/csvs/Parcels.csv", stringsAsFactors = FALSE)
parcels$MTRSA_LG <- as.character(parcels$MTRSA_LG)
parcels_csv$MTRSA_LG <- as.character(parcels_csv$MTRSA_LG)
parcels_joined <- parcels |> left_join(parcels_csv, by = "MTRSA_LG") |> prep_polygons()
saveRDS(parcels_joined,
        "data/preprocessed/parcel_polygons.rds",
        compress = FALSE)

# ---------- Reservations ----------
reservations <- st_read("data/reservation-shapes/AmericanIndianReservations.shp",
                        quiet = TRUE)
reservations_clean <- reservations |> prep_polygons()
saveRDS(reservations_clean,
        "data/preprocessed/reservations.rds",
        compress = FALSE)

# ---------- Universities ----------
universities <- st_read("data/landgrabu-data/shapes/University_Points.shp", quiet = TRUE) |> st_make_valid() |> st_transform(4326)
saveRDS(universities,
        "data/preprocessed/universities.rds",
        compress = FALSE)

message("✅ Spatial preprocessing complete (RDS)")