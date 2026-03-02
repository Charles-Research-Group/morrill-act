library(sf)
library(dplyr)

parcels <- readRDS("data/preprocessed/parcel_data.rds")

# keep only what you actually need in the app
parcels_small <- parcels %>%
  select(MTRSA_LG, University.x, Present_Day_Tribes, geometry) %>%  # adjust columns you need
  st_make_valid() %>%
  st_simplify(dTolerance = 50, preserveTopology = TRUE) %>% # tune tolerance
  st_cast("MULTIPOLYGON", warn = FALSE)

saveRDS(parcels_small, "data/preprocessed/parcel_data_small.rds", compress = "xz")