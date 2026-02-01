library(sf)
library(dplyr)

parcel_data <- readRDS('data/preprocessed/parcel_data.rds')

# Just aggregate - no simplification
parcel_data_aggregated <- parcel_data %>%
  filter(st_geometry_type(.) %in% c('POLYGON', 'MULTIPOLYGON')) %>%
  group_by(University.x, Tribal_Nation.x) %>%
  summarise(
    parcel_count = n(),
    geometry = st_union(geometry),
    .groups = 'drop'
  )

saveRDS(parcel_data_aggregated, 'data/preprocessed/parcel_data_aggregated.rds')