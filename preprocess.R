library(sf)
library(dplyr)
library(readr)

# Use planar geometry ops for preprocessing
sf_use_s2(FALSE)

# Load parcel data
parcel_data <- readRDS("data/preprocessed/parcel_data.rds")

# Clean raw parcel data
parcel_data_clean <- parcel_data %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  filter(!st_is_empty(.)) %>%
  mutate(
    University.x = trimws(University.x),
    Present_Day_Tribes = trimws(Present_Day_Tribes)
  ) %>%
  filter(
    !is.na(geometry)
  ) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON")

aggregate_and_simplify <- function(data, group_vars, tolerance_m = 1000) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(do_union = TRUE, .groups = "drop") %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON") %>%
    st_transform(5070) %>%   # projected CRS in meters
    st_simplify(dTolerance = tolerance_m, preserveTopology = TRUE) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON") %>%
    st_transform(4326)
}

# By university
parcel_by_university <- parcel_data_clean %>%
  filter(!is.na(University.x), University.x != "") %>%
  aggregate_and_simplify(group_vars = c("University.x"))

# By tribe
parcel_by_tribe <- parcel_data_clean %>%
  filter(!is.na(Present_Day_Tribes), Present_Day_Tribes != "") %>%
  aggregate_and_simplify(group_vars = c("Present_Day_Tribes"))

# By university + tribe
parcel_by_university_tribe <- parcel_data_clean %>%
  filter(
    !is.na(University.x), University.x != "",
    !is.na(Present_Day_Tribes), Present_Day_Tribes != ""
  ) %>%
  aggregate_and_simplify(group_vars = c("University.x", "Present_Day_Tribes"))

saveRDS(parcel_by_university, "data/preprocessed/parcel_by_university.rds")
saveRDS(parcel_by_tribe, "data/preprocessed/parcel_by_tribe.rds")
saveRDS(parcel_by_university_tribe, "data/preprocessed/parcel_by_university_tribe.rds")

parcel_by_university <- readRDS("data/preprocessed/parcel_by_university.rds")
parcel_by_tribe <- readRDS("data/preprocessed/parcel_by_tribe.rds")
parcel_by_university_tribe <- readRDS("data/preprocessed/parcel_by_university_tribe.rds")