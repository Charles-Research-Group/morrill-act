library(sf)
library(dplyr)
sf_use_s2(FALSE)

# ============================================================
# CONFIG
# ============================================================
SIMPLIFY_TOLERANCE_M <- 5
PROJECTED_CRS <- 5070

# rmapshaper converts sf -> GeoJSON text -> V8 JS engine (mapshaper).
# For large parcel datasets this text blow-up + V8 heap limit is the
# most common cause of a hard crash ("fatal error"), so it's OFF by
# default here. Only flip this on for a final polish pass on data
# that's ALREADY been downsized by st_simplify below.
USE_RMAPSHAPER <- FALSE

# Number of chunks to split each layer into before simplifying.
# More chunks = lower peak memory per step, slower overall.
# Bump this up (20, 50...) if it still crashes.
N_CHUNKS <- 10

# ============================================================
# HELPERS
# ============================================================
report_sizes <- function(obj, path, label) {
  mem_mb <- format(object.size(obj), units = "MB")
  disk_mb <- if (file.exists(path)) {
    paste0(round(file.size(path) / 1024^2, 1), " MB")
  } else {
    "not written yet"
  }
  cat(sprintf("%-35s in-memory: %-12s on-disk: %s\n", label, mem_mb, disk_mb))
}

# Simplifies one already-projected chunk. Kept tiny and self-contained
# so nothing extra lingers in memory after each call returns.
simplify_chunk <- function(chunk, tolerance_m) {
  if (USE_RMAPSHAPER) {
    out <- rmapshaper::ms_simplify(
      chunk, keep = 0.10, keep_shapes = TRUE, snap = TRUE
    )
  } else {
    out <- st_simplify(chunk, dTolerance = tolerance_m, preserveTopology = TRUE)
  }
  out <- st_make_valid(out)
  out[!st_is_empty(out), ]
}

# Processes a whole layer in chunks, with progress printout + gc()
# between chunks to keep peak memory down.
simplify_layer_batched <- function(x, tolerance_m, crs, n_chunks) {
  orig_crs <- st_crs(x)
  
  cat("  reprojecting...\n")
  x <- st_transform(x, crs)
  
  n <- nrow(x)
  chunk_id <- ceiling(seq_len(n) / (n / n_chunks))
  
  results <- vector("list", n_chunks)
  
  for (i in seq_len(n_chunks)) {
    cat(sprintf("  simplifying chunk %d/%d (%d rows)...\n",
                i, n_chunks, sum(chunk_id == i)))
    
    chunk <- x[chunk_id == i, ]
    results[[i]] <- simplify_chunk(chunk, tolerance_m)
    
    rm(chunk)
    gc(full = TRUE, verbose = FALSE)
  }
  
  rm(x)
  gc(full = TRUE, verbose = FALSE)
  
  cat("  combining chunks...\n")
  out <- bind_rows(results)
  rm(results)
  gc(full = TRUE, verbose = FALSE)
  
  st_transform(out, orig_crs)
}

# Full pipeline for one file: load -> clean -> simplify -> save -> free memory.
# Doing this one file at a time (instead of loading both up front) roughly
# halves peak memory usage.
process_file <- function(in_path, out_path, select_cols) {
  cat(sprintf("\n=== Processing %s ===\n", in_path))
  
  x <- readRDS(in_path)
  report_sizes(x, in_path, "loaded")
  
  x <- x %>% select(all_of(select_cols)) %>% distinct()
  x <- st_zm(x, drop = TRUE, what = "ZM")
  gc(full = TRUE, verbose = FALSE)
  
  x <- simplify_layer_batched(x, SIMPLIFY_TOLERANCE_M, PROJECTED_CRS, N_CHUNKS)
  
  cat("  saving...\n")
  saveRDS(x, out_path, compress = "xz")
  
  report_sizes(x, out_path, "final")
  
  rm(x)
  gc(full = TRUE, verbose = FALSE)
  invisible(NULL)
}

# ============================================================
# RUN ONE FILE AT A TIME
# ============================================================
process_file(
  "data/preprocessed/parcel_by_university.rds",
  "data/preprocessed/parcel_by_university_small.rds",
  c("University.x", "MTRSA_LG", "geometry")
)

process_file(
  "data/preprocessed/parcel_by_university_tribe.rds",
  "data/preprocessed/parcel_by_university_tribe_small.rds",
  c("University.x", "Present_Day_Tribes", "MTRSA_LG", "geometry")
)