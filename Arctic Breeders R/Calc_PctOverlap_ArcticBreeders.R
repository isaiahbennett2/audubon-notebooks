
# ============================================================
# eBird breeding abundance overlap with a study-area polygon
#
# Outputs, for every species:
#   overlap                         = TRUE if any positive
#                                     abundance occurs in polygon
#   positive_abundance_cell_equiv   = polygon-weighted cell
#                                     equivalents with abundance > 0
#   modeled_cell_equiv              = total polygon-weighted
#                                     cell equivalents with data
#   polygon_cell_equiv              = total polygon-weighted
#                                     cell equivalents
#   percent_modeled_area_with_abundance
#                                   = % of modeled area with
#                                     abundance > 0
#   percent_polygon_with_abundance = % of entire polygon with
#                                     abundance > 0
#   mean_relative_abundance         = coverage-weighted mean
#                                     abundance across modeled area
#   abundance_weighted_cell_sum     = sum of abundance values
#                                     weighted by cell coverage
#
# Partial boundary cells are weighted by their coverage fraction.
# ============================================================


# 1. Packages -------------------------------------------------

library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(purrr)
library(stringr)
library(readr)


# 2. User-defined paths ---------------------------------------

shapefile_path <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/ArcticEcozonesSolid.shp"

data_path <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters/Clipped"

output_folder <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters/OverlapOutput"

output_path <- file.path(
  output_folder,
  "ebird_breeding_abundance_overlap.csv"
)


# 3. Read and prepare the study-area polygon ------------------

study_area <- st_read(shapefile_path, quiet = TRUE) |>
  st_make_valid() |>
  summarise(geometry = st_union(geometry))

study_area <- st_zm(study_area, drop = TRUE, what = "ZM")


# 4. Find all breeding abundance rasters -----------------------

raster_files <- list.files(
  path = data_path,
  pattern = "_breeding_abundance\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(raster_files) == 0) {
  stop("No raster files found. Check data_path.")
}

print(length(raster_files))


# 5. Extract species identifiers ------------------------------

extract_species_name <- function(file) {
  
  filename <- tools::file_path_sans_ext(basename(file))
  
  str_remove(
    filename,
    "_breeding_abundance$"
  ) |>
    str_replace_all("_", " ")
}


# 6. Summarize one abundance raster ----------------------------


summarize_abundance_raster <- function(file, polygon) {
  
  species_name <- extract_species_name(file)
  
  # Attempt to read the raster and check for valid values.
  # An unreadable TIFF is handled separately from an empty raster.
  
  r <- tryCatch(
    rast(file),
    error = function(e) NULL
  )
  
  if (is.null(r)) {
    warning("Could not open raster: ", basename(file))
    return(NULL)
  }
  
  if (nlyr(r) != 1) {
    stop("Expected a single-layer raster: ", basename(file))
  }
  
  if (is.na(crs(r)) || crs(r) == "") {
    stop("Raster has no defined CRS: ", basename(file))
  }
  
  raster_min <- tryCatch(
    global(r, "min", na.rm = TRUE)[1, 1],
    error = function(e) NA_real_
  )
  
  # Return zero overlap if the raster is entirely empty.
  
  if (is.na(raster_min)) {
    return(
      tibble(
        species_name = species_name,
        overlap = FALSE,
        positive_abundance_cell_equiv = 0,
        modeled_cell_equiv = 0,
        polygon_cell_equiv = NA_real_,
        modeled_area_percent_of_polygon = NA_real_,
        percent_modeled_area_with_abundance = NA_real_,
        percent_polygon_with_abundance = NA_real_,
        mean_relative_abundance = NA_real_,
        abundance_weighted_cell_sum = NA_real_,
        max_relative_abundance = NA_real_,
        raster_file = basename(file)
      )
    )
  }
  
  # Reproject polygon to raster CRS.
  
  polygon_raster_crs <- st_transform(
    polygon,
    crs(r)
  )
  
  # Extract raster values and polygon coverage fractions.
  
  cell_data <- tryCatch(
    exact_extract(r, polygon_raster_crs)[[1]],
    error = function(e) NULL
  )
  
  if (is.null(cell_data)) {
    warning("Could not extract raster values: ", basename(file))
    return(NULL)
  }
  
  polygon_cell_equiv <- sum(
    cell_data$coverage_fraction,
    na.rm = TRUE
  )
  
  valid_data <- cell_data |>
    filter(!is.na(value))
  
  modeled_cell_equiv <- sum(
    valid_data$coverage_fraction,
    na.rm = TRUE
  )
  
  if (nrow(valid_data) == 0 || modeled_cell_equiv == 0) {
    return(
      tibble(
        species_name = species_name,
        overlap = FALSE,
        positive_abundance_cell_equiv = 0,
        modeled_cell_equiv = 0,
        polygon_cell_equiv = polygon_cell_equiv,
        modeled_area_percent_of_polygon = 0,
        percent_modeled_area_with_abundance = NA_real_,
        percent_polygon_with_abundance = 0,
        mean_relative_abundance = NA_real_,
        abundance_weighted_cell_sum = NA_real_,
        max_relative_abundance = NA_real_,
        raster_file = basename(file)
      )
    )
  }
  
  positive_data <- valid_data |>
    filter(value > 0)
  
  positive_abundance_cell_equiv <- sum(
    positive_data$coverage_fraction,
    na.rm = TRUE
  )
  
  percent_modeled_area_with_abundance <-
    100 * positive_abundance_cell_equiv / modeled_cell_equiv
  
  percent_polygon_with_abundance <-
    100 * positive_abundance_cell_equiv / polygon_cell_equiv
  
  mean_relative_abundance <- weighted.mean(
    valid_data$value,
    valid_data$coverage_fraction
  )
  
  abundance_weighted_cell_sum <- sum(
    valid_data$value * valid_data$coverage_fraction,
    na.rm = TRUE
  )
  
  max_relative_abundance <- max(
    valid_data$value,
    na.rm = TRUE
  )
  
  tibble(
    species_name = species_name,
    overlap = positive_abundance_cell_equiv > 0,
    positive_abundance_cell_equiv = positive_abundance_cell_equiv,
    modeled_cell_equiv = modeled_cell_equiv,
    polygon_cell_equiv = polygon_cell_equiv,
    modeled_area_percent_of_polygon = 
      100 * modeled_cell_equiv / polygon_cell_equiv,
    percent_modeled_area_with_abundance =
      percent_modeled_area_with_abundance,
    percent_polygon_with_abundance =
      percent_polygon_with_abundance,
    mean_relative_abundance = mean_relative_abundance,
    abundance_weighted_cell_sum = abundance_weighted_cell_sum,
    max_relative_abundance = max_relative_abundance,
    raster_file = basename(file)
  )
}



# 7. Process every raster -------------------------------------

abundance_results <- map_dfr(
  raster_files,
  \(file) {
    summarize_abundance_raster(
      file = file,
      polygon = study_area
    )
  }
) |>
  arrange(desc(percent_modeled_area_with_abundance))



# 8. Export results --------------------------------------------

write_csv(
  abundance_results,
  output_path
)

message(
  "Finished. Results were written to:\n",
  output_path
)

# 9. Join proper name field -----------------------------------

# Replace species names in overlap report with original CommonName values.

raster_report <- read_csv(
  "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters/Clipped/Arctic_Breeding_Raster_Report.csv"
) |>
  mutate(
    CommonName_join = str_replace_all(CommonName, "'", " ")
  )

overlap_report <- read_csv(
  "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters/OverlapOutput/ebird_breeding_abundance_overlap.csv"
)

overlap_report <- overlap_report |>
  left_join(
    raster_report |>
      select(CommonName, CommonName_join),
    by = c("species_name" = "CommonName_join")
  ) |>
  mutate(
    species_name = coalesce(CommonName, species_name)
  ) |>
  select(-CommonName)

write_csv(
  overlap_report,
  "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters/OverlapOutput/ebird_breeding_abundance_overlap.csv"
)
