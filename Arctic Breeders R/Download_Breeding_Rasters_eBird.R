
# ============================================================
# eBird Breeding Abundance Rasters
# Downloads 3 km seasonal mean abundance and extracts
# the breeding band without clipping or reprojection.
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------
library(ebirdst)
library(terra)
library(dplyr)


# ------------------------------------------------------------
# 2. PATHS AND SETTINGS
# ------------------------------------------------------------

set_ebirdst_access_key(
  "161487c0-be01-46b8-8a22-242c78e2f16d",
  overwrite = TRUE
)

data_dir <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters"

species_csv <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/FWS_BLI_ArcticBreeders.csv"

output_dir <- file.path(data_dir, "Breeding")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(EBIRDST_DATA_DIR = data_dir)

# ------------------------------------------------------------
# 3. CHECK DATA VERSION
# ------------------------------------------------------------

cat("\nInstalled ebirdst package version:\n")
print(packageVersion("ebirdst"))

cat("\neBird Status and Trends data version:\n")
print(ebirdst_version())


# ------------------------------------------------------------
# 4. LOAD SPECIES LIST
# ------------------------------------------------------------

species_table <- read.csv(
  species_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (!"CommonName" %in% names(species_table)) {
  stop("Your CSV must contain a column named 'CommonName'.")
}

species_table <- species_table[
  !is.na(species_table$CommonName) &
    trimws(species_table$CommonName) != "",
]

species_table <- species_table[
  !duplicated(tolower(trimws(species_table$CommonName))),
]

spp_list <- trimws(species_table$CommonName)

if (!"BreedingStatus" %in% names(species_table)) {
  species_table$BreedingStatus <- NA_character_
}

cat("\nNumber of species in CSV:", length(spp_list), "\n")


# ------------------------------------------------------------
# 5. MATCH SPECIES TO EBIRD CODES
# ------------------------------------------------------------

species_crosswalk <- species_table |>
  mutate(name_key = tolower(trimws(CommonName))) |>
  left_join(
    ebirdst_runs |>
      transmute(
        name_key = tolower(trimws(common_name)),
        species_code
      ),
    by = "name_key"
  ) |>
  select(CommonName, species_code, BreedingStatus)

cat("\nSpecies without a matching eBird code:\n")
print(species_crosswalk |> filter(is.na(species_code)))


# ------------------------------------------------------------
# 6. CHECK AVAILABILITY
# ------------------------------------------------------------

available_species <- tolower(ebirdst_runs$common_name)

species_available <- tolower(spp_list) %in% available_species

cat("\nSpecies available:", sum(species_available), "\n")
cat("Species unavailable:", sum(!species_available), "\n")


# ------------------------------------------------------------
# 7. DOWNLOAD AND EXTRACT BREEDING RASTERS
# ------------------------------------------------------------

results <- vector("list", length(spp_list))

for (i in seq_along(spp_list)) {
  
  spp <- spp_list[i]
  breeding_status <- species_table$BreedingStatus[i]
  
  cat("\n----------------------------------------\n")
  cat("Processing:", spp, "\n")
  cat("----------------------------------------\n")
  
  if (!species_available[i]) {
    
    results[[i]] <- data.frame(
      CommonName = spp,
      BreedingStatus = breeding_status,
      Result = "Not available in ebirdst",
      OutputFile = NA_character_
    )
    
    next
  }
  
  result <- tryCatch({
    
    # Download/load original seasonal mean abundance
    abd_seasonal <- load_raster(
      species = spp,
      product = "abundance",
      period = "seasonal",
      metric = "mean",
      resolution = "3km"
    )
    
    if (!"breeding" %in% names(abd_seasonal)) {
      stop("No breeding band found.")
    }
    
    # Extract breeding band only
    abd_breeding <- abd_seasonal[["breeding"]]
    
    # Preserve native raster CRS, extent, and resolution
    spp_code <- species_crosswalk$species_code[i]
    
    output_file <- file.path(
      output_dir,
      paste0(spp_code, "_breeding_abundance_3km_2023.tif")
    )
    
    writeRaster(
      abd_breeding,
      output_file,
      overwrite = TRUE,
      wopt = list(gdal = "COMPRESS=LZW")
    )
    
    cat("Successfully saved:", output_file, "\n")
    
    data.frame(
      CommonName = spp,
      BreedingStatus = breeding_status,
      Result = "Successfully saved",
      OutputFile = output_file
    )
    
  }, error = function(e) {
    
    cat("ERROR:", conditionMessage(e), "\n")
    
    data.frame(
      CommonName = spp,
      BreedingStatus = breeding_status,
      Result = paste("Error:", conditionMessage(e)),
      OutputFile = NA_character_
    )
    
  })
  
  results[[i]] <- result
}


# ------------------------------------------------------------
# 8. SAVE DOWNLOAD RESULTS
# ------------------------------------------------------------

results_df <- bind_rows(results)

write.csv(
  results_df,
  file.path(output_dir, "breeding_raster_download_results.csv"),
  row.names = FALSE
)

cat("\nDownload process complete.\n")
print(table(results_df$Result))
