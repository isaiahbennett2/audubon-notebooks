 
# ============================================================
# eBird Arctic Breeding Abundance Rasters
# Downloads 3 km seasonal mean abundance, extracts breeding
# season, and clips rasters to the Canadian Arctic ecozones.
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------
library(sf)
library(ebirdst)
library(terra)


# ------------------------------------------------------------
# 2. PATHS AND SETTINGS
# ------------------------------------------------------------

# Set eBird access key
set_ebirdst_access_key("161487c0-be01-46b8-8a22-242c78e2f16d", overwrite = TRUE)


# Folder where eBird downloads will be stored
data_dir <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters"

# CSV containing your species list
species_csv <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/FWS_BLI_ArcticBreeders.csv"

# Merged Canadian Arctic ecozones shapefile
arctic_shapefile <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/ArcticEcozonesSolid.shp"

# Output folder for clipped breeding abundance rasters
output_dir <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters/Clipped"

# Create folders if they don't exist
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Tell ebirdst where to store downloaded data
Sys.setenv(EBIRDST_DATA_DIR = data_dir)


# ------------------------------------------------------------
# 3. CHECK DATA VERSION
# ------------------------------------------------------------

cat("\nInstalled ebirdst package version:\n")
print(packageVersion("ebirdst"))

cat("\neBird Status and Trends data version:\n")
version_info <- ebirdst_version()
print(version_info)


# ------------------------------------------------------------
# 4. LOAD SPECIES LIST
# ------------------------------------------------------------

species_table <- read.csv(
  species_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Confirm required column exists
if (!"CommonName" %in% names(species_table)) {
  stop("Your CSV must contain a column named 'CommonName'.")
}

# Remove blank species names
species_table <- species_table[
  !is.na(species_table$CommonName) &
    trimws(species_table$CommonName) != "",
]

# Remove duplicate species
species_table <- species_table[
  !duplicated(tolower(trimws(species_table$CommonName))),
]

spp_list <- trimws(species_table$CommonName)

# Add a status column if one isn't present
if (!"BreedingStatus" %in% names(species_table)) {
  species_table$BreedingStatus <- NA_character_
}

cat("\nNumber of species in CSV:", length(spp_list), "\n")


# ------------------------------------------------------------
# 5. CHECK WHICH SPECIES ARE AVAILABLE
# ------------------------------------------------------------

available_species <- ebirdst_runs$common_name

available_species_lower <- tolower(available_species)

species_available <- tolower(spp_list) %in% available_species_lower

cat("\nSpecies available in ebirdst:",
    sum(species_available), "\n")

cat("Species not available:",
    sum(!species_available), "\n")


# ------------------------------------------------------------
# 6. LOAD AND PROJECT ARCTIC POLYGON
# ------------------------------------------------------------

cat("\nLoading Arctic ecozones polygon...\n")

arctic_poly <- vect(arctic_shapefile)

if (is.na(crs(arctic_poly)) || crs(arctic_poly) == "") {
  stop("The Arctic shapefile does not have a defined coordinate system.")
}

cat("Arctic polygon loaded.\n")


# ------------------------------------------------------------
# 7. PROCESS SPECIES
# ------------------------------------------------------------

results <- vector("list", length(spp_list))

for (i in seq_along(spp_list)) {
  
  spp <- spp_list[i]
  
  breeding_status <- species_table$BreedingStatus[i]
  
  cat("\n----------------------------------------\n")
  cat("Processing:", spp, "\n")
  cat("----------------------------------------\n")
  
  
  # Skip species not available in the eBird release
  if (!species_available[i]) {
    
    cat("Species not available. Skipping.\n")
    
    results[[i]] <- data.frame(
      CommonName = spp,
      BreedingStatus = breeding_status,
      Result = "Not available in ebirdst",
      OutputFile = NA_character_,
      stringsAsFactors = FALSE
    )
    
    next
  }
  
  
  # Download, load, crop, and save
  result <- tryCatch({
    
    # Load seasonal mean relative abundance at 3 km
    # load_raster automatically downloads missing data
    abd_seasonal <- load_raster(
      species = spp,
      product = "abundance",
      period = "seasonal",
      metric = "mean",
      resolution = "3km"
    )
    
    
    # Check whether a breeding-season layer exists
    if (!"breeding" %in% names(abd_seasonal)) {
      
      cat("No breeding-season layer available. Skipping.\n")
      
      list(
        CommonName = spp,
        BreedingStatus = breeding_status,
        Result = "No breeding layer",
        OutputFile = NA_character_
      )
      
    } else {
      
      
      # Extract breeding-season layer
      abd_breeding <- abd_seasonal[["breeding"]]
      
      # Load species-specific eBird mapping parameters
      pars <- load_fac_map_parameters(spp)
      
      # Get species-specific custom projection
      species_crs <- sf::st_crs(pars$custom_projection)
      
      # Project Arctic polygon to original raster CRS
      arctic_native <- project(
        arctic_poly,
        crs(abd_breeding)
      )
      
      # Crop and mask in native raster CRS
      abd_crop <- crop(abd_breeding, arctic_native)
      abd_mask <- mask(abd_crop, arctic_native)
      
      # Project to Canada Lambert Conformal Conic
      abd_lambert <- project(
        abd_mask,
        "EPSG:3347",
        method = "near",
        res = 3000
      )
      
      # Create output filename
      spp_filename <- gsub(
        "[^A-Za-z0-9_-]+",
        "_",
        spp
      )
      
      output_file <- file.path(
        output_dir,
        paste0(spp_filename, "_breeding_abundance.tif")
      )
      
      # Save projected raster
      writeRaster(
        abd_lambert,
        output_file,
        overwrite = TRUE,
        wopt = list(gdal = "COMPRESS=LZW")
      )
      
      
      
      cat("Successfully saved:", output_file, "\n")
      
      list(
        CommonName = spp,
        BreedingStatus = breeding_status,
        Result = "Successfully saved",
        OutputFile = output_file
      )
    }
    
  }, error = function(e) {
    
    cat("ERROR:", conditionMessage(e), "\n")
    
    list(
      CommonName = spp,
      BreedingStatus = breeding_status,
      Result = paste("Error:", conditionMessage(e)),
      OutputFile = NA_character_
    )
    
  })
  
  
  results[[i]] <- as.data.frame(
    result,
    stringsAsFactors = FALSE
  )
  
}


# ------------------------------------------------------------
# 8. EXPORT PROCESSING REPORT
# ------------------------------------------------------------

results_df <- do.call(rbind, results)

report_path <- file.path(
  output_dir,
  "Arctic_Breeding_Raster_Report.csv"
)

write.csv(
  results_df,
  report_path,
  row.names = FALSE
)


# ------------------------------------------------------------
# 9. SUMMARY
# ------------------------------------------------------------

cat("\n\n========================================\n")
cat("PROCESSING COMPLETE\n")
cat("========================================\n")

cat(
  "Successfully saved:",
  sum(results_df$Result == "Successfully saved"),
  "\n"
)

cat(
  "No breeding layer:",
  sum(results_df$Result == "No breeding layer"),
  "\n"
)

cat(
  "Not available:",
  sum(results_df$Result == "Not available in ebirdst"),
  "\n"
)

cat(
  "Errors:",
  sum(grepl("^Error:", results_df$Result)),
  "\n"
)

cat("\nOutput directory:\n", output_dir, "\n")

cat("\nProcessing report:\n", report_path, "\n")
