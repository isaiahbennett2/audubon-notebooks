data_path <- "C:/Users/isaia/Downloads/Inuit Bird Ranges/Arctic Outputs/eBirdRasters/2023"


raster_files <- list.files(
  path = data_path,
  pattern = "_abundance_seasonal_mean_3km_2023\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

print(raster_files)