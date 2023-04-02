calculate_sediment_type_hsi <- function(sediment_raster, optimal_sediment, suitability_values){
  # sediment_raster: a raster object containing categorical sediment type values
  # optimal_sediment: the sediment type at which the species has the highest fitness
  # suitability_values: a named vector of suitability values for each sediment type
  
  # Create a new raster with suitability values based on the categorical sediment type values
  hsi_sediment <- terra::reclassify(sediment_raster, suitability_values)
  
  # Set any negative values to 0
  hsi_sediment[hsi_sediment < 0] <- 0
  
  # Set the suitability value for the optimal sediment type to 1
  hsi_sediment[sediment_raster == optimal_sediment] <- 1
  
  # Return the HSI raster object
  return(hsi_sediment)
}