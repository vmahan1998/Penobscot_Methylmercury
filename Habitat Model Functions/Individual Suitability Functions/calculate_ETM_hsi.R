etm_hsi <- function(turbidity_raster, temp_raster, salinity_raster, sediment_raster) {
  
  # Rescale the rasters to a common scale (e.g., 0-1)
  turbidity_rescaled <- (turbidity_raster - min(turbidity_raster)) / (max(turbidity_raster) - min(turbidity_raster))
  temp_rescaled <- (temp_raster - min(temp_raster)) / (max(temp_raster) - min(temp_raster))
  salinity_rescaled <- (salinity_raster - min(salinity_raster)) / (max(salinity_raster) - min(salinity_raster))
  sediment_rescaled <- (sediment_raster - min(sediment_raster)) / (max(sediment_raster) - min(sediment_raster))
  
  # Compute the weighted average of the rasters, using weights based on expert knowledge or empirical data
  habitat_suitability <- 0.5*turbidity_rescaled + 0.2*temp_rescaled + 0.2*salinity_rescaled + 0.1*sediment_rescaled
  
  # Return the habitat suitability raster
  return(habitat_suitability)
}