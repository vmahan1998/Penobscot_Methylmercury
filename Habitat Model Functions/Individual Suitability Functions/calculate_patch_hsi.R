calculate_patch_hsi <- function(depth_raster, salinity_raster, velocity_raster, sediment_raster, temperature_raster, turbidity_raster, elevation_raster, 
                          depth_weight = 0.25, salinity_weight = 0.2, velocity_weight = 0.1, sediment_weight = 0.15, temperature_weight = 0.1, 
                          turbidity_weight = 0.1, elevation_weight = 0.1) {
  
  # Normalize raster data
  depth_norm <- (depth_raster - min(depth_raster)) / (max(depth_raster) - min(depth_raster))
  salinity_norm <- (salinity_raster - min(salinity_raster)) / (max(salinity_raster) - min(salinity_raster))
  velocity_norm <- (velocity_raster - min(velocity_raster)) / (max(velocity_raster) - min(velocity_raster))
  sediment_norm <- (sediment_raster - min(sediment_raster)) / (max(sediment_raster) - min(sediment_raster))
  temperature_norm <- (temperature_raster - min(temperature_raster)) / (max(temperature_raster) - min(temperature_raster))
  turbidity_norm <- (turbidity_raster - min(turbidity_raster)) / (max(turbidity_raster) - min(turbidity_raster))
  elevation_norm <- (elevation_raster - min(elevation_raster)) / (max(elevation_raster) - min(elevation_raster))
  
  # Calculate habitat suitability index for each parameter
  depth_hsi <- depth_norm * depth_weight
  salinity_hsi <- salinity_norm * salinity_weight
  velocity_hsi <- velocity_norm * velocity_weight
  sediment_hsi <- sediment_norm * sediment_weight
  temperature_hsi <- temperature_norm * temperature_weight
  turbidity_hsi <- turbidity_norm * turbidity_weight
  elevation_hsi <- elevation_norm * elevation_weight
  
  # Combine the habitat suitability indices into a single index
  combined_hsi <- depth_hsi + salinity_hsi + velocity_hsi + sediment_hsi + temperature_hsi + turbidity_hsi + elevation_hsi
  
  # Return the final habitat suitability index as a raster object
  return(combined_hsi)
}