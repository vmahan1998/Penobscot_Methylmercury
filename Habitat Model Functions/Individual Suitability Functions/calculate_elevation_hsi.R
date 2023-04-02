calculate_elevation_hsi <- function(elevation_raster, elevation_range, optimal_elevation, slope = 1){
  # elevation_raster: a raster object containing elevation values
  # elevation_range: a vector of two values representing the minimum and maximum elevation tolerable by the species
  # optimal_elevation: the elevation at which the species has the highest fitness
  # slope: a slope parameter controlling the steepness of the HSI curve
  
  # Normalize the elevation values to a scale from 0 to 1
  elevation_norm <- terra::normalize(elevation_raster, type = "div", range = elevation_range)
  
  # Compute the suitability index based on the normalized elevation values
  hsi_elevation <- 1 / (1 + exp(slope * (elevation_norm - optimal_elevation)))
  
  # Set any negative values to 0
  hsi_elevation[hsi_elevation < 0] <- 0
  
  # Return the HSI raster object
  return(hsi_elevation)
}