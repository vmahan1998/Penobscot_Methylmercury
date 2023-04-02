calculate_depth_hsi <- function(depth_raster, depth_range, optimal_depth){
  # depth_raster: a raster object containing depth values
  # depth_range: a vector of two values representing the minimum and maximum depth tolerable by the species
  # optimal_depth: the depth at which the species has the highest fitness
  
  # Normalize the depth values to a scale from 0 to 1
  depth_norm <- terra::normalize(depth_raster, type = "div", range = depth_range)
  
  # Compute the suitability index based on the normalized depth values
  hsi_depth <- exp(-0.5 * ((depth_norm - optimal_depth) / 0.1)^2)
  
  # Set any negative values to 0
  hsi_depth[hsi_depth < 0] <- 0
  
  # Return the HSI raster object
  return(hsi_depth)
}