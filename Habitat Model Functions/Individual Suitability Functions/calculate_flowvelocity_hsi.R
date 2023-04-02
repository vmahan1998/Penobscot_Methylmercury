calculate_flowvelocity_hsi <- function(flow_raster, min_flow_range, optimal_flow){
  # flow_raster: a raster object containing flow velocity values
  # min_flow_range: a vector of two values representing the minimum and maximum flow velocity tolerable by the species
  # optimal_flow: the flow velocity at which the species has the highest fitness
  
  # Normalize the flow velocity values to a scale from 0 to 1
  flow_norm <- terra::normalize(flow_raster, type = "div", range = min_flow_range)
  
  # Compute the suitability index based on the normalized flow velocity values
  hsi_min_flow_velocity <- exp(-0.5 * ((flow_norm - optimal_flow) / 0.1)^2)
  
  # Set any negative values to 0
  hsi_min_flow_velocity[hsi_min_flow_velocity < 0] <- 0
  
  # Return the HSI raster object
  return(hsi_min_flow_velocity)
}