calculate_salinity_hsi <- function(salinity_raster, salinity_range, optimal_salinity){
  # salinity_raster: a raster object containing salinity values
  # salinity_range: a vector of two values representing the minimum and maximum salinity tolerable by the species
  # optimal_salinity: the salinity value at which the species has the highest fitness
  
  # Normalize the salinity values to a scale from 0 to 1
  salinity_norm <- normalize(salinity_raster, type = "div", range = salinity_range)
  
  # Compute the suitability index based on the normalized salinity values
  hsi_salinity <- exp(-0.5 * ((salinity_norm - optimal_salinity) / 0.1)^2)
  
  # Set any negative values to 0
  hsi_salinity[hsi_salinity < 0] <- 0
  
  # Return the HSI raster object
  return(hsi_salinity)
}