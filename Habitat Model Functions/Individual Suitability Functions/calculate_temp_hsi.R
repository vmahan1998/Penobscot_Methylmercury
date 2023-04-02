calculate_temp_hsi <- function(temp_raster, min_temp, max_temp) {
  # Calculate the range of temperatures within the min-max bounds
  temp_range <- clamp(temp_raster, min_temp, max_temp)
  # Calculate the difference between the raster and the optimal temperature
  temp_diff <- abs(temp_range - ((min_temp + max_temp) / 2))
  # Calculate the HSI using a logistic function
  hsi <- 1 / (1 + exp(-0.5 * (temp_diff - ((max_temp - min_temp) / 4))))
  return(hsi)
}