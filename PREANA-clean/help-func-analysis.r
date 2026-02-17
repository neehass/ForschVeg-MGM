# help-function analysis

# ---- calculate max slope of Tempprofiles by Area group ------------------------
func_smooth <- function(Temps, Depths, span) {
    n <- length(Temps)
    safe_span <- min(span, max(3 / n, 0.3))  # Ensure at least 3 points or 30%
 
    loess_fit <- loess(Temps ~ Depths, span = span)
    smooth_temps <- predict(loess_fit)
    return(smooth_temps)}

func_max_slope <- function(Temps, Depths, span) {

  # smooth the temperature data using loess
  smooth_temps <- func_smooth(Temps, Depths, span)

  dT <- lead(smooth_temps) - smooth_temps
  dz <- lead(Depths) - Depths
  gradient <- dT / dz 
  # Find index of minimum absolute gradient
  abs_gradient <- abs(round(gradient, 2))
  max_g <- max(abs_gradient, na.rm = TRUE)
  
  # Find all indices where the absolute gradient equals the minimum
  max_indices <- which(abs_gradient == max_g)
  
  # Get corresponding Temps and Depths
  maxT <- min(round(Temps[max_indices]))
  maxZ <- min(Depths[max_indices])

  return(list(max_slope = max_g, maxT = maxT,
              maxZ = maxZ, smooth_temps = smooth_temps,
              gradient = gradient))
}

# ---- finde WEndepunkt, 2 Ableitung -----------------------------------
# This function finds the inflection point in a temperature profile
func_wendepunkt <- function(Temp, Depth, span) {
  # Ensure Depth is sorted
  ord <- order(Depth)
  Temp <- Temp[ord]
  Depth <- Depth[ord]

  # Smooth the temperature profile
  smooth_temps <- func_smooth(Temp, Depth, span)
  
  # First derivative (slope)
  dT <- lead(smooth_temps) - smooth_temps
  dz <- lead(Depth) - Depth
  slope <- dT / dz
  
  # Second derivative (change in slope)
  d_slope <- lead(slope) - slope
  dz2 <- lead(Depth) - Depth
  ch_slope <- d_slope / dz2
  
  # finde punkte an dem die Steigung wechselt (+-, -+)
  sign_change <- which(diff(sign(ch_slope)) != 0)
  
  if (length(sign_change) == 0) {
    return(list(inflectionZ = NA, inflectionT = NA))
  }
  
  # Choose the first inflection point (or refine logic as needed)
  idx <- sign_change[1]
  inflectionZ <- Depth[idx]
  inflectionT <- smooth_temps[idx]
  
  return(list(inflectionZ = inflectionZ, inflectionT = inflectionT))

}
