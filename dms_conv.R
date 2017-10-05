#' @title Simple conversion of decimal degree systems to DMS coordinate systems.
#' @param x The decimal degree values of the coordinates.
#' @description This is used for the assessment of coordinate precision.
#' @return A numeric DMS vector of length 3.

dms_conv <- function(x) {
  
  assertthat::is.number(x)
  
  # Takes decimal degrees and converts them to dms coordinates.
  
  if (x < 0) { w <- TRUE }
  
  x <- abs(x)
  
  d <- x - (x %% 1)
  m <- ((x - d) * 60) - (((x - d) * 60) %% 1)
  s <- (x - (d + m/60)) * 3600
  
  c(ifelse(w, d, -d), m, ifelse(s < 0.5, 0, s))
}
