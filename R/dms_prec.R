#' @title Calculate coordinate precision
#' @description Uses coordinate pairs and assesses the minimum precision of the coordinate pair by converting coordinates to DMS estimates.
#' @param x A two element numeric vector.
#' @return A numeric value.

dms_prec <- function(x) {
  # Estimates precision for coordinates.  Assumes a two element numeric.
  assertthat::assert_that(length(x) == 2 & class(x) == 'numeric')
  
  coords <- sapply(x, dms_conv)
  
  if (any(((floor(coords) - coords) > 1e-10))) {
    # All of the degrees/minutes/seconds don't come out to round numbers.
    return(10^(-prec(x)))
  }
  # We'll test increments of 1, 5, 10, 15.
  index <- c(0, 1, 5, 10, 15)
  
  # returns a six by four matrix.
  for (i in 3:1) {
    modulo <- rbind(coords[i,1] %% c(1, 5, 10, 15, 30, 60),
                    coords[i,2] %% c(1, 5, 10, 15, 30, 60))
    
    if (!all(modulo == 0)) {
      precision <- apply(modulo, 1, function(x) suppressWarnings(min(which(!x < 1e-6))))
      
      values <- suppressWarnings(na.omit(as.numeric(modulo[,precision])))
      
      return(suppressWarnings(min(values[values > 1e-6])) * c(1, 1/60, 1/3600)[i])
      
    } else {
      rounded <- NA
    }
  }
  
  if (is.na(rounded)) {
    estimate <- NA
  }
  
  return(estimate)
  
}
