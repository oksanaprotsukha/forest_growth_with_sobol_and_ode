#' Calculate forest growth rate using a differential equation
#'
#' @param y - the size of the forest in units of carbon
#' @param parms - as list with three values, r, g, K
#' @param r - an exponential growth rate during a young forest stage before a canopy closure threshold is reached
#' @param g - a linear growth rate after a canopy closure threshold is reached
#' @param K - carrying capacity in units of carbon
#' @param canopy_thresh - canopy closure threshold in units of carbon 
#' @param t - time period for which forest growth is analysed, in years
#'
#' @return derivative of forest growth with time
#' @export
#'
#' @examples

forest_growth <- function(t, y, parms){
  # Current carbon value
  C <- y[1]  
  
  # Extract parameters from the list
  K <- parms$K
  r <- parms$r
  g <- parms$g
  canopy_thresh <- parms$canopy_thresh
  
  if (C < canopy_thresh) {
    forest_dt <- r * C
  } else {
    forest_dt <- g * (1 - C/K)
  }
  
  return(list(forest_dt))
}

