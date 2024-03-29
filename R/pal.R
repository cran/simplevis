#' D3 palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a non-numeric (or non-ordererd) variable.
#' 
#' @param n The number of colours (excluding an NA colour).
#' 
#' @return A character vector of hex codes.
#' @keywords internal
#' 
pal_d3_mix <- function(n) {
  
  c("#17BECF", "#BCBD22", "#8C564B", "#E377C2", "#1F77B4", "#FF7F0E", "#2CA02C", "#9467BD", "#D62728")[1:n]
}

#' Viridis palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a numeric (or ordererd) variable.
#' 
#' @param n The number of colours (excluding an NA colour).
#' 
#' @return A character vector of hex codes.
#' @keywords internal 
#' 
pal_viridis_mix <- function(n) {
  
  if(n == 1) viridis::viridis(4)[2]
  else if(n == 2) viridis::viridis(4)[c(2, 3)]
  else if(n >= 3) viridis::viridis(n)
}

#' NA palette.
#' 
#' @description A function to retreive a hex code for a colour to use for NA values.
#' 
#' @param pal The hex code or name of the NA colour. Defaults to "#7F7F7FFF".
#'
#' @return A character vector.
#' @keywords internal
#' 
pal_na <- function(pal = "#7F7F7F") {
  return(pal)  
}
