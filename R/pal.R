#' D3 palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a non-numeric (or non-ordererd) variable.
#' 
#' @param n The number of colours (excluding an NA colour).
#' 
#' @return A character vector of hex codes.
#' @export
#' @examples 
#' scales::show_col(pal_d3_reorder(9)) 
pal_d3_reorder <- function(n) {
  
  c("#17BECFFF", "#BCBD22FF", "#8C564BFF", "#E377C2FF", "#1F77B4FF", "#2CA02CFF", "#FF7F0EFF", "#9467BDFF", "#D62728FF")[1:n]
}

#' Viridis palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a numeric (or ordererd) variable.
#' 
#' @param n The number of colours (excluding an NA colour).
#' 
#' @return A character vector of hex codes.
#' @export 
#' @examples 
#' scales::show_col(pal_viridis_reorder(9)) 
pal_viridis_reorder <- function(n) {
  
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
#' @export 
#' @examples 
#' scales::show_col(pal_na()) 
pal_na <- function(pal = "#7F7F7FFF") {
  return(pal)  
}