#' Water Column Depth Designator (surface or subsurface) for samples
#'
#' This function returns a vector designating whether a sample is a surface or subsurface sample depending a single cutoff depth.
#' @param startDepth, endDepth, cutoff
#' @keywords water column, surface, subsurface
#' @export
#' @examples
#' cat_function()



depthType <- function(startDepth, endDepth, cutoff) {
  
  startDepth[is.na(startDepth)] <- 99999
  endDepth[is.na(endDepth)] <- 99999
  
  ifelse(startDepth != 99999 & endDepth != 99999,
    ifelse(startDepth > cutoff | endDepth > cutoff,"subsurface","surface"),
    ifelse(startDepth == 99999 & endDepth != 99999, 
           ifelse(endDepth > cutoff, "subsurface", "surface"),
           ifelse(startDepth != 99999 & endDepth == 99999, 
                  ifelse(startDepth > cutoff, "subsurface", "surface"),
                  "unknown")))

}  
