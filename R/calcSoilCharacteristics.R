#' @import madrat
#' @importFrom dplyr left_join


calcSoilCharacteristics <- function() {
  x = readSource("LPJmL_inputs", subtype = "LPJmL_inputs:basic.soil")
  
  soil_char <-
    toolGetMapping(name = "mappingSoil.csv", type = "sectoral")
  
  w = array(
    NA,
    dim = c(dim(x)[1], dim(soil_char[,-1])[2], dim(x)[2]),
    dimnames = c(dimnames(x)[1], dimnames(soil_char[,-1])[2], dimnames(x)[2])
  )
  for (i in dimnames(x)[[2]]) {
    g           <- as.data.frame(x@.Data[, i,])
    colnames(g) <- "soil"
    g           <- left_join(g, soil_char, by = "soil", keep = TRUE)
    g           <- data.matrix(g[,-1])
    w[, , i]    <- g
    
  }
  
  w <- as.magpie(w)
  return(w)
}
