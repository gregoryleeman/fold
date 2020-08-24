#' sq_comp
#' 
#' Last modified: 2018-05-03
#' 
sq_comp <- function(sq){
  ret <- character(length(sq));
  ret[sq=="A"] <- "T";
  ret[sq=="T"] <- "A";
  ret[sq=="G"] <- "C";
  ret[sq=="C"] <- "G";
  ret[sq=="N"] <- "N";
  return(ret); 
}