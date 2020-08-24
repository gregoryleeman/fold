#' compare_check
#' 
#' Last modified: 2018-05-03
#'
compare_check <- function(top, bot){
  ret <- list(
    skip = NA,
    stut = NA,
    error = NA,
    error_sum = 0
  );
  
  topMT <- sq_match(top, "T");
  botMT <- sq_match(bot, "T");
  topMA <- sq_match(top, "A");
  botMA <- sq_match(bot, "A");
  topMC <- sq_match(top, "C");
  botMC <- sq_match(bot, "C");
  topMG <- sq_match(top, "G");
  botMG <- sq_match(bot, "G");
  topMT <- sq_match(top, "T");
  botMT <- sq_match(bot, "T");
  
  ret$skip <- 
    topMA & botMA | 
    topMG & botMG | 
    topMA & botMG | 
    topMG & botMA | 
    topMA & botMC | 
    topMC & botMA;
  
  ret$stut <- topMT & botMT;
  
  ret$error <- ret$skip | ret$stut;
  ret$error_sum <- sum(ret$error);
  return(ret);
}