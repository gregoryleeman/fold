#' str2chr
#' 
#' Last modified: 2018-05-03
#' 
str2chr <- function(nstr){
  return(substring(nstr, seq(1,nchar(nstr),1), seq(1,nchar(nstr),1)));
}