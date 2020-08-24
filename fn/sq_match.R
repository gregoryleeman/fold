#' sq_match
#' 
#' Last modified: 2018-05-03
#'
sq_match <- function(sq, pattern, soft=FALSE){
  ret <- rep(F, length(sq))
  if(length(pattern) > length(sq)){
    return(FALSE);
  }else{
    for(i in seq(0, (length(sq) - length(pattern)))){
      window <- rep(F, length(pattern));
      for(j in seq(1, length(pattern))){
        if(
          (soft & (pattern[j]=="N"))|
          (pattern[j]==sq[i+j])
        ){
          window[j] <- TRUE;
        }
      }
      if(all(window)){
        ret[(i+1):(i+length(pattern))] <- TRUE;
      }
    }
  }
  return(ret)
}