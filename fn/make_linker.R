#' make_linker
#' 
#' Last modified: 2018-05-03
#' 
make_linker <- function(batch, barlen, side="right"){
  bar_empty <- rep("N", barlen);
  batch_conv <- batch;
  batch_conv[batch_conv=="C"] <- "T";
  batch_revcompconv <- rev(sq_comp(batch)); batch_revcompconv[batch_revcompconv=="C"] <- "T";
  if(side=="right"){
    return(c(batch_conv, bar_empty, batch_revcompconv));
  }else if(side=="left"){
    return(c(batch_revcompconv, bar_empty, batch_conv));
  }
}