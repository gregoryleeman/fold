#' fold_multi
#' 
#' Last modified: 2018-05-03
#' 
fold_multi <- function(sqs, batch, barlen, side="right", sqlen=NULL){
  ret <- list(
    sqs = sqs,
    batch = batch,
    barlen = barlen,
    side = side,
    sqlen = sqlen,
    sum = 0,
    CpG_sum = 0,
    CpG_max = 0,
    CpG_unmeth_sum = 0,
    CpG_fullmeth_sum = 0,
    CpG_top_meth_sum = 0,
    CpG_bot_meth_sum = 0,
    noncpgmeth_sum = 0,
    Us = c(),
    ms = c(),
    l = c()
  );
  # construct linker
  ret$linker <- make_linker(ret$batch, ret$barlen, ret$side);
  
  # fold each sequence
  for(i in seq(1, length(ret$sqs))){
    
#    message(i);
    
    name <- noquote(paste0("sq", i));
    ret[[name]] <- fold(ret$sqs[[i]], ret$linker, ret$barlen, ret$side, ret$sqlen);
    if(!ret[[name]]$error){
      ret$sum <- ret$sum + 1;
      ret$CpG_sum <- ret$CpG_sum + ret[[name]]$CpG_sum;
      ret$CpG_max <- max(ret$CpG_max, ret[[name]]$CpG_sum);
      ret$CpG_unmeth_sum <- ret$CpG_unmeth_sum + sum(ret[[name]]$CpG_unmeth);
      ret$CpG_fullmeth_sum <- ret$CpG_fullmeth_sum + sum(ret[[name]]$CpG_fullmeth);
      ret$CpG_top_meth_sum <- ret$CpG_top_meth_sum + sum(ret[[name]]$CpG_top_meth);
      ret$CpG_bot_meth_sum <- ret$CpG_bot_meth_sum + sum(ret[[name]]$CpG_bot_meth);
      ret$noncpgmeth_sum <- ret$noncpgmeth_sum + ret[[name]]$noncpgmeth_sum;
      ret$Us <- c(ret$Us, ret[[name]]$U);
      ret$ms <- c(ret$ms, ret[[name]]$m);
      ret$l <- c(ret$l, i);
    }
  }
  
  # calculate rcp
  ret$U <- ret$CpG_unmeth_sum/ret$CpG_sum;
  ret$m <- (ret$CpG_top_meth_sum + ret$CpG_bot_meth_sum)/(ret$CpG_sum*2);
  ret$rcp <- (sqrt(ret$U*(ret$U + 2*ret$m - 1)))/(1 - ret$U - ret$m);
  
  # make summary text output
  ret$summary <- paste(c(
    "Linker: ", ret$linker, " (", side, ")\n",
    "No. Sequences = ", length(ret$sqs), "\n",
    "No. Succesful folds = ", ret$sum, "\n",
    #"Maximum number of CpG sites = ", ret$CpG_max, "\n",
    "U = ", round(ret$U, 5), "\n",
    "m = ", round(ret$m, 5), "\n",
    "RCP = ", round(ret$rcp, 5), "\n"
    #"Non-cpg-site-methylation = ", ret$noncpgmeth_sum
  ),collapse="");
  for(i in seq(1, length(ret$sqs))){
    name <- noquote(paste("sq", i, sep=""));
    g <- ret$CpG_max - ret[[name]]$CpG_sum;
    space <- rep(" ", g);
    if(ret$side=="right"){
      ret$summary <- paste(c(
        ret$summary, "\n",
        space, ret[[name]]$summary_top, " ", i, ": ", ret[[name]]$bar, "\n",
        space, ret[[name]]$summary_bot
      ), collapse = "");
    }else{
      ret$summary <- paste(c(
        ret$summary, "\n",
        ret[[name]]$summary_top, space, " ", i, ": ", ret[[name]]$bar, "\n",
        ret[[name]]$summary_bot, space
      ), collapse = "");
    }
  }
  for(i in seq(1, length(ret$sqs))){
    name <- noquote(paste("sq", i, sep=""));
    ret$summary <- paste(c(
      ret$summary, "\n",
      "=================================================================================================================\n",
      i, ": ", ret[[name]]$bar, "\n",
      ret[[name]]$summary_top, "\n",
      ret[[name]]$summary_bot, "\n",
      ret[[name]]$summary
    ), collapse = "");
  }
#  cat(ret$summary);
  
  return(ret);
}
