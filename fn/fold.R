#' fold
#' 
#' Last modified: 2018-05-03
#' 
fold <- function(sq, linker, barlen, side="right", sqlen=NA){
  ret <- list(
    sq = sq,
    match = FALSE,
    flip = FALSE,
    error = FALSE,
    error_msg = "",
    CpG_sum = 0,
    summary_top = "",
    summary_bot = ""
  );
  
  # find linker sequence in input sequence
  while(TRUE){
    sqMlinker <- sq_match(ret$sq, linker, soft=TRUE);
    if(any(sqMlinker)){
      ret$sqMlinker <- sqMlinker;
      ret$match <- TRUE;
      break;
    }else if(ret$flip==FALSE){
      ret$sq <- rev(sq_comp(ret$sq));
      ret$flip <- TRUE;
    }else{
      ret$error <- TRUE;
      ret$error_msg <- paste0(ret$error_msg, 
                              "ERROR 1: linker sequence not found in input sequence");
      break;
    }
  }
  if(ret$error){
    ret$summary <- paste(c(
      ret$error_msg, "\n"
    ),collapse="");
    return(ret);
  }
  
  # check for duplicate linkers
  if(sum(ret$sqMlinker)/length(linker) != 1){
    ret$error <- TRUE;
    ret$error_msg <- paste0(ret$error_msg, 
                            "ERROR 2: duplicate linker sequences found in input sequence")
  }
  if(ret$error){
    ret$summary <- paste(c(
      ret$error_msg, "\n"
    ),collapse="");
    return(ret);
  }
  
  # define top, bottom and bar sequences
  min <- min(which(ret$sqMlinker));
  max <- max(which(ret$sqMlinker));
  ret$top_unxt <- ret$sq[1:(min-1)];
  ret$bar <- ret$sq[min:max][linker=="N"];
  ret$bot_unxt <- rev(ret$sq[(max+1):length(ret$sq)]);
  
  # make top and bottom sequences the same length
  if(length(ret$top_unxt) != length(ret$bot_unxt)){
    ret$large <- max(length(ret$top_unxt), length(ret$bot_unxt));
    ret$small <- min(length(ret$top_unxt), length(ret$bot_unxt));
    ret$dif <- ret$large-ret$small;
    ret$filler <- rep("-", ret$dif);
    if(length(ret$top_unxt) < length(ret$bot_unxt)){
      ret$top_uncut <- c(ret$filler, ret$top_unxt);
      ret$bot_uncut <- ret$bot_unxt;
    }else{
      ret$bot_uncut <- c(ret$filler, ret$bot_unxt);
      ret$top_uncut <- ret$top_unxt;
    }
  }else{
    ret$top_uncut <- ret$top_unxt;
    ret$bot_uncut <- ret$bot_unxt;
  }
  
  ret$top_unfixed <- ret$top_uncut;
  ret$bot_unfixed <- ret$bot_uncut;

  # fix base errors
  
  # cat(ret$top_unfixed, "\n", ret$bot_unfixed, "\n");
  
  fixed <- compare_fix(ret$top_unfixed, ret$bot_unfixed);
  ret$top = c(fixed$top);
  ret$bot = c(fixed$bot);
  ret$shift_count <- fixed$shift_count;
  ret$excise_count <- fixed$excise_count;
  
  # trim sequence (optional)
  if(!is.na(sqlen)){
    ret$top <- ret$top[(length(ret$top)-sqlen):length(ret$top)];
    ret$bot <- ret$bot[(length(ret$bot)-sqlen):length(ret$bot)];
  }
  
  # flip
  if(side=="left"){
    temp = ret$top_unxt;
    ret$top_unxt = rev(ret$bot_unxt);
    ret$bot_unxt = rev(temp);
    
    temp = ret$top_uncut;
    ret$top_uncut = rev(ret$bot_uncut);
    ret$bot_uncut = rev(temp);
    
    temp = ret$top_unfixed;
    ret$top_unfixed = rev(ret$bot_unfixed);
    ret$bot_unfixed = rev(temp);
    
    temp = ret$top;
    ret$top = rev(ret$bot);
    ret$bot = rev(temp);
  }
  
  # make logical lists corresponding to cpg site methylation state
  topMC <- sq_match(ret$top, "C");
  botMC <- sq_match(ret$bot, "C");
  topMCG <- sq_match(ret$top, c("C", "G"));
  botMGC <- sq_match(ret$bot, c("G", "C"));
  topMTG <- sq_match(ret$top, c("T", "G"));
  botMGT <- sq_match(ret$bot, c("G", "T"));
  
  ret$fullmeth <- sq_match((topMCG & botMGC), c("TRUE", "TRUE"));
  ret$unmeth <- sq_match((topMTG & botMGT), c("TRUE", "TRUE"));
  ret$top_hemimeth <- sq_match((topMCG & botMGT), c("TRUE", "TRUE"));
  ret$top_meth <- sq_match((ret$top_hemimeth | ret$fullmeth), c("TRUE", "TRUE"));
  ret$bot_hemimeth <- sq_match((topMTG & botMGC), c("TRUE", "TRUE"));
  ret$bot_meth <- ret$bot_hemimeth | ret$fullmeth;
  ret$CpG <- ret$top_meth | ret$bot_meth | ret$unmeth;
  
  # identify non-cpg site methylation
  ret$noncpgmeth <- (topMC | botMC) & !ret$CpG;
  ret$noncpgmeth_sum <- sum(ret$noncpgmeth);
  
  # make logical lists of cpg states only
  tmp <- seq(1, sum(ret$CpG), 2);
  ret$CpG_fullmeth <- ret$fullmeth[ret$CpG][tmp];
  ret$CpG_unmeth <- ret$unmeth[ret$CpG][tmp];
  ret$CpG_top_hemimeth <- ret$top_hemimeth[ret$CpG][tmp];
  ret$CpG_top_meth <- ret$top_meth[ret$CpG][tmp];
  ret$CpG_bot_hemimeth <- ret$bot_hemimeth[ret$CpG][tmp];
  ret$CpG_bot_meth <- ret$bot_meth[ret$CpG][tmp];
  
  # reverse convert sequence
  ret$top_revconv <- ret$top;
  ret$top_revconv[topMC] = "C";
  ret$top_revconv[ret$top=="T" & !ret$bot=="A"] = "C";
  ret$bot_revconv <- ret$bot;
  ret$bot_revconv[botMC] = "C";
  ret$bot_revconv[ret$bot=="T" & !ret$top=="A"] = "C";
  
  # calculate statistics
  ret$CpG_sum <- sum(ret$CpG/2);
  ret$U <- sum(ret$CpG_unmeth)/ret$CpG_sum;
  ret$m <- (sum(ret$CpG_top_meth) + sum(ret$CpG_bot_meth))/(ret$CpG_sum*2);
  ret$rcp <- (sqrt(ret$U*(ret$U + 2*ret$m - 1)))/(1 - ret$U - ret$m);
  
  # make summary for text output
  temp_CpG <- rep("|", length(ret$bot));
  temp_CpG[!(ret$top %in% c("G", "A", "T", "C")) | 
             !(ret$bot %in% c("G", "A", "T", "C"))] <- " ";
  temp_CpG[ret$CpG] <- "*";
  temp_topMC <- rep(" ", length(ret$top)); temp_topMC[topMC] <- "m";
  temp_botMC <- rep(" ", length(ret$bot)); temp_botMC[botMC] <- "m";
  temp_top_CpG <- rep("\U25A1", ret$CpG_sum); 
  temp_top_CpG[ret$CpG_top_meth] <- "\U25A0";
  temp_bot_CpG <- rep("\U25A1", ret$CpG_sum); 
  temp_bot_CpG[ret$CpG_bot_meth] <- "\U25A0";
  
  ret$summary_top <- temp_top_CpG;
  ret$summary_bot <- temp_bot_CpG;
  
  ret$summary <- paste(c(
    temp_topMC, "\n",
    #ret$top_unxt, "\n",
    #ret$top_uncut, "\n",
    #ret$top_unfixed, "\n",
    ret$top, "\n",
    #ret$top_revconv, "\n", 
    temp_CpG, "\n",
    #ret$bot_revconv, "\n",
    ret$bot, "\n",
    #ret$bot_unfixed, "\n",
    #ret$bot_uncut, "\n",
    #ret$bot_unxt, "\n",
    temp_botMC, "\n",
    "No. CpG Sites = ", ret$CpG_sum, "\n",
    "(Fully-methylated) = ", sum(ret$CpG_fullmeth), "\n",
    "(Hemi-methylated) = ", sum(ret$CpG_top_hemimeth) + sum(ret$CpG_bot_hemimeth), "\n",
    "(Un-methylated) = ", sum(ret$CpG_unmeth), "\n\n",
    "U: ", round(ret$U, 2), " m: ", round(ret$m, 2), "\n\n",
    "Non-CpG methylation = ", ret$noncpgmeth_sum, "\n",
    "Alignment shifts = ", ret$shift_count, "\n",
    "Unfixable erroneous bases = ", ret$excise_count
  ), collapse="");
  
  return(ret);
}