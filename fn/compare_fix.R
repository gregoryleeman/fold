#' compare_fix
#' 
#' Last modified: 2018-05-03
#'
compare_fix <- function(top, bot){
  ret = list(
    top = top,
    bot = bot,
    shift_count = 0,
    excise_count = 0
  );
  while(TRUE){
    compare <- compare_check(ret$top, ret$bot);  # check for errors
    if(compare$error_sum != 0){
      temp <- rep(" ", length(ret$top));
      temp[compare$error] <- "X";
      
      message("errors:", compare$error_sum, "\n", top, "\n", bot, "\n", temp, "\n");
      
      s <- max(which(compare$error));  # first error
      
      # try excising erroneous bases
      top_excise = ret$top;
      top_excise[s] = tolower(top_excise[s]);
      bot_excise = ret$bot;
      bot_excise[s] = tolower(bot_excise[s]);
      compare_excise = compare_check(top_excise, bot_excise);
      
      if(s != length(top)){
        # try fixing a slippage
        topMTT <- sq_match(top, c("T", "T"));
        botMTT <- sq_match(bot, c("T", "T"));
        top_shift <- ret$top;
        bot_shift <- ret$bot;
        if(compare$stut[s]){
          if(topMTT[s+1]){
            top_shift <- ret$top[-(s+1)];
            bot_shift <- ret$bot[-1];
          }else if(botMTT[s+1]){
            top_shift <- ret$top[-1];
            bot_shift <- ret$bot[-(s+1)];
          }else{
            top_shift[s] <- tolower(top_shift[s]);
            bot_shift[s] <- tolower(bot_shift[s]);
          }
        }else if(compare$skip[s]){
          if(topMTT[s+1]){
            top_shift <- c(ret$top[1:s], "T", ret$top[(s+1):(length(ret$top))]);
            bot_shift <- c("-", ret$bot);
          }else if(botMTT[s+1]){
            top_shift <- c("-", ret$top);
            bot_shift <- c(ret$bot[1:s], "T", ret$bot[(s+1):(length(ret$bot))]);
          }else{
            top_shift[s] <- tolower(top_shift[s]);
            bot_shift[s] <- tolower(bot_shift[s]);
          }
        }
        compare_shift = compare_check(top_shift, bot_shift);
        # use method that results in the downstream errors
        if(compare_excise$error_sum > compare_shift$error_sum){
          ret$top <- top_shift;
          ret$bot <- bot_shift;
          ret$shift_count <- ret$shift_count + 1;
        }else{
          ret$top <- top_excise;
          ret$bot <- bot_excise;
          ret$excise_count <- ret$excise_count + 1;
        }
      }else{
        ret$top <- top_excise;
        ret$bot <- bot_excise;
        ret$excise_count <- ret$excise_count + 1;
      }
    }else{
      break
    }
  }
  return(ret);
}