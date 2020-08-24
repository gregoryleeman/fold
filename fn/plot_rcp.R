#' plot_rcp
#' 
#' Last modified: 2018-05-03
#' 
plot_rcp <- function(u, m, rcp){
  rcpinf <- function(U) 1-U;
  rcp8  <- function(U) (1/64)*(-sqrt(64*U - 63*U**2) - 63*U + 64);
  rcp4  <- function(U) (1/16)*(-sqrt(16*U - 15*U**2) - 15*U + 16);
  rcp2  <- function(U) (1/4)*(-sqrt(4*U - 3*U**2) - 3*U + 4);
  rcp1  <- function(U) 1 - sqrt(U);
  rcp12 <- function(U) -2*sqrt(3*U**2 + U) + 3*U + 1;
  rcp14 <- function(U) -4*sqrt(15*U**2 + U) + 15*U + 1;
  rcp18 <- function(U) -8*sqrt(63*U^2 + U) + 63*U + 1;
  rcp0 <- function(U) 0.5-0.5*U;
  plot <- ggplot(data=data_frame(u=u, m=m, rcp=rcp), mapping=aes(x=u, y=m)) +
    stat_function(fun = rcpinf) + annotate("text", x=1/2, y=0.53, label="inf") +
    stat_function(fun = rcp8, linetype="dotted") + annotate("text", x=4/9, y=1/2, label="8") +
    stat_function(fun = rcp4, linetype="dotted") + annotate("text", x=2/5, y=1/2, label="4") +
    stat_function(fun = rcp2, linetype="dotted") + annotate("text", x=1/3, y=1/2, label="2") +
    stat_function(fun = rcp1, linetype="dotted", size=1) + annotate("text", x=1/4, y=0.53, label="1") +
    stat_function(fun = rcp12, linetype="dotted") + annotate("text", x=1/6, y=1/2, label="1/2") +
    stat_function(fun = rcp14, linetype="dotted") + annotate("text", x=1/10, y=1/2, label="1/4") +
    stat_function(fun = rcp18, linetype="dotted") + annotate("text", x=1/18, y=1/2, label="1/8") +
    stat_function(fun = rcp0) + annotate("text", x=0, y=0.49, label="0") +
    geom_point(colour="Red") +
    geom_text(colour="Red", nudge_y = 0.2, aes(label = paste0("RCP: ", round(rcp, 2)))) +
    xlim(0,1) +
    ylim(0,1) +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(colour = "Black"),
      legend.position = "none",
      text = element_text(size=15, family="Courier")
    )
  return(plot);
}