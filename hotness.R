hotness <- function(df, ov){
  ggplot(df, aes(fill = Circuit, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
    geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
    geom_rect() + 
    coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
    geom_text(aes(x = 0, y = 0, label = sprintf("%s%% (%s)", (percentage * 100), ov), colour=Circuit), size=15) +
    geom_text(aes(x=1.5, y=1.5, label=Circuit), size=10) + 
    facet_wrap(~Circuit, ncol = 5) +
    theme_void() +
    guides(fill=FALSE) +
    guides(colour=FALSE)  
}
