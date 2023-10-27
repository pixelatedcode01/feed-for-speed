hotness <- function(df, ov){
  # Create a polar bar chart using ggplot2
  ggplot(df, aes(fill = Circuit, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
    # Add a background rectangle with a light yellow color
    geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
    # Add rectangles representing data points
    geom_rect() + 
    # Set the coordinate system to polar
    coord_polar(theta = "y", start = -pi/2) +
    # Set the x-axis and y-axis limits
    xlim(c(0, 2)) + ylim(c(0, 2)) +
    # Add text labels inside the polar chart
    geom_text(aes(x = 0, y = 0, label = sprintf("%s%% (%s)", (percentage * 100), ov), colour = Circuit), size = 15) +
    # Add text labels at the center of the chart
    geom_text(aes(x = 1.5, y = 1.5, label = Circuit), size = 10) + 
    # Create multiple facets (subplots) based on the Circuit variable
    facet_wrap(~Circuit, ncol = 5) +
    # Remove all the axis labels and ticks
    theme_void() +
    # Hide the legend for fill
    guides(fill = FALSE) +
    # Hide the legend for color
    guides(colour = FALSE)  
}