start_grid_summary <- function(results_df, start_grid_df){
  # Returns left joined data frame for comparison between starting and final position.
  summary_grid <- results_df %>%
    # Selecting All columns from results_italy table except Driver and Car to avoid duplicacy.
    select(-Driver, -Car) %>%
    # Doing a left join on start_grid table
    left_join(start_grid_df, by = "No") %>%
    dplyr::rename("Race.Position" = Pos.x, "Starting.Position" = Pos.y)
  summary_grid['Race.Position'] <- suppressWarnings(as.numeric(summary_grid$Race.Position))
  
  # Mutating Position Column
  summary_grid <- summary_grid %>%
    mutate('Position+G/-L' = Starting.Position - Race.Position)
  
  # Display Flattened out df to the output
  # Return Summary Grid df
  return(summary_grid)
}

# Making a new function that take results data and pitstops data
pitstop_analysis <- function(results_df, pitstops_df){
  # Joining results and pitstops data with a full_join by Driver name primary key
  pitstop_analyis_df <- results_df %>% full_join(pitstops_df, by= 'Driver') %>%
    # Selecting particular rows from the joined table.
    select(Pos, PTS, No.x, Driver, Car.x, Stops, Lap, Time, Total) %>%
    # Renaming 
    dplyr::rename("Car" = Car.x, "Driver.No" = No.x)
  
  # Display Flattened out df to the output
  return (pitstop_analyis_df)
}

overall_summary <- function(pitstop_df, starting_grid_df, fastest_lap_df, results_df){
  n <- pitstop_df %>%
    # Grouping Pitstops driver-wise
    group_by(Driver, No) %>%
    dplyr::summarise(No.of.Pitstops = max(Stops), Time = suppressWarnings(sum(as.numeric(Time)))) %>%
    dplyr::rename('Total.Pitstop.Time(S)' = Time)
  test <- starting_grid_df %>%
    #Selecting All columns from results_italy table except Driver and Car to avoid duplicacy.
    select(-Driver, -Time) %>%
    #Doing a left join on pitstops and results table
    left_join(n, by = "No") %>%
    left_join(results_df, by = "No") %>%
    left_join(fastest_lap_df, by = "No") %>%
    select(-Driver.x, -Driver.y, -Car.x, -Car.y, -Pos.y) %>%
    dplyr::rename('Starting.Position' = Pos.x, 'Race.Position' = Pos, 'Fastest.Lap.Time' = Time, 'Fastest.Lap' = Lap)
  
  # Lets sort the table with lowest pitstops time.
  sorted_pitstop_time <- test[order(test$`Total.Pitstop.Time(S)`, na.last = TRUE), ]
  rownames(sorted_pitstop_time) <- NULL
  
  # Return Sorted pitstop df
  return (sorted_pitstop_time)
}

# Define a function that takes a dataframe and circuit name as inputs
plot_summary <- function(dataframe, circuit_name){
  
  # Create a new column 'DriverSubstring' by extracting the first 15 characters from the 'Driver' column
  dataframe$DriverSubstring <- substr(dataframe$Driver, start = 1, stop = 15)
  
  dataframe <- dataframe[order(-dataframe$PTS, na.last = TRUE), ]
  # Create a ggplot object 'f' for visualizing the data
  f <- ggplot(dataframe, aes(x = fct_inorder(DriverSubstring), y = PTS, fill = Car)) + 
  
  # Customize the appearance of the plot, including rotating x-axis labels, setting axis labels, and specifying the title
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5, size = 12),
        axis.text.y = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 20, color = "black", face = "bold"),
        axis.title = element_text(size = 15)) +
  labs(x = 'Drivers', y = 'Driver Points', title = sprintf('Race Summary of %s', circuit_name))
  
  # Print the plot with grouped columns representing driver points
  return(f + geom_col())
}

fastest_lap_summary <- function(dataframe){
  
  # Create a new column 'DriverSubstring' by extracting the first 15 characters from the 'Driver' column
  dataframe$DriverSubstring <- substr(dataframe$Driver, start = 1, stop = 15)
  
  # Sorting the dataframe by Time in ascending order
  dataframe <- dataframe[order(dataframe$Time, na.last = TRUE), ]
  # Create a ggplot object 'f' for visualizing the data
  f <- ggplot(dataframe, aes(x = fct_inorder(DriverSubstring), y = Time, fill = Car)) + geom_bar(stat = "identity") + 
    
    # Customize the appearance of the plot, including rotating x-axis labels, setting axis labels, and specifying the title
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5, size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12), 
          plot.title = element_text(size = 20, color = "black", face = "bold"),
          axis.title = element_text(size = 15)) +
    labs(x = 'Drivers', y = 'Lap Time', title = sprintf('Fastest Lap time'))
  
  # Print the plot with grouped columns representing driver points
  return(f + geom_col())
}