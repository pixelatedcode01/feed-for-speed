# Load the 'rvest' and 'gganimate' packages
library(rvest)
library(gganimate)

# Function to scrape and process race positions data for a given circuit
race_positions <- function(df, id) {
  # Construct the URL for the race positions data based on the circuit ID
  url <- sprintf("https://www.statsf1.com/%s/tour-par-tour.aspx", df$Links[df$Circuits == id])
  
  # Create a session and scrape the webpage for race positions data
  url <- bow(url)
  page <- scrape(url, query = list(t = "semi-soft", per_page = 100))
  
  # Extract header and data for race positions
  scores_df_header <- page %>%
    html_node(".datatable") %>%
    html_table(header = TRUE)
  names <- page %>%
    html_node(".datatable") %>%
    html_nodes("a") %>%
    html_text()
  names <- names[-1]
  scores_df <- page %>%
    html_node(".datatable") %>%
    html_table(header = FALSE)
  titles <- page %>%
    html_node(".datatable") %>%
    html_nodes("a") %>%
    html_attr("title")
  titles <- titles[-1]
  titles <- c("Laps", titles)
  names_pos <- 1:length(names)
  namespos <- as.character(as.vector(scores_df[1, ]))
  namespos <- namespos[-1]
  
  # Reorganize the data to create a table of race positions
  for (item in namespos) {
    scores_df_header[[item]] <- mapvalues(scores_df_header[[item]], from = names, to = names_pos)
  }
  laps <- scores_df_header %>% nrow()
  players <- 1:length(scores_df_header) - 1
  players <- players[-1]
  table_without_laps <- scores_df_header[-1]
  table_without_laps <- rbind(players, table_without_laps)
  
  # Create a data frame for generating the race position graph
  race_graph_df <- data.frame(laps = c(1:laps))
  race_graph_df <- rbind(0, race_graph_df)
  pos <- c()
  
  # Extract and organize position data for each driver
  for (i in 1:length(players)) {
    for (j in 1:(laps + 1)) {
      current_pos <- which(table_without_laps[j, ] == i)
      current_pos <- integer0_test(current_pos)
      pos <- append(pos, current_pos)
    }
    varname <- paste("driver", i, sep = ".")
    race_graph_df[[varname]] <- with(race_graph_df, pos)
    pos <- c()
  }
  
  # Remove unnecessary rows and columns
  cropped <- race_graph_df[-1]
  cropped <- cropped[-1, ]
  
  # Map position values to scores and laps
  scores <- c(length(players):1)
  pos_value <- c(1:length(players))
  for (i in 1:length(players)) {
    cropped[[i]] <- mapvalues(cropped[[i]], from = pos_value, to = scores)
  }
  for (i in 1:laps) {
    cropped[i, ] <- cropped[i, ] * i
  }
  
  # Add column names and create a long-format data frame
  cropped <- rbind(c(1:length(players)), cropped)
  cropped <- cbind(laps = c(0:laps), cropped)
  names(cropped) <- titles
  cropped_long <- cropped %>% gather(key = Drivers, value = Points, titles[2]:titles[length(titles)])
  names(race_graph_df) <- titles
  pos_table <- race_graph_df %>% gather(key = Drivers, value = Poisition, titles[2]:titles[length(titles)])
  
  # Merge the long-format data frames
  merged_main <- merge(cropped_long, pos_table)
  
  return(merged_main)
}

# Function to create and save a bar chart race animation
plot_race <- function(data_frame, circuit) {
  # Create a basic bar chart plot
  plot <- data_frame %>%
    ggplot(aes(x = as.numeric(Poisition), y = Points, fill = Drivers)) +
    geom_col(alpha = 0.7) +
    labs(title = "Bar chart race: {closest_state}", x = "", y = "") +
    coord_flip() +
    scale_x_reverse() +
    theme_minimal()
  
  # Create an animation by transitioning through race laps
  anim <- plot +
    transition_states(laps, state_length = 0, wrap = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
  
  # Save the animation as a GIF file
  anim_save(sprintf("%s.gif", circuit), animate(anim))
}
