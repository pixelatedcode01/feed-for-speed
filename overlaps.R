#define function to catch integer(0)
integer0_test0 <- function(data) {
  
  if(identical(data, integer(0))) {
    return(0)
  }
  
  else {
    return(data)
  }
  
}

integer0_testna <- function(data) {
  
  if(identical(data, integer(0))) {
    return(NA)
  }
  
  else {
    return(data)
  }
  
}

total_overlaps <- function(df, id){
  suppressMessages({
     url <- sprintf('https://www.statsf1.com/%s/tour-par-tour.aspx', df$Links[df$Circuits == id])
     url <- bow(url)
     page <- scrape(url, query=list(t="semi-soft", per_page=100))

    scores_df_header <- page %>% html_node('.datatable') %>% html_table(header = FALSE)
    scores_df <- page %>% html_node('.datatable') %>% html_table(header = FALSE)
    names <- page %>% html_node('.datatable') %>% html_nodes('a') %>% html_text()
    names <- names[-1]
    
    namespos <- as.character(as.vector(scores_df[1, ]))
    namespos <- namespos[-1]
    
    titles <- page %>% html_node('.datatable') %>% html_nodes('a') %>% html_attr('title')
    titles <- titles[-1]
    titles <- c("Laps", titles)
    
    laps <- (scores_df_header %>% nrow()) - 1
    
    names(scores_df_header) <- titles
    
    for (i in 2:length(titles)){
      scores_df_header[[i]] <- mapvalues(scores_df_header[[i]], from = namespos, to = names)
    }
    
    scores_df_header <- scores_df_header[-1]
    
    overlap <- 0
    df <- data.frame()
    for (name in names){
      for (i in 1:laps){
        a <- which(scores_df_header[i, ] == name) - which(scores_df_header[i+1, ] == name)
        a <- integer0_test0(a)
        if (a > 0){
          overlap <- overlap + a
        }
      }
      df <- rbind(df, c(name, overlap))
      overlap <- 0
      a <- 0
    }
    df <- cbind(df, Id = names)
    names(df) <- c('Driver', 'Overlaps', 'DrID')
    df[[1]] <- mapvalues(df[[1]], from=names, to=titles[-1])
    return(df)
  })
}


mod_total_overlaps <- function(df, id, vector){
  suppressMessages({
     url <- sprintf('https://www.statsf1.com/%s/tour-par-tour.aspx', df$Links[df$Circuits == id])
     url <- bow(url)
     page <- scrape(url, query=list(t="semi-soft", per_page=100))
    
    scores_df_header <- page %>% html_node('.datatable') %>% html_table(header = FALSE)
    scores_df <- page %>% html_node('.datatable') %>% html_table(header = FALSE)
    names <- page %>% html_node('.datatable') %>% html_nodes('a') %>% html_text()
    names <- names[-1]
    
    namespos <- as.character(as.vector(scores_df[1, ]))
    namespos <- namespos[-1]
    
    titles <- page %>% html_node('.datatable') %>% html_nodes('a') %>% html_attr('title')
    titles <- titles[-1]
    titles <- c("Laps", titles)
    
    laps <- (scores_df_header %>% nrow()) - 1
    
    names(scores_df_header) <- titles
    
    for (i in 2:length(titles)){
      scores_df_header[[i]] <- mapvalues(scores_df_header[[i]], from = namespos, to = names)
    }
    
    scores_df_header <- scores_df_header[-1]
    names_pos <- c(1:length(names))
    did <- 0
    got <- 0
    vector_number <- mapvalues(vector, from = names, to = names_pos)
    
    vector_number <- sort(vector_number)
    vector <- mapvalues(vector_number, from = names_pos, to = names)
    for (i in 1:laps){
      previous_lap <- which(scores_df_header[i, ] == vector[1]) - which(scores_df_header[i, ] == vector[2])
      next_lap <- which(scores_df_header[i + 1, ] == vector[1]) - which(scores_df_header[i + 1, ] == vector[2])
      previous_lap  <- integer0_test0(previous_lap)
      next_lap <- integer0_test0(next_lap)
      if (previous_lap < 0 & next_lap > 0) {
        did <- did + 1  
      }
      else if (previous_lap > 0 && next_lap < 0) {
        got <- got + 1
      }
    }
    return(c(got, did))
    #print(sprintf('%s overtakes %s %d time(s)', vector[1], vector[2], got))
    #print(sprintf('%s overtakes %s %d time(s)', vector[2], vector[1], did))
  })
}

get_images <- function(link){
  url <- sprintf('https://www.statsf1.com/en%s', link)
  req <- bow(url)
  page <- scrape(req, query=list(t="semi-soft", per_page=100))
  p1 <- page %>% html_node('#ctl00_CPH_Main_IMG_Photo') %>% html_attr('src')
  link <- sprintf('https://www.statsf1.com%s', p1)
  return(link)
}


plot_overlapping <- function(df, d_one, d_two){
  plot <- ggplot(df, aes(x = Laps)) +
    geom_line(aes(y = .data[[d_one]], colour="D1"), linewidth = 1.5) +
    geom_line(aes(y = .data[[d_two]], colour="D2"), linewidth = 1.5) +
    labs(x = "Laps", y = "Driver Position", fill = "Drivers") +
    scale_y_continuous(breaks = c(df[[d_one]], df[[d_two]])) + 
    scale_color_manual(name = "Drivers", values = c("D1" = "blue", "D2" = "red"), labels = c(d_one, d_two)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5, size = 12),
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12), 
          plot.title = element_text(size = 20, color = "black", face = "bold"),
          axis.title = element_text(size = 15))
  return(plot)
}


modded_race_positions <- function(df, id){
  url <- sprintf('https://www.statsf1.com/%s/tour-par-tour.aspx', df$Links[df$Circuits == id])
  url <- bow(url)
  page <- scrape(url, query=list(t="semi-soft", per_page=100))
  scores_df_header <- page %>% html_node('.datatable') %>% html_table(header = TRUE)
  names <- page %>% html_node('.datatable') %>% html_nodes('a') %>% html_text()
  names <- names[-1]
  scores_df <- page %>% html_node('.datatable') %>% html_table(header = FALSE)
  titles <- page %>% html_node('.datatable') %>% html_nodes('a') %>% html_attr('title')
  titles <- titles[-1]
  titles <- c("Laps", titles)
  names_pos <- 1:length(names)
  namespos <- as.character(as.vector(scores_df[1, ]))
  namespos <- namespos[-1]
  for (item in namespos){
    scores_df_header[[item]] <- mapvalues(scores_df_header[[item]], from=names, to=names_pos)
  }
  laps <- scores_df_header %>% nrow
  players <- 1:length(scores_df_header)-1
  players <- players[-1]
  table_without_laps <- scores_df_header[-1]
  table_without_laps <- rbind(players, table_without_laps)
  
  race_graph_df <- data.frame(laps = c(1:laps))
  race_graph_df <- rbind(0, race_graph_df)
  
  pos <- c()
  for (i in 1:length(players)){
    for (j in 1:(laps+1)){
      current_pos <- which(table_without_laps[j, ] == i)
      current_pos <- integer0_testna(current_pos)
      pos <- append(pos, current_pos)
    }
    varname <- paste("driver", i , sep=".")
    race_graph_df[[varname]] <- with(race_graph_df, pos)
    pos <- c()
  }
  
  cropped <- race_graph_df[-1]
  cropped <- cropped[-1, ]
  scores <- c(length(players):1)
  pos_value <- c(1:length(players))
  
  for (i in 1:length(players)){
    cropped[[i]] <- mapvalues(cropped[[i]], from = pos_value, to = scores)
  }
  
  for (i in 1:laps){
    cropped[i, ] <- cropped[i, ] * i
  }
  
  cropped <- rbind(c(1:length(players)), cropped)
  cropped <- cbind(laps = c(0:laps), cropped)
  names(cropped) <- titles
  cropped_long <- cropped %>% gather(key = Drivers, value = Points, titles[2]:titles[length(titles)])
  names(race_graph_df) <- titles
  return(race_graph_df)
}
