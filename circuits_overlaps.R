# Function to fetch overlap data for a specific racetrack and create a data frame
# Input: 'name' - the name of the racetrack
# Output: A data frame containing overlap data for the racetrack
track_wise <- function(name){
  # Create the URL for the racetrack page
  track <- sprintf('https://www.statsf1.com/en/circuit-%s.aspx', name)
  
  # Create a session and scrape the page
  session <- bow(track)
  cpage <- scrape(session, query=list(t="semi-soft", per_page=100))
  
  # Extract the custom key for sorting the circuits
  cir <- cpage %>% html_nodes('.circuittable') %>% html_nodes('td') %>% html_attr('sorttable_customkey')
  cir <- cir[!is.na(cir)][2]
  
  # Extract circuit links based on the custom key
  circuit_links <- cpage %>% html_nodes(sprintf('td[sorttable_customkey="%s"]', cir)) %>% html_nodes('a') %>% html_attr('href')
  circuit_links <- sub(x = circuit_links, pattern = "\\.aspx$", replacement = '')
  
  # Fetch overlap data for each circuit link
  ov_list <- sapply(X = circuit_links, FUN = safer_overlap)
  
  # Create a data frame with overlap data and add the racetrack name
  circs <- data.frame(ov_list)
  circs <- circs %>% mutate('Circuit' = name)
  
  return(circs)
}


# Function to calculate the total number of overlaps for drivers in a given race
# Input: 'link' - the URL link to the race data
# Output: The total number of overlaps for all drivers in the race
total_overlaps <- function(link){
  suppressMessages({
    url <- sprintf('https://www.statsf1.com%s/tour-par-tour.aspx', link)
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
        a <- integer0_testna(a)
        if (a > 0){
          overlap <- overlap + a
        }
      }
      df <- rbind(df, c(name, overlap))
      overlap <- 0
      a <- 0
    }
    names(df) <- c('Driver', 'Overlaps')
    df[[1]] <- mapvalues(df[[1]], from=names, to=titles[-1])
    total <- sum(as.integer(df$Overlaps))
    return(total)
  })
}

# Functions to wrap other functions with error handling and return 'NA' in case of an error
# 'safer_track_wise' wraps 'track_wise' function, and 'safer_overlap' wraps 'total_overlaps' function.
# Input:
#   - .f: The function to be wrapped with error handling
#   - otherwise: The value to return in case of an error
# Output:
#   - A function that calls the original function with error handling and returns 'NA' on error

safer_track_wise <- possibly(.f = track_wise, otherwise = NA)
safer_overlap <- possibly(.f = total_overlaps, otherwise = NA)
