track_wise <- function(name){
  track <- sprintf('https://www.statsf1.com/en/circuit-%s.aspx', name)
  session <- bow(track)
  cpage <- scrape(session, query=list(t="semi-soft", per_page=100))
  cir <- cpage %>% html_nodes('.circuittable') %>% html_nodes('td') %>% html_attr('sorttable_customkey')
  cir <- cir[!is.na(cir)][2]
  circuit_links <- cpage %>% html_nodes(sprintf('td[sorttable_customkey="%s"]', cir)) %>% html_nodes('a') %>% html_attr('href')
  circuit_links <- sub(x = circuit_links, pattern = "\\.aspx$", replacement = '')
  ov_list <- sapply(X = circuit_links, FUN = safer_overlap)
  circs <- data.frame(ov_list)
  circs <- circs %>% mutate('Circuit' = name)
  return(circs)
}

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

safer_track_wise <- possibly(.f = track_wise, otherwise = NA)
safer_overlap <- possibly(.f = total_overlaps, otherwise = NA)