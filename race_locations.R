library(polite)
race_circuits <- function(year){
  session <- bow(sprintf("https://www.statsf1.com/en/%d.aspx", year), force = TRUE)
  result <- scrape(session, query=list(t="semi-soft", per_page=100))
  circuits <- result %>% html_nodes('.pays') %>% html_text2()
  links <- result %>% html_nodes('.gp') %>% html_node('.flag') %>% html_node('a') %>% html_attr('href')
  circuits <- gsub("\\d+\\. ", "", circuits)
  links <- gsub("\\.aspx$", "", links)
  circuits <- gsub("\r| ", "", circuits)
  final <- data.frame(Circuits = circuits, Links = links)
  
  return(final)
}

gfx_race_circuits <- function(year){
  session <- bow(sprintf("https://www.statsf1.com/en/%d.aspx", year), force = TRUE)
  result <- scrape(session, query=list(t="semi-soft", per_page=100))
  driver_links <- result %>% html_nodes('.libelle') %>% html_node('a') %>% html_attr('href')
  driver_names <- result %>% html_nodes('.libelle') %>% html_node('a') %>% html_text2()
  driver_info <- data.frame(Driver = driver_names, Profile = driver_links)

  return(driver_info)
}


clean <- function(string){
  cleaned_string <- gsub("\\d+|/|-", " ", string)
  cleaned_string <- str_trim(cleaned_string, 'left')
  return (cleaned_string)
}

race_circuits(1950)

# From official F1
race <- function(year){
  
  # Create a URL based on the user-provided year to fetch Formula 1 race data
  url <- sprintf("https://www.formula1.com/en/results.html/%d/races.html", year)
  
  # Read the HTML content of the webpage
  page <- read_html(url)
  
  # Extract the circuit names and their corresponding codes from the webpage
  circuits <- page %>% html_nodes('a[data-name="meetingKey"]') %>% html_nodes('.clip') %>% html_text()
  codes <- page %>% html_nodes('a[data-name="meetingKey"]') %>% html_attr('data-value')
  
  # Create a data frame with circuit names and their codes
  df <- data.frame(Circuit = circuits[2: length(circuits)],
                   Codes = codes[2: length(codes)])
  
  return(df)
}


summary <- function(link){
  # Scraps and return data from the link supplied as parameter
  print(paste('Fetching Table', link))
  url <- link
  page <- read_html(url)
  results_df <- page %>%
    #Class name for the results table : .resultsarchive-table
    html_nodes(".resultsarchive-table") %>% 
    html_table(header = TRUE)
  results_df <- data.frame(results_df)
  results_df <- results_df[ , colSums(is.na(results_df))==0]
  print('Fetching Complete. To access your data pass index to the list. Eg df[[1]]')
  return (results_df)
  
}

all_dfs <- function(df, year_inp, circuit){
  value <- df$Codes[df$Circuit == as.character(circuit)]
  circuit_list <- c(sprintf("https://www.formula1.com/en/results.html/%d/races.html", year_inp),
                    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/race-result.html", year_inp, value),
                    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/fastest-laps.html", year_inp, value),
                    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/pit-stop-summary.html", year_inp, value),
                    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/starting-grid.html", year_inp, value))
  
  # Use the 'map' function to apply the 'circuit_summary' function to each URL and store the results in 'list_of_df'
  list_of_df <- map(circuit_list, summary)
  return (list_of_df)
}
