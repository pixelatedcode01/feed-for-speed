# Load the 'polite' package
library(polite)

# Function to scrape Formula 1 race circuits information for a given year
race_circuits <- function(year){
  # Create a session and force a new request to the URL
  session <- bow(sprintf("https://www.statsf1.com/en/%d.aspx", year), force = TRUE)
  
  # Scrape the webpage for race circuit information
  result <- scrape(session, query=list(t="semi-soft", per_page=100))
  
  # Extract circuit names and links
  circuits <- result %>% html_nodes('.pays') %>% html_text2()
  links <- result %>% html_nodes('.gp') %>% html_node('.flag') %>% html_node('a') %>% html_attr('href')
  
  # Clean up the extracted data
  circuits <- gsub("\\d+\\. ", "", circuits)
  links <- gsub("\\.aspx$", "", links)
  circuits <- gsub("\r| ", "", circuits)
  
  # Create a data frame with circuit names and links
  final <- data.frame(Circuits = circuits, Links = links)
  
  return(final)
}

# Function to scrape Formula 1 driver information for a given year
gfx_race_circuits <- function(year){
  # Create a session and force a new request to the URL
  session <- bow(sprintf("https://www.statsf1.com/en/%d.aspx", year), force = TRUE)
  result <- scrape(session, query=list(t="semi-soft", per_page=100))
  # Scrape the webpage for driver information
  driver_links <- result %>% html_nodes('.libelle') %>% html_node('a') %>% html_attr('href')
  driver_names <- result %>% html_nodes('.libelle') %>% html_node('a') %>% html_text2()
  
  # Create a data frame with driver names and profiles
  driver_info <- data.frame(Driver = driver_names, Profile = driver_links)
  
  return(driver_info)
}

# Function to clean a string by removing digits, slashes, and hyphens
clean <- function(string){
  cleaned_string <- gsub("\\d+|/|-", " ", string)
  cleaned_string <- str_trim(cleaned_string, 'left')
  return (cleaned_string)
}

# Call the 'race_circuits' function with a specific year (e.g., 1950) to get race circuits data
race_circuits(1950)

# Function to fetch Formula 1 race data for a given year from the official Formula 1 website
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

# Function to scrape and return data from a given link
summary <- function(link){
  # Print a message to indicate that data is being fetched
  print(paste('Fetching Table', link))
  url <- link
  page <- read_html(url)
  
  # Scrape the results table and convert it into a data frame
  results_df <- page %>%
    html_nodes(".resultsarchive-table") %>%
    html_table(header = TRUE)
  
  # Remove columns with all NA values
  results_df <- data.frame(results_df)
  results_df <- results_df[ , colSums(is.na(results_df))==0]
  
  # Print a completion message and return the data frame
  print('Fetching Complete. To access your data pass an index to the list. Eg df[[1]]')
  return (results_df)
}

# Function to fetch data from multiple URLs and return a list of data frames
all_dfs <- function(df, year_inp, circuit){
  value <- df$Codes[df$Circuit == as.character(circuit)]
  circuit_list <- c(
    sprintf("https://www.formula1.com/en/results.html/%d/races.html", year_inp),
    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/race-result.html", year_inp, value),
    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/fastest-laps.html", year_inp, value),
    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/pit-stop-summary.html", year_inp, value),
    sprintf("https://www.formula1.com/en/results.html/%d/races/%s/starting-grid.html", year_inp, value))
  
  # Use the 'map' function to apply the 'summary' function to each URL and store the results in 'list_of_df'
  list_of_df <- map(circuit_list, summary)
  return (list_of_df)
}
