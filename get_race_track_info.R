# Function to get latitude for a given track name
get_lat <- function(track_name){
  Sys.sleep(1)  # Pause for 1 second to avoid overloading the server
  url <- sprintf('https://www.statsf1.com/en/circuit-%s.aspx', track_name)
  page <- read_html(url)  # Read the HTML page
  link <- page %>% html_node("#ctl00_CPH_Main_HL_Google") %>% html_attr('href')  # Extract the Google Maps link
  parsed <- url_parse(link)
  cords <- strsplit(parsed$domain, ',') %>% unlist  # Split and extract the latitude and longitude
  return(as.numeric(cords[1]))  # Convert latitude to a numeric value
}

# Function to get longitude for a given track name
get_long <- function(track_name){
  Sys.sleep(1)  # Pause for 1 second to avoid overloading the server
  url <- sprintf('https://www.statsf1.com/en/circuit-%s.aspx', track_name)
  page <- read_html(url)  # Read the HTML page
  link <- page %>% html_node("#ctl00_CPH_Main_HL_Google") %>% html_attr('href')  # Extract the Google Maps link
  parsed <- url_parse(link)
  cords <- strsplit(parsed$domain, ',') %>% unlist  # Split and extract the latitude and longitude
  return(as.numeric(cords[2]))  # Convert longitude to a numeric value
}

# Function to scrape race track data for a given year
race_tracks <- function(year_val){
  safer_get_lat <- possibly(get_lat, otherwise = NA, quiet = FALSE)  # Wrap get_lat in error handling
  safer_get_long <- possibly(get_long, otherwise = NA, quiet = FALSE)  # Wrap get_long in error handling
  url <- sprintf('https://gpracingstats.com/seasons/%d-world-championship/', year_val)
  page <- read_html(url)  # Read the HTML page
  pos <- page %>% html_node('.summary') %>% html_table(header = TRUE)  # Extract the summary table
  pos$Circuit <- gsub(' ', '-', pos$Circuit)  # Replace spaces in circuit names
  
  # List of manual corrections for circuit names
  pos$Circuit[pos$Circuit == 'Jeddah'] = 'Djeddah'
  pos$Circuit[pos$Circuit == 'Florida'] = 'Miami'
  pos$Circuit[pos$Circuit == 'Marina-Bay'] = 'Singapour'
  pos$Circuit[pos$Circuit == 'Monte-Carlo'] = 'Monaco'
  pos$Circuit[pos$Circuit == 'Catalunya'] = 'Barcelone'
  pos$Circuit[pos$Circuit == 'Baku'] = 'Bakou'
  pos$Circuit[pos$Circuit == 'Sepang'] = 'Kuala-Lumpur'
  pos$Circuit[pos$Circuit == 'Casablanca'] = 'Ain-Diab'
  pos$Circuit[pos$Circuit == 'Mosport'] = 'Mosport-Park'
  pos$Circuit[pos$Circuit == 'Buddh'] = 'New-Delhi'
  
  # Add latitude and longitude columns to the data using safer_get_lat and safer_get_long
  pos <- pos %>% mutate('lat' = sapply(pos$Circuit, safer_get_lat), 
                        'long' = sapply(pos$Circuit, safer_get_long))
  
  # Write the data to a CSV file named after the year
  write_csv(pos, sprintf('race_tracks_%d.csv', year_val))
}

# Uncomment and run the code below, providing a year from 1950 to 2023 to test the race_tracks function
# race_tracks(year)
