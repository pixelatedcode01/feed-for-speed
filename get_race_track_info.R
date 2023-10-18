get_lat <- function(track_name){
  Sys.sleep(1)
  url <- sprintf('https://www.statsf1.com/en/circuit-%s.aspx', track_name)
  page <- read_html(url)
  link <- page %>% html_node("#ctl00_CPH_Main_HL_Google") %>% html_attr('href')
  parsed <- url_parse(link)
  cords <- strsplit(parsed$domain, ',') %>% unlist
  
  return(as.numeric(cords[1]))
}

get_long <- function(track_name){
  Sys.sleep(1)
  url <- sprintf('https://www.statsf1.com/en/circuit-%s.aspx', track_name)
  page <- read_html(url)
  link <- page %>% html_node("#ctl00_CPH_Main_HL_Google") %>% html_attr('href')
  parsed <- url_parse(link)
  cords <- strsplit(parsed$domain, ',') %>% unlist
  
  return(as.numeric(cords[2]))
}

race_tracks <- function(year_val){
  safer_get_lat <- possibly(get_lat, otherwise = NA, quiet = FALSE)
  safer_get_long <- possibly(get_long, otherwise = NA, quiet = FALSE)
  url <- sprintf('https://gpracingstats.com/seasons/%d-world-championship/', year_val)
  page <- read_html(url)
  pos <- page %>% html_node('.summary') %>% html_table(header = TRUE)
  pos$Circuit <- gsub(' ', '-', pos$Circuit)
  
  # List to be updated manually.
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
  
  pos <- pos %>% mutate('lat' = sapply(pos$Circuit, safer_get_lat), 
                        'long' = sapply(pos$Circuit, safer_get_long))
  
  write_csv(pos, sprintf('race_tracks_%d.csv', year_val))
}

# Un comment the code below and enter year from 1950 to 2023 to test
# race_tracks(year)
