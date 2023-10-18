#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#if (!require(shiny)) install.packages("shiny")
#if (!require(tidyverse))install.packages("tidyverse")
#if (!require(rvest))install.packages("rvest")
#if (!require(plyr))install.packages("plyr")
#if (!require(dplyr))install.packages("dplyr")
#if (!require(ggplot2))install.packages("ggplot2")
#if (!require(gganimate))install.packages("gganimate")
#if (!require(sjmisc))install.packages("sjmisc")
#if (!require(gt)) install.packages("gt")
#if (!require(gtExtras))install.packages("gtExtras")
#if (!require(magick))install.packages("magick")
#if (!require(leaflet))install.packages("leaflet")
#if (!require(sf))install.packages("sf")
#if (!require(stringi))install.packages("stringi")
#if (!require(geojsonio))install.packages("geojsonio")
#if (!require(shiny.router))install.packages("shiny.router")
#if (!require(shinydashboard))install.packages("shinydashboard")
#if (!require(datasets))install.packages("datasets")
#if (!require(bslib))install.packages("bslib")




library(shiny)
library(tidyverse)
library(rvest)
library(plyr)
library(dplyr)
library(ggplot2)
library(gganimate)
library(sjmisc)
library(gt)
library(gtExtras)
library(magick)
library(leaflet)
library(sf)
library(stringi)
library(geojsonio)
library(shiny.router)
library(shinydashboard)
library(datasets)
library(bslib)
source("positions.R")
source("race_locations.R")
source("overlaps.R")
source("relations.R")
source("hotness.R")


cir_ov <- read_csv("final_circuit_wise_overlap.csv")
year_ov <- read_csv("final_year_wise_overlap.csv")


ui <- fluidPage(
  tags$head(tags$link(href="style.css", rel="stylesheet", type="text/css")),
  tabsetPanel(id = "panels",
              tabPanel("Feed for Speed",
                       tags$div(class="background",
                                tags$div(class="main_image", tags$div(class="logo-white"), tags$div(class="para", tags$p(class="white-text", "Welcome to Feed for Speed, where our mission is straightforward: our deep passion for Formula 1 drives us to harness the power of data for a deeper understanding. Our scope extends beyond current races, encompassing every Grand Prix from 1950 to the present. We meticulously assess driver performances, delve into team strategies, and analyze each racetrack. At Feed for Speed, our ultimate aim is to offer you enlightening insights, enabling you to appreciate the world of Formula 1, both its rich history and present-day excitement."))),
                                tags$div(class="footer", 
                                         tags$div(class="one", tags$h3(class="divtext", "LIVE RACING SIMULATION")),
                                         tags$div(class="two", tags$h3(class="divtext", "FASTEST LAP SUMMARY")),
                                         tags$div(class="three", tags$h3(class="divtext", "PITSTOP ANALYSIS")),
                                         tags$div(class="four", tags$h3(class="divtext", "OVERLAP ANALYSIS")),
                                         tags$div(class="five", tags$h3(class="divtext", "AND MUCH MORE..."))
                                         )
                                )
                       ),
              tabPanel("Select Race",
                       tags$div(class="container",
                                tags$div(class="first", tags$img(class="logo", src="f1-logo-final.png")),
                                tags$div(class="second", tags$div(class="year", selectInput(inputId = 'year_val', '', c("Select Year", 2023:1950)))),
                                tags$div(class="third", leafletOutput("mymap", height = 750)))),
              tabPanel("Live Racing",
                       tags$div(class="container",
                                tags$div(class="first", tags$img(class="logo", src="f1-logo-final.png")),
                                tags$div(class="second", 
                                         tags$div(class="year", selectInput(inputId = 'year_inp', '', c("Select Year", 2023:1950))),
                                         tags$div(class="year button", actionButton("goButton", "Submit")),
                                         tags$div(class="year", selectInput(inputId = 'track', '', c("Select Circuit", NULL))),
                                         tags$div(class="circuit button", actionButton("raceButton", "Start", class="btn-success"))
                                         ),
                                tags$div(class="third", imageOutput("racechart", height = 750)))),
              tabPanel("Overlapping Zone",
                       tags$div(class="container",
                                tags$div(class="first", tags$img(class="logo", src="f1-logo-final.png")),
                                tags$div(class="second", 
                                         tags$div(class="year", selectInput(inputId = 'overlapyear', '', c("Select Year", 2023:1950))),
                                         tags$div(class="year button", actionButton("goYear", "Submit")),
                                         tags$div(class="year", selectInput(inputId = 'overlaptrack', '', c("Select Circuit", NULL))),
                                         tags$div(class="circuit button", actionButton("goCircuit", "Submit", class="btn-success"))
                                ),
                                tags$div(class="forth", 
                                         tags$div(class="table", gt_output(outputId = "table1")),
                                         tags$div(class="head", tags$div(class="second", 
                                                                         tags$div(class="year", selectInput(inputId = 'driver_one', '', c("Select Driver", NULL))),
                                                                         tags$div(class="year", selectInput(inputId = 'driver_two', '', c("Select Driver", NULL))),
                                                                         tags$div(class="circuit button", actionButton("godrivertwo", "Submit"))
                                         ),
                                         tags$div(class="text", uiOutput(outputId = "headtohead")),
                                         tags$div(class="pimage1",uiOutput(outputId = "image")),
                                         tags$div(class="pimage2", uiOutput(outputId = "image2")),
                                         tags$br(), tags$br(),
                                         tags$h1(class="text", textOutput('textmain')),
                                         tags$h3(class="text", textOutput('text1')),
                                         tags$h3(class="text", textOutput('text2'))
                                         )
                                         ),
                                uiOutput(outputId = 'overlapplothead'), plotOutput(outputId = 'overlapplot'))),
                                                                  tabPanel("Race Hotness Meter", tags$div(class="container",
                                                                                            tags$div(class="first", tags$img(class="logo", src="f1-logo-final.png")),
                                                                                            tags$div(class="second", 
                                                                                                     tags$div(class="year", selectInput(inputId = 'country_input', '', c("Select Country", cir_ov$Country))),
                                                                                                     tags$div(class="year button", actionButton("goCountry", "Submit")),
                                                                                                     tags$div(class="circuit", selectInput(inputId = 'track_inp', '', c("Select Circuit", NULL))),
                                                                                                     tags$div(class="circuit button", actionButton("goTrack", "Submit")),
                                                                                                     tags$div(class="circuit", selectInput(inputId = 'year_inp1', '', c("Select Year", NULL))),
                                                                                                     tags$div(class="circuit button", actionButton("goYearHotness", "Submit", class="btn-success")))),
                                                             tags$div(plotOutput(outputId = "overallhotness", height = 600)), uiOutput(outputId = "stats")), 
              tabPanel("Insights", navbarPage(title = "Insights", 
                                                    tabPanel("Introduction", 
                                                             tags$h1("Feed For Speed"),
                                                             tags$div(class="year", selectInput(inputId = 'main', 'Select Year', c("Select Year", 2023:1950))),
                                                             tags$br(),
                                                             tags$div(class="", actionButton("goMainYear", "Submit")),
                                                             tags$br(),
                                                             tags$div(class="year", selectInput(inputId = 'maincircuit', 'Select Circuit', c("Select Circuit", NULL))),
                                                             tags$br(),
                                                             tags$div(class="", actionButton("goMain", "Submit", class="btn-success"))),
                                                    tabPanel("Better vs Faster", uiOutput(outputId = "heading1"), plotOutput("race_points"),
                                                             uiOutput(outputId = "heading2"), plotOutput("fastest")),
                                                  #  tabPanel("Race Results", sidebarLayout(
                                                  #    mainPanel(gt_output(outputId = "raceresults")),
                                                  #    sidebarPanel("Race Results")
                                                  #  )),
                                                    tabPanel("Pitstop Analysis", sidebarLayout(
                                                      mainPanel(gt_output(outputId = "pitstop")),
                                                      sidebarPanel("Pitstop Analysis: This table offers a detailed breakdown of each driver's pitstop count, pitstop durations, and the specific laps on which these pitstops occurred. This data serves as a crucial tool for pinpointing the drivers who made the most efficient pitstops and those who lagged behind, wasting valuable time.")
                                                    )),
                                                    tabPanel("Race Analysis", sidebarLayout(
                                                      mainPanel(gt_output(outputId = "overall")),
                                                      sidebarPanel("Overall Comparison: This table offers a comprehensive race overview, encompassing key elements such as total pitstop time, initial grid placement, analysis of the fastest lap, and the ultimate race position. It enables a detailed examination of these factors, helping to rationalize the final race standings for each driver.")
                                                    )),
                                                    tabPanel("Position Gained or Lost", sidebarLayout(
                                                      mainPanel(gt_output(outputId = "pvs")),
                                                      sidebarPanel("Start vs Finish: This table offers a detailed account of how many positions each driver gained or lost throughout the race. The higher the numbers, the more intense the race, showcasing a greater number of overtaking maneuvers and changes in position, adding an extra layer of excitement and drama to the event.")
                                                    )),

                                                      ))))

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    if (input$year_val != "Select Year"){
      race_tracks <- read_csv(sprintf('./race_tracks/race_tracks_%s.csv', input$year_val), show_col_types = FALSE)
      
      track_popup <- paste0("<strong>Grand Prix: </strong>", 
                            race_tracks$`Grand Prix`, 
                            "<br><strong>Circuit: </strong>", 
                            race_tracks$Circuit,
                            "<br><strong>Date: </strong>",
                            race_tracks$Date
      )
      
      car.icon <- makeIcon("./f1-car.png")
      leaflet(options = leafletOptions(minZoom = 2.5, attributionControl = FALSE)) %>%
        setMaxBounds(lng1 = -180 , lat1 = -65, lng2 = 180, lat2 = 90) %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to Level 1",
          onClick=JS("function(btn, map){ map.setZoom(2.5); }"))) %>%
        addMarkers(lng = race_tracks$long, lat = race_tracks$lat, popup = track_popup) %>%
        addControl(sprintf('Grand Prix: %s', input$year_val), position = 'topright') %>%
        addProviderTiles(providers$OpenStreetMap.DE, options = providerTileOptions(noWrap = FALSE))
    }

    })
  
  
  
  observeEvent(input$goYear, {
    ov_year <- input$overlapyear
    cirs_df <- race_circuits(as.numeric(ov_year))
    updateSelectInput(session = session, inputId = "overlaptrack", choices = c("Select Circuit", cirs_df$Circuits))
  })
  
  plots.dfs <- eventReactive(input$goCircuit, {
    ov_year <- input$overlapyear
    cirs_df <- race_circuits(as.numeric(ov_year))
    circuit <- input$overlaptrack
    ov_df <- total_overlaps(cirs_df, id = circuit)
    return(ov_df)
  })
  
  observeEvent(input$goCircuit, {
    df <- plots.dfs()
    ove_df <- df %>% gt() %>% gt_theme_538() %>% tab_header(title = sprintf("Overlaps Summary"),subtitle = "Overlaps per driver")
    output$table1 <- render_gt(expr = ove_df)
  })
  
  
#  output$table1 <- render_gt({
#    df <- plots.dfs()
#    ove_df <- df %>% gt() %>% gt_theme_538() %>% tab_header(title = sprintf("Overlaps Summary"),subtitle = "Overlaps per driver")
#    output$table1 <- render_gt(expr = ove_df)
#  })
  
#  output$table1 <- renderTable({
#    plots.dfs()
#  }, striped = TRUE, hover = TRUE, align = 'c', width = 500, caption = "Total Overlaps for the race per driver.")
  
  observeEvent(input$goCircuit, {
    ov_df <- plots.dfs()
    updateSelectInput(session = session, inputId = "driver_one", choices = c("Select Driver", ov_df$DrID))
    updateSelectInput(session = session, inputId = "driver_two", choices = c("Select Driver", ov_df$DrID))
  })
  
  plots.ovdf <- eventReactive(input$godrivertwo, {
      d_one <- input$driver_one
      d_two <- input$driver_two
      ov_year <- input$overlapyear
      cirs_df <- race_circuits(as.numeric(ov_year))
      circuit <- input$overlaptrack
      total <- plots.dfs()
      d_one_full <- total$Driver[total$DrID == d_one]
      d_two_full <- total$Driver[total$DrID == d_two]
      info <- gfx_race_circuits(as.numeric(ov_year))
      info$Driver <- gsub(x = info$Driver, pattern = ' ', replacement = '')
      dlink_one <- info$Profile[info$Driver == d_one_full]
      dlink_two <- info$Profile[info$Driver == d_two_full]
      headtohead_df <- mod_total_overlaps(df = cirs_df, id = circuit, vector = c(d_one, d_two))
    return(c(d_one, d_two, headtohead_df, dlink_one, dlink_two, d_one_full, d_two_full))
  })
  # For last section

  
  observeEvent(input$goMainYear, {
    inp_year <- as.numeric(input$main)
    circuit_main_df <- race(year = inp_year)
    updateSelectInput(session = session, inputId = "maincircuit", choices = c("Select Circuit", circuit_main_df$Circuit))
  })
  observeEvent(input$goMain, {
    inp_year <- as.numeric(input$main)
    circuit_main_df <- race(year = inp_year)
    inp_circuit <- input$maincircuit
    all_relations <- all_dfs(df = circuit_main_df, year_inp = inp_year, circuit = inp_circuit)
    raceresults_df <- all_relations[[1]]
    raceresults_df <- raceresults_df %>% gt() %>% gt_theme_538() %>% tab_header(title = sprintf("Race Results for %d", inp_year),subtitle = "Race Results")
    #output$raceresults <- render_gt(expr = raceresults_df)
    win_loss <- start_grid_summary(results_df = all_relations[[2]], start_grid_df = all_relations[[5]])
    win_loss <- win_loss %>% gt() %>% gt_theme_538() %>% tab_header(title = sprintf("Positions Gained or Lost for %s, %d", inp_circuit, inp_year),subtitle = "Starting Grid comparison") %>% data_color(columns = `Position+G/-L`, palette = c('red', 'green'))
    output$pvs <- render_gt(expr = win_loss)
    pitstops_df <- pitstop_analysis(results_df = all_relations[[2]], pitstops_df = all_relations[[4]])
    pitstops_df <- pitstops_df %>% gt() %>% gt_theme_538() %>% tab_header(title = sprintf("Pitstop Analysis for %s, %d", inp_circuit, inp_year),subtitle = "Driver Pitstop Count")
    output$pitstop <- render_gt(expr = pitstops_df)
    overall_df <- overall_summary(pitstop_df = all_relations[[4]], starting_grid_df = all_relations[[5]], fastest_lap_df = all_relations[[3]], results_df = all_relations[[2]])
    overall_df <- overall_df %>% gt() %>% gt_theme_538() %>% tab_header(title = sprintf("Race Analysis for %s, %d", inp_circuit, inp_year),subtitle = "Overall Summary") %>% cols_align(align = "center")
    output$overall <- render_gt(expr = overall_df)
    plot_summary_race <- plot_summary(dataframe = all_relations[[2]], inp_circuit)
    output$heading1 <- renderUI({
      tags$h3(sprintf("Race Summary by driver points for %s, %d", inp_circuit, inp_year))
    })
    output$race_points <- renderPlot(plot_summary_race)
    plot_fastest <- fastest_lap_summary(all_relations[[3]])
    output$heading2 <- renderUI({
      tags$h3(sprintf("Fastest Lap Summary by driver points for %s, %d", inp_circuit, inp_year))
    })
    output$fastest <- renderPlot(plot_fastest)
  })

  
  
  # - ####################### - #
  # For Race Hotness Meter
  
  
  observeEvent(input$goCountry, {
    cir_ov <- read_csv("final_circuit_wise_overlap.csv")
    cir_country <- input$country_input
    updateSelectInput(session = session, inputId = "track_inp", choices = c("Select Circuit", cir_ov$Circuit[cir_ov$Country == cir_country]))
  })
  
  observeEvent(input$goTrack, {
    cir_year_ov <- read_csv("final_year_wise_overlap.csv")
    cir_track <- input$track_inp
    updateSelectInput(session = session, inputId = "year_inp1", choices = c("Select Year", sort(cir_year_ov$Year[cir_year_ov$Circuit == cir_track])))
  })
  
  observeEvent(input$goYearHotness, {
    cir_year_ov <- as.numeric(input$year_inp1)
    cir_track <- input$track_inp
    mini <- cir_ov$Min[cir_ov$Circuit == cir_track]
    maxi <- cir_ov$Max[cir_ov$Circuit == cir_track]
    countraces <- as.numeric(cir_ov$Count[cir_ov$Circuit == cir_track])
    differ <- maxi - mini
    cr_overlaps <- year_ov$Overlaps[year_ov$Circuit == cir_track & year_ov$Year == cir_year_ov]
    inp_df <- cir_ov[cir_ov$Circuit == cir_track, ]
    inp_df <- inp_df %>% mutate(percentage = round(((cr_overlaps - mini) / differ), 2))
    hot_plot <- hotness(inp_df, cr_overlaps)
    output$overallhotness <- renderPlot(hot_plot)
    outstat <- sprintf('Total Races: %d - Miniumum Overlaps: %d - Maximum Overlaps: %d', countraces, mini, maxi)
    output$stats <- renderUI({
      tags$h4(outstat[1])
    })
  })
  

  
  # ######################## #
  
  
  

    
  observeEvent(input$godrivertwo, {
    infor <- plots.ovdf()
    overlapyear1 <- input$overlapyear
    overlaping_df1 <- race_circuits(as.numeric(overlapyear1))
    overlapcircuit1 <- input$overlaptrack
    
    output$text1 <- renderText({
      values <- plots.ovdf()
      first <- paste(sprintf("%s overtook %s: %s times", values[7], values[8], values[3]))
      return(first)
    })
    output$text2 <- renderText({
      values <- plots.ovdf()
      second <- paste(sprintf("%s overtook %s: %s time(s)", values[8], values[7], values[4]))
      return(second)
    })
    output$textmain <- renderText({
      values <- plots.ovdf()
      res <- paste(sprintf('%s vs %s', values[3], values[4]))
      return(res)
    })
    
    output$headtohead <- renderUI({
      tags$h3("Head to Head Result")
    })
    
    output$image <- renderUI({
      infor <- plots.ovdf()
      p1 <- get_images(link = infor[5])
      tags$img(src = p1)
    })
    
    output$image2 <- renderUI({
      infor <- plots.ovdf()
      p2 <- get_images(link = infor[6])
      tags$img(src = p2)
    })
    
    output$overlapplothead <- renderUI({
      infor <- plots.ovdf()
      tags$h3(sprintf("Overlapping Plot for %s and %s", infor[7], infor[8]))
    })
    
    
    finalfinal_df <- modded_race_positions(df = overlaping_df1, id = overlapcircuit1)
    ploto <- plot_overlapping(df = finalfinal_df, d_one = infor[7], d_two = infor[8])
    output$overlapplot <- renderPlot(expr = ploto)
  })
    
    
  observeEvent(input$goButton, {
    race_year <- input$year_inp
    final <- race_circuits(as.numeric(race_year))
    updateSelectInput(session = session, inputId = "track", choices = c("Select Circuit", final$Circuits))
  })
  
  observeEvent(input$raceButton, {
    race_year <- input$year_inp
    final <- race_circuits(as.numeric(race_year))
    circuit <- input$track
    merged <- race_positions(final, circuit)
    #plot_race(merged, circuit)
    plot <- merged %>% 
      ggplot(aes(x = as.numeric(Poisition), y = Points, fill = Drivers)) +
      geom_col(alpha = 0.7) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      geom_text(aes(y = Points
                    , label = paste(Drivers)
                    , color = Drivers)
                , hjust = - 0.2, size = 5) +
      labs(title = sprintf('Race for %s', circuit), x = "", y = 'Lap: {closest_state}', caption = "Race") +
      coord_flip() +
      scale_x_reverse() +
      theme_minimal() +
      theme(axis.text.y = element_blank()
            , axis.ticks.y = element_blank()
            , axis.ticks.x = element_blank()
            , axis.text.x = element_blank()
            , plot.margin = margin(2, 2, 2, 2, "cm")
            , plot.title = element_text(size = 20, color = "black", face = "bold")
            , axis.title = element_text(size=30,face="bold", color="grey")
            , legend.position = "none"
            , panel.grid.minor.x = element_blank()
            , panel.grid.major.x = element_blank()) +
      transition_states(Laps, state_length = 0, wrap = FALSE)
    
    anim_save(filename = "racing.gif", animation = plot, start_pause = 30, end_pause = 30, height = 750, width = 1800, nframes = 200, fps = 20)
    
    output$racechart <- renderImage({
      list(src = "racing.gif", contentType = "image/gif")
    },deleteFile = FALSE)
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)