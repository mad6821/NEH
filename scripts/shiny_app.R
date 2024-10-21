## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] shiny_app.R
## [ AUTH ] Maya Dalton (mdalton@neh.gov) Benjamin Skinner (bskinner@neh.gov)
## [ INIT ] August 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "readxl", "sf", "maps", "leaflet", "shiny", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")
tab_dir <- file.path(root, "tables")

# Load libraries
## library(plotly)
## library(formattable)
## library(vtable)
## library(scales)
## library(htmltools)

## -------------------------------------
## macros
## -------------------------------------

## award period
award_period <- 2018:2023

## appalachian state abbreviations
app_st <- c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN",
            "VA", "WV")

## counties in Appalachian region
df_arc <- read_excel(list.files(file.path(dat_dir, "arc"), full.names = TRUE),
                     skip = 4) |>
  rename_all(tolower) |>
  left_join(county.fips |>
              mutate(fips = sprintf("%05d", fips)),
            by = "fips") |>
  rename(county_name = polyname)

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

df <- read_csv(file.path(dat_dir, "analysis.csv"),
               show_col_types = FALSE)

## -----------------------------------------------------------------------------
## map data
## -----------------------------------------------------------------------------

## states
map_app_st <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) |>
  rename_all(tolower) |>
  filter(id %in% pull(distinct(df_arc, state = tolower(state), state)))

## counties
map_app_ct <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) |>
  rename_all(tolower) |>
  filter(id %in% pull(distinct(df_arc, county_name = tolower(county_name),
                               county_name))) |>
  separate(id, c("state", "county"), sep = ",") |>
  mutate(state = str_to_title(state))

## counties <- counties %>% # Fix county anomalies
##   mutate(county = case_when(
##     county == "de kalb" ~ "dekalb",
##     county == "st clair" ~ "st. clair",
##     .default = as.character(county)
##   ),
##   state = str_to_title(state)) # Capitalize states for cleaning

###############################################
# Cleaning counties to Appalachia
###############################################

## # Subsetting econ data by state, then re-binding together to avoid counties with the same
## # name in different states overlapping one another (i.e., Jefferson Co. in AL, OH, and WV)

## appstates <- c("Alabama", "Georgia")

## out_list <- purrr::map(appstates,
##                        ~ subset(app_econ_df, State == .x) |>
##                          subset(counties, county %in% tolower(.data$County) & state == .x))

## econ_list <- vector("list", length(appstates))
## counties_list <- vector("list", length(appstates))
## for(i in appstates) {
##   tmp_st <- subset(app_econ_df, State == i)
##   counties_list[[i]] <- subset(counties, county %in% tolower(tmp_state[[i]]$County) & state == i)
##   econ_list[[i]] <- tmp_state
## }

## # Alabama
## econ_al <- subset(app_econ_df, State == "Alabama")
## counties_al <- subset(counties, county %in% tolower(econ_al$County) & state == "Alabama")

## # Georgia
## econ_ga <- subset(app_econ_df, State == "Georgia")
## counties_ga <- subset(counties, county %in% tolower(econ_ga$County) & state == "Georgia")

## # Kentucky
## econ_ky <- subset(app_econ_df, State == "Kentucky")
## counties_ky <- subset(counties, county %in% tolower(econ_ky$County) & state == "Kentucky")

## # Maryland
## econ_md <- subset(app_econ_df, State == "Maryland")
## counties_md <- subset(counties, county %in% tolower(econ_md$County) & state == "Maryland")

## # Mississippi
## econ_ms <- subset(app_econ_df, State == "Mississippi")
## counties_ms <- subset(counties, county %in% tolower(econ_ms$County) & state == "Mississippi")

## # New York
## econ_ny <- subset(app_econ_df, State == "New York")
## counties_ny <- subset(counties, county %in% tolower(econ_ny$County) & state == "New York")

## # North Carolina
## econ_nc <- subset(app_econ_df, State == "North Carolina")
## counties_nc <- subset(counties, county %in% tolower(econ_nc$County) & state == "North Carolina")

## # Ohio
## econ_oh <- subset(app_econ_df, State == "Ohio")
## counties_oh <- subset(counties, county %in% tolower(econ_oh$County) & state == "Ohio")

## # Pennsylvania
## econ_pa <- subset(app_econ_df, State == "Pennsylvania")
## counties_pa <- subset(counties, county %in% tolower(econ_pa$County) & state == "Pennsylvania")

## # South Carolina
## econ_sc <- subset(app_econ_df, State == "South Carolina")
## counties_sc <- subset(counties, county %in% tolower(econ_sc$County) & state == "South Carolina")

## # Tennessee
## econ_tn <- subset(app_econ_df, State == "Tennessee")
## counties_tn <- subset(counties, county %in% tolower(econ_tn$County) & state == "Tennessee")

## # Virginia
## econ_va <- subset(app_econ_df, State == "Virginia")
## counties_va <- subset(counties, county %in% tolower(econ_va$County) & state == "Virginia")

## # West Virginia
## econ_wv <- subset(app_econ_df, State == "West Virginia")
## counties_wv <- subset(counties, county %in% tolower(econ_wv$County) & state == "West Virginia")

## counties_app <- rbind(counties_al, counties_ga, counties_ky, counties_md, counties_ms, counties_nc,
##                       counties_ny, counties_oh, counties_pa, counties_sc, counties_tn, counties_va, counties_wv)

## df_clean$county <- tolower(df_clean$County) # Lowercase county in NEH/Econ merge
## df_clean$state <- df_clean$State # Lowercase state in NEH/Econ merge

## app_merge <- left_join(counties_app, df_clean, by = c("state"="State", "county"))

###############################################
# Additional cleaning and merging
###############################################

# Creating new award variable to account for those that received only matching, etc. 
neh_data <- app_merge %>%
  group_by(county) %>%
  mutate(total_award = ifelse(OriginalAmount == 0, sum(ApprovedOutright, na.rm=T), sum(OriginalAmount, na.rm=T))) 

neh_data <- neh_data %>%
  group_by(county, YearAwarded) %>%
  mutate(annual_award = ifelse(OriginalAmount == 0, sum(ApprovedOutright, na.rm=T), sum(OriginalAmount, na.rm=T))) 


app_econ_df$County <- str_to_lower(app_econ_df$County)
econ_df <- left_join(app_econ_df, counties, by=c("County"="county", "State"="state")) 
econ_sf <- st_as_sf(econ_df)
###############################################
#
# Define UI
#
###############################################

ui <- fluidPage(
  titlePanel("NEH Funding in the Appalachian Region"),
  sidebarLayout(
    sidebarPanel(
      helpText("Welcome to a comprehensive dashboard of NEH funding in the Appalachian region. 
               To explore, please use the drop down menus below to select a state and year."),
      selectInput("state", "Select a State:",  # STATE DROPDOWN MENU
                  choices = c("All", unique(app_merge$state)),
                  selected = "All", width = "200px"),
      selectInput("year", "Select a Year:",    # YEAR DROPDOWN MENU
                  choices = c("All", sort(unique(df_clean$YearAwarded))),
                  selected = "All", width = "200px")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview of Awards Granted", 
                 fluidRow(    # COUNTIES MAP
                   column(width = 8,
                          h4("Appalachian Regional Map"), 
                          leafletOutput("countyMap1")
                   )
                 ),
                 fluidRow(   # AWARDS STATS
                   column(width = 12,
                          h4("Award Overview"), 
                          tableOutput("summaryTable1")
                          )
                 )
                 ),
        tabPanel("Award Summary Statistics",
                 fluidRow(    # COUNTIES MAP
                   column(width = 8,
                          h4("Appalachian Regional Map"), 
                          leafletOutput("countyMap2")
                          )
                 ),
                 fluidRow(   # DISCIPLINES TABLE
                   column(width = 12,
                          h4("Disciplines Funded"), 
                          tableOutput("summaryTable2")
                          )
                 ),
                 fluidRow(   # DIVISIONS TABLE
                   column(width = 12,
                          h4("Divisions Funded"), 
                          tableOutput("summaryTable3")
                          )
                 ),
                 fluidRow(   # ORGANIZATION TYPE TABLE
                   column(width = 12,
                          h4("Organizations Funded"), 
                          DT::DTOutput("summaryTable4")
                          )
                 ),
                 fluidRow(   # AWARDEE INFORMATION
                   column(width = 12,
                          h4("Individual Awardee Information"), 
                          DT::DTOutput("summaryTable5")
                          )
                 ),
                 
                 )
      )
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  ####################################################################################################
  ############################# REGIONAL MAP AND AWARDEE INFORMATION TAB #############################
  ####################################################################################################
  
  ####################################################
  ################# Regional Map #####################
  ###### Includes fill of amount awarded by NEH ######
  ##### Same as countyMap2, for visual purposes ######
  ####################################################
  output$countyMap1 <- renderLeaflet({
    
    # Title positioning using CSS
    tag.map.title <- tags$style(HTML("
      .leaflet-control.map-title { 
        transform: translate(-50%,20%);
        left: 60%;
        text-align: center;
        padding-left: 5px; 
        padding-right: 5px; 
        background: rgba(255,255,255,0.75);
        font-weight: bold;
        font-size: 16px;
      }
    "))
    
    # Filter data based on selected state
    if (input$state == "All") {     # ENTIRE APPALACHIAN REGIONAL MAP
      state_data <- app_merge # NEH data for state fills
      neh_data <- neh_data # NEH data for award fills
      counties_dat <- counties # 13 Appalachian states and all counties
      counties_app_dat <- counties_app # Appalachia counties only (i.e., only Appalachian KY)
      appalachia_state <- appalachia # Polygons for Appalachian states
      map_title <- tags$div(
        tag.map.title, HTML("Appalachian Region")
      )
      
    } else {     # APPALACHIAN STATES MAP
      state_data <- app_merge %>% filter(state == input$state)
      neh_data <- neh_data %>% filter(state == input$state)
      counties_dat <- counties %>% filter(state == input$state)
      counties_app_dat <- counties_app %>% filter(state == input$state)
      appalachia_state <- appalachia %>% filter(ID == input$state)
      map_title <- tags$div(
        tag.map.title, HTML(paste("Appalachian Counties in", input$state))
      )
    }
    
    # Filter data based on selected year
    if (input$year == "All") {
      state_data <- state_data
      neh_data <- neh_data
      
      # Create labels for each unique county
      neh_labels <- lapply(1:nrow(neh_data), function(i) {
        HTML(paste0(
          str_to_title(neh_data$county[i]), br(), 
          "Total Funding: $", format(neh_data$total_award[i], big.mark = ","), br(), 
          "Unemployment Rate: ", round(neh_data$Unemp_Rate[i], 2), "%", br(),
          "Poverty Rate: ", round(neh_data$Poverty_Rate[i], 2), "%"
        ))
      })
      
      
    }else{
      state_data <- state_data %>% filter(YearAwarded == as.numeric(input$year))
      neh_data <- neh_data %>% filter(YearAwarded == as.numeric(input$year))
      econ_dat <- econ_sf
      
      # No Data Available Message
      if (nrow(state_data) == 0 || nrow(neh_data) == 0) {
        return(leaflet() %>%
                 addProviderTiles(providers$Stamen.TonerLite) %>%
                 addControl("No Data Available", position = "bottomright", className = "info legend"))
      }
      
      # Create labels for each unique county
      neh_labels <- lapply(1:nrow(neh_data), function(i) {
        HTML(paste0(
          str_to_title(neh_data$county[i]), br(), 
          "Total Funding: $", format(neh_data$annual_award[i], big.mark = ","), br(), 
          "Unemployment Rate: ", round(neh_data$Unemp_Rate[i], 2), "%", br(),
          "Poverty Rate: ", round(neh_data$Poverty_Rate[i], 2), "%"
        ))
      })
      
    }
    
    pal <- colorNumeric(palette = "Spectral", na.color="#808080",
                domain = neh_data$total_award)

    # Create the leaflet map
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% # Provider tiles for minimal background
      addPolygons(data = counties_dat, # All 13 Appalachian states
                  fillColor = "white", color = "black", weight = 1, opacity = 0.2,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addPolygons(data = counties_app_dat, # Appalachian-only counties
                  fillColor = "#808080", color = "black", weight = 1, opacity = 0.2) %>%
      addPolygons(data = neh_data, # NEH Award Data
                  fillColor = ~pal(as.numeric(total_award)), 
                  color = "black", weight = 1,
                  label = neh_labels,
                  labelOptions = labelOptions(
                    style = list("color" = "black", "font-size" = "12px", "font-weight" = "normal", 
                                 "background-color" = "white", "border" = "1px solid black", 
                                 "padding" = "5px", "border-radius" = "3px"),
                    textOnly = FALSE,
                    direction = "auto",
                    html = TRUE)) %>%
      addLegend("bottomleft", pal = pal, values = neh_data$total_award, title = "NEH Funding") %>%
      addPolygons(data = appalachia_state, # Appalachian state borders
                  fillColor = "white", color = "black", weight = 0.3, opacity = 0.1) %>%
      addControl(map_title, position = "topleft", className="map-title") 
  })
  
  ####################################################
  ################# Regional Map #####################
  ###### Includes fill of amount awarded by NEH ######
  ##### Same as countyMap1, for visual purposes ######
  ####################################################
  output$countyMap2 <- renderLeaflet({
    # Title positioning using CSS
    tag.map.title <- tags$style(HTML("
      .leaflet-control.map-title { 
        transform: translate(-50%,20%);
        left: 60%;
        text-align: center;
        padding-left: 5px; 
        padding-right: 5px; 
        background: rgba(255,255,255,0.75);
        font-weight: bold;
        font-size: 16px;
      }
    "))
    
    # Filter data based on selected state
    if (input$state == "All") {     # ENTIRE APPALACHIAN REGIONAL MAP
      state_data <- app_merge # NEH data for state fills
      neh_data <- neh_data # NEH data for award fills
      counties_dat <- counties # 13 Appalachian states and all counties
      counties_app_dat <- counties_app # Appalachia counties only (i.e., only Appalachian KY)
      appalachia_state <- appalachia # Polygons for Appalachian states
      map_title <- tags$div(
        tag.map.title, HTML("Appalachian Region")
      )
      
    } else {     # APPALACHIAN STATES MAP
      state_data <- app_merge %>% filter(state == input$state)
      neh_data <- neh_data %>% filter(state == input$state)
      counties_dat <- counties %>% filter(state == input$state)
      counties_app_dat <- counties_app %>% filter(state == input$state)
      appalachia_state <- appalachia %>% filter(ID == input$state)
      map_title <- tags$div(
        tag.map.title, HTML(paste("Appalachian Counties in", input$state))
      )
    }
    # Filter data based on selected year
    if (input$year == "All") {
      state_data <- state_data
      neh_data <- neh_data 
      
      # Create labels for each unique county
      neh_labels <- lapply(1:nrow(neh_data), function(i) {
        HTML(paste0(
          str_to_title(neh_data$county[i]), br(), 
          "Total Funding: $", format(neh_data$total_award[i], big.mark = ","), br(), 
          "Unemployment Rate: ", round(neh_data$Unemp_Rate[i], 2), "%", br(),
          "Poverty Rate: ", round(neh_data$Poverty_Rate[i], 2), "%"
        ))
      })
      
      
    }else{
      state_data <- state_data %>% filter(YearAwarded == as.numeric(input$year))
      neh_data <- neh_data %>% filter(YearAwarded == as.numeric(input$year))
      econ_dat <- econ_sf
      
      # No Data Available Message
      if (nrow(state_data) == 0 || nrow(neh_data) == 0) {
        return(leaflet() %>%
                 addProviderTiles(providers$Stamen.TonerLite) %>%
                 addControl("No Data Available", position = "bottomright", className = "info legend"))
      }
      
      # Create labels for each unique county
      neh_labels <- lapply(1:nrow(neh_data), function(i) {
        HTML(paste0(
          str_to_title(neh_data$county[i]), br(), 
          "Total Funding: $", format(neh_data$annual_award[i], big.mark = ","), br(), 
          "Unemployment Rate: ", round(neh_data$Unemp_Rate[i], 2), "%", br(),
          "Poverty Rate: ", round(neh_data$Poverty_Rate[i], 2), "%"
        ))
      })
      
      
    }
    
    
    pal <- colorNumeric(palette = "Spectral", na.color="#808080",
                        domain = neh_data$total_award)

    # Create the leaflet map
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% # Provider tiles for minimal background
      addPolygons(data = counties_dat, # All 13 Appalachian states
                  fillColor = "white", color = "black", weight = 1, opacity = 0.2,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addPolygons(data = counties_app_dat, # Appalachian-only counties
                  fillColor = "#808080", color = "black", weight = 1, opacity = 0.2) %>%
      addPolygons(data = neh_data, # NEH Award Data
                  fillColor = ~pal(as.numeric(total_award)), 
                  color = "black", weight = 1,
                  label = neh_labels,
                  labelOptions = labelOptions(
                    style = list("color" = "black", "font-size" = "12px", "font-weight" = "normal", 
                                 "background-color" = "white", "border" = "1px solid black", 
                                 "padding" = "5px", "border-radius" = "3px"),
                    textOnly = FALSE,
                    direction = "auto",
                    html = TRUE)) %>%
      addLegend("bottomleft", pal = pal, values = neh_data$total_award, title = "NEH Funding") %>%
      addPolygons(data = appalachia_state, # Appalachian state borders
                  fillColor = "white", color = "black", weight = 0.3, opacity = 0.1) %>%
      addControl(map_title, position = "topleft", className="map-title") 
  })
  
  
  ####################################################
  ########### Award & Econ Stats Table ###############
  #### Includes count of awards, max, and average ####
  ####################################################
  output$summaryTable1 <- renderTable({
    # Filter data based on selected state and year
    state_data <- df_clean
    if (input$state != "All") {
      state_data <- state_data %>% filter(state == input$state)
    }
    if (input$year != "All") {
      state_data <- state_data %>% filter(YearAwarded == as.numeric(input$year))
      if (nrow(state_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }
    
    # Create summary table
    summary_table <- state_data %>%
      summarise(
        Total_Grants = n(),
        Total_Amount = sum(AwardOutright, na.rm = TRUE),
        Average_Amount = mean(AwardOutright, na.rm = TRUE),
        Max_Amount = max(AwardOutright, na.rm = TRUE),
        CARES = sum(CARES, na.rm=TRUE)
      ) %>%
      select(Total_Grants, CARES, Total_Amount, Average_Amount, Max_Amount)
    
    summary_table$Total_Amount <- paste('$',formatC(summary_table$Total_Amount, big.mark=',', format = 'f', digits=2))
    summary_table$Average_Amount <- paste('$',formatC(summary_table$Average_Amount, big.mark=',', format = 'f', digits=2))
    summary_table$Max_Amount <- paste('$',formatC(summary_table$Max_Amount, big.mark=',', format = 'f', digits=2))
    
    colnames(summary_table) <- c("Total Grants", "CARES/ARP Awards", "Total Awarded", "Average", "Max")
    summary_table[] <- lapply(summary_table, as.character)
    print(summary_table)
  })
  
  
  ####################################################################################################
  ################################# SUMMARY STATISTICS TAB ###########################################
  ####################################################################################################

  ####################################################
  ########### Discipline Statistics Table ############
  ########## Includes count & amount awarded #########
  ####################################################
    output$summaryTable2 <- renderTable({
      # Filter data based on selected state and year
      state_data <- df_clean
      if (input$state != "All") {
        state_data <- state_data %>% filter(state == input$state)
      }
      if (input$year != "All") {
        state_data <- state_data %>% filter(YearAwarded == as.numeric(input$year))
        if (nrow(state_data) == 0) {
          return(data.frame("Message" = "No Data Available"))
        }
      }
      
      # Create discipline summary stats
      disc_table <- state_data %>%
        group_by(Discipline) %>%
        summarise(Count = n(), Awarded = sum(OriginalAmount, na.rm=T)) %>%
        arrange(desc(Awarded))
      
      # Format the table
      disc_table$Awarded <- paste('$',formatC(disc_table$Awarded, big.mark=',', format = 'f', digits=2))
      colnames(disc_table) <- c("Discipline", "Count", "Amount Awarded")
      print(disc_table)
      })
    
    ####################################################
    ############ Division Statistics Table #############
    ##### Includes count of disciplines & divisions ####
    ####################################################
    output$summaryTable3 <- renderTable({
      # Filter data based on selected state and year
      state_data <- df_clean
      if (input$state != "All") {
        state_data <- state_data %>% filter(state == input$state)
      }
      if (input$year != "All") {
        state_data <- state_data %>% filter(YearAwarded == as.numeric(input$year))
        if (nrow(state_data) == 0) {
          return(data.frame("Message" = "No Data Available"))
        }
      }
      
      # Create discipline summary stats
      div_table <- state_data %>%
        group_by(Division) %>%
        summarise(Count = n(), Awarded = sum(OriginalAmount, na.rm=T)) %>%
        arrange(desc(Awarded))
      
      # Format the table
      div_table$Awarded <- paste('$',formatC(div_table$Awarded, big.mark=',', format = 'f', digits=2))
      colnames(div_table) <- c("Division", "Count", "Amount Awarded")
      print(div_table)
    })
    
    ####################################################
    ########## Organization Type Stats Table ###########
    ####### Includes count of organization types #######
    ####################################################
    output$summaryTable4 <- DT::renderDT({
      # Filter data based on selected state and year
      state_data <- df_clean
      if (input$state != "All") {
        state_data <- state_data %>% filter(state == input$state)
      }
      if (input$year != "All") {
        state_data <- state_data %>% filter(YearAwarded == as.numeric(input$year))
        if (nrow(state_data) == 0) {
          return(data.frame("Message" = "No Data Available"))
        }
      }
      
      # Create organization type summary stats
      org_table <- state_data %>%
        group_by(OrganizationType) %>%
        summarise(Count = n(), Awarded = sum(OriginalAmount, na.rm=T)) %>%
        arrange(desc(Awarded))
      
      # Format the table
      org_table$Awarded <- paste('$',formatC(org_table$Awarded, big.mark=',', format = 'f', digits=2))
      colnames(org_table) <- c("Organization", "Count", "Amount Awarded")
      print(org_table)
    })
    
    ####################################################
    ########### Awardee Information Table ##############
    ## Includes institution, project title, and award ##
    ####################################################
    output$summaryTable5 <- DT::renderDT({
      # Filter data based on selected state and year
      state_data <- df_clean
      if (input$state != "All") {
        state_data <- state_data %>% filter(state == input$state)
      }
      if (input$year != "All") {
        state_data <- state_data %>% filter(YearAwarded == as.numeric(input$year))
        if (nrow(state_data) == 0) {
          return(data.frame("Message" = "No Data Available"))
        }
      }
      
      # Create summary table 
      award_table <- state_data %>%
        mutate(total_award = ifelse(OriginalAmount == 0, ApprovedOutright, OriginalAmount),
               CARES = ifelse(CARES == 1, "Yes", "No")) %>%
        select(Institution, YearAwarded, ProjectTitle, total_award, CARES, City, State, County) 
      
      # Format the table
      award_table$total_award <- paste('$',formatC(award_table$total_award, 
                                                   big.mark=',', format = 'f', digits=2))
      colnames(award_table) <- c("Institution", "Year", "Project Title", "Award Amount", "CARES/ARP Award?", 
                                 "City", "State", "County")
      award_table[] <- lapply(award_table, as.character)
      print(award_table)
    })
    
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


