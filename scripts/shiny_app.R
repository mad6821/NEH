## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] shiny_app.R
## [ AUTH ] Maya Dalton (mdalton@neh.gov) Benjamin Skinner (bskinner@neh.gov)
## [ INIT ] August 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "readxl", "sf", "leaflet", 
          "shiny", "crosswalkr", "RColorBrewer", "kableExtra")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
doc_dir <- file.path(root, "docs")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")
tab_dir <- file.path(root, "tables")

## -------------------------------------
## functions
## -------------------------------------

source(file.path(scr_dir, "utils.R"))

## -------------------------------------
## set CRS
## -------------------------------------

map_crs <- "EPSG:4326"

## ----------------------
## macros
## ----------------------

## counties in appalachian region
df_arc <- read_excel(list.files(file.path(dat_dir, "arc"), full.names = TRUE),
                     skip = 4) |>
  rename_all(tolower) |>
  left_join(maps::county.fips |>
              mutate(fips = sprintf("%05d", fips)),
            by = "fips") |>
  rename(county_name = polyname)

## appalachian state abbreviations and names
app_st <- df_arc |>
  distinct(state) |>
  left_join(stcrosswalk |> select(stabbr, stname),
            by = c("state" = "stname"))

## -----------------------------------------------------------------------------
## read in data; pull aggregate data out; pull macros out
## -----------------------------------------------------------------------------

df <- read_csv(file.path(dat_dir, "analysis.csv"), show_col_types = FALSE)

## award period
award_period <- df |> distinct(yearawarded) |> pull()

## ----------------------
## aggregate
## ----------------------

df_agg_ct <- df |>
  select(stname, county, fips, fips_all_totaward, ends_with("_rate")) |>
  group_by(stname, county, fips) |>
  summarise(totaward = first(fips_all_totaward),
            unemp_rate = mean(unemp_rate),
            poverty_rate = mean(poverty_rate, na.rm = TRUE),
            yearawarded = 0,
            .groups = "drop") |>
  rename(state = stname)

df_agg_ct_yr <- df |>
  select(stname, county, fips, yearawarded, fips_yr_totaward,
         ends_with("_rate")) |>
  distinct(fips_yr_totaward, .keep_all = TRUE) |>
  rename(totaward = fips_yr_totaward,
         state = stname)

df_agg <- bind_rows(df_agg_ct, df_agg_ct_yr)

## -----------------------------------------------------------------------------
## map data
## -----------------------------------------------------------------------------

## all counties in appalachia states, with indicator for appalachian counties
map_ct <- st_read_zip(file.path(dat_dir, "tiger", "cb_2023_us_county_500k.zip")) |>
  rename_all(tolower) |>
  rename(fips = geoid,
         stfips = statefp,
         county = name) |>
  mutate(stfips = as.integer(stfips)) |>
  select(stfips, fips, county, geometry) |>
  left_join(crosswalkr::stcrosswalk |> select(stfips, state = stname),
            by = "stfips") |>
  filter(state %in% pull(app_st, state)) |>
  left_join(df_arc |>
              mutate(appalachia = 1) |>
              select(fips, appalachia),
            by = "fips") |>
  mutate(appalachia = ifelse(is.na(appalachia), 0, appalachia)) |>
  st_transform(crs = map_crs)

## states
map_st <- st_read_zip(file.path(dat_dir, "tiger", "cb_2023_us_state_500k.zip")) |>
  rename_all(tolower) |>
  rename(state = name,
         stfips = statefp) |>
  filter(tolower(state) %in% pull(distinct(df_arc, state = tolower(state),
                                           state))) |>
  select(stfips, state, geometry) |>
  st_transform(crs = map_crs)

## -----------------------------------------------------------------------------
## UI
## -----------------------------------------------------------------------------

# Define UI
ui <- fluidPage(
  ## -------------------------------------
  ## inline CSS to center header content
  ## -------------------------------------
  tags$head(
    tags$style(HTML("
      .centered-panel {
        display: flex;
        justify-content: center;
        text-align: center;
      }
      .input-row {
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      .input-row label {
        margin-right: 10px;
        min-width: 100px;
        text-align: right;
      }
      .centered-inputs {
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      .help-block {
        color: black !important;
      }
     .leaflet-control .legend .leaflet-legend-labels {
        display: flex; !important
        align-items: center;
        font-size: 10px;
      }
      .leaflet .legend {
        font-size: 10px;
        padding: 4px;
        line-height: 12px;
        width: auto;
      }
      .leaflet .legend i{
        width: 10px;
        height: 8px;
        margin-right: 2px;
        float: left;
        font-size: 8px;
      }
    "))
  ),
  
  ## -------------------------------------
  ## fluid row for main panel (inputs)
  ## -------------------------------------
  fluidRow(
    div(
      class = "centered-panel",
      mainPanel(
        wellPanel(
          helpText("Welcome to a comprehensive dashboard of NEH funding in the Appalachian region. 
                   To explore, please use the drop down menus below to select a state and year.")
        ),
        # Centered inputs
        div(
          class = "centered-inputs",
          # State dropdown menu (with text to the left)
          div(
            class = "input-row",
            tags$label("Select a State:"),
            selectInput("state", NULL,
                        choices = c("All", app_st |> pull(state)),
                        selected = "All", width = "200px")
          ), # end of state div
          
          # Year dropdown menu (with text to the left)
          div(
            class = "input-row",
            tags$label("Select a Year:"),
            selectInput("year", NULL,
                        choices = c("All", award_period),
                        selected = "All", width = "200px")
          ) # end of year div
        ) # end of inputs div
      ) # end of main panel
    ) # end of text div
  ), # end of fluid row
  
  ## -------------------------------------
  ## Main panel leaflet map
  ## -------------------------------------
    column(width = 8, offset = 0, 
           div(style='margin-right:-10em;'),
      mainPanel(
      ## ---------------------------------------------------------------------
      ## MAIN COUNTIES MAP  ------------------------------------------------
      ## ---------------------------------------------------------------------
        fluidRow(    # COUNTIES MAP
          column(width = 12, 
                 h4("Appalachian Regional Map"),
                 leafletOutput("countyMap1")
          ) # end of column
        ), # end of fluid row
      ## ---------------------------------------------------------------------
      ## PDFS of REPORTS  ------------------------------------------------
      ## ---------------------------------------------------------------------
      fluidRow(   # PDF REPORTS
        column(width = 12,
               h4("Reports"),
               actionButton("project_report", "Project Report"),
               actionButton("codebook", "Codebook")
          ) # end of column
        ) # end of fluid row
      ) # end of main panel
    ), # end of column
  
  ## -------------------------------------
  ## main panel summary tables
  ## -------------------------------------
    column(width = 4, offset = 0, 
           style='margin-left:-10em;',
      tabsetPanel( 
      ## ---------------------------------------------------------------------
      ## AWARD OVERVIEW ------------------------------------------------
      ## ---------------------------------------------------------------------
        tabPanel("Overview of Awards Granted",
                 fluidRow(   # AWARDS STATS
                   column(width = 12,
                          h4("Award Overview"),
                          tableOutput("summaryTable1")
              ) # end of column
            ) # end of fluid row
          ), # end of tabPanel
      ## ---------------------------------------------------------------------
      ## DIV/DISCP OVERVIEW ------------------------------------------------
      ## ---------------------------------------------------------------------
        tabPanel("Overview of Divisions & Disciplines", 
                fluidRow(   # DISCIPLINES TABLE
                    column(width = 12,
                          h4("Disciplines Funded"),
                          tableOutput("summaryTable2")
            ) # end of column
          ), # end of fluid row
                fluidRow(   # DIVISIONS TABLE
                    column(width = 12,
                          h4("Divisions Funded"),
                          tableOutput("summaryTable3")
              ) # end of column
            ) # end of fluid row
          ), # end of tabPanel
      
      ## -------------------------------------------------------------------
      ## ORGS OVERVIEW ------------------------------------------------
      ## -------------------------------------------------------------------
        tabPanel("Overview of Organizations", 
          fluidRow(   # IPEDS TABLE
            column(width = 12,
                   h4("HEIs Funded"),
                   tableOutput("summaryTable4")
            ) # end of column
          ), # end of fluid row
          fluidRow(   # HBCU TABLE
            column(width = 12,
                   h4("HBCUs Funded"),
                   tableOutput("summaryTable5")
            ) # end of column
          ), # end of fluid row
          fluidRow(   # ORGANIZATION TYPE TABLE
            column(width = 12,
                   h4("Organizations Funded"),
                   DT::DTOutput("summaryTable6")
            ) # end of column
          ) # end of fluid row
        ), # end of tabPanel
      
      ## ---------------------------------------------------------------------
      ## AWARDEE OVERVIEW ------------------------------------------------
      ## ---------------------------------------------------------------------
        tabPanel("Overview of Awardees",
          fluidRow(   # AWARDEE INFORMATION
            column(width = 12,
                    h4("Individual Awardee Information"),
                   DT::DTOutput("summaryTable7")
            ) # end of column
          ) # end of fluid row
        ) # end of tabPanel
      ) # end of tabsetPanel
    ) # end of column
  ) # end of fluid page


## -----------------------------------------------------------------------------
## server
## -----------------------------------------------------------------------------
server <- function(input, output, session) {

  ## ---------------------------------------------------------------------------
  ## map
  ## ---------------------------------------------------------------------------
  output$countyMap1 <- renderLeaflet({

    # Title positioning using CSS
    tag.map.title <- tags$style(HTML("
      .leaflet-control.map-title {
        transform: translate(-50%,-80%);
        left: 60%;
        text-align: center;
        padding-left: 5px;
        padding-right: 5px;
        background: rgba(255,255,255,0.75);
        font-weight: bold;
        font-size: 14px;
      }
    "))
    
    ## -------------------------------------
    ## filter on state (or all)
    ## -------------------------------------
    if (input$state == "All") {
      select_df_agg <- df_agg
      select_map_ct <- map_ct
      select_map_st <- map_st
      map_title <- tags$div(
        tag.map.title, HTML("Appalachian Region")
      )
    } else {
      select_df_agg <- df_agg |> filter(state == input$state)
      select_map_ct <- map_ct |> filter(state == input$state)
      select_map_st <- map_st |> filter(state == input$state)
      map_title <- tags$div(
        tag.map.title, HTML(paste("Appalachian Counties in", input$state))
      )
    }

    ## -------------------------------------
    ## filter on year (or all)
    ## -------------------------------------
    if (input$year == "All") {
      select_df_agg <- select_df_agg |> filter(yearawarded == 0)
    } else {
      select_df_agg <- select_df_agg |> filter(yearawarded == input$year)
      ## ----------------------
      ## message: no data
      ## ----------------------
      if (nrow(select_df_agg) == 0) {
        return(leaflet() |>
                 addProviderTiles(providers$Stamen.TonerLite) %>%
                 addControl("No Data Available",
                            position = "bottomright",
                            className = "info legend"))
      }
    }

    ## -------------------------------------
    ## convert NEH data to sf
    ## -------------------------------------
    select_df_agg <- select_df_agg |>
      left_join(select_map_ct, by = c("state", "county", "fips")) |>
      st_as_sf(crs = map_crs) |>
      filter(appalachia == 1)

    ## -------------------------------------
    ## county-specific labels
    ## -------------------------------------
    neh_labels <- lapply(1:nrow(select_df_agg), function(i) {
      HTML(paste0(
        paste0("<b>", select_df_agg$county[i], ", ", select_df_agg$state[i], "</b>"),
        br(),
        "Total Funding: ", scales::dollar(select_df_agg$totaward[i]),
        br(),
        "Unemployment Rate: ", scales::percent(select_df_agg$unemp_rate[i],
                                               scale = 1,
                                               accuracy = 0.01),
        br(),
        "Poverty Rate: ", scales::percent(select_df_agg$poverty_rate[i],
                                          scale = 1,
                                          accuracy = 0.01)
      ))
    })

    ## -------------------------------------
    ## set up color palatte
    ## -------------------------------------
    pal <- colorNumeric(palette = brewer.pal(n = 9, name = "Reds"), 
                        na.color = "#808080", 
                        domain = select_df_agg$totaward)

    ## -------------------------------------
    ## leaflet map
    ## -------------------------------------
    leaflet() |>
      ## minimal background tiles
      addProviderTiles(providers$CartoDB.PositronNoLabels) |>
      ## state borders
      addPolygons(data = select_map_st,
                  fillColor = "white",
                  color = "black",
                  weight = 0.75,
                  opacity = 0.75) |>
      ## all counties in appalachia states
      addPolygons(data = select_map_ct,
                  fillColor = "white",
                  color = "black",
                  weight = 1,
                  opacity = 0.2,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) |>
      ## appalachia counties only
      addPolygons(data = select_map_ct |> filter(appalachia == 1),
                  fillColor = "#808080",
                  color = "black",
                  weight = 1,
                  opacity = 0.2) |>
      ## main data / labels
      addPolygons(data = select_df_agg,
                  fillColor = ~pal(as.numeric(totaward)),
                  color = "black",
                  weight = 1,
                  label = neh_labels,
                  fillOpacity = 0.8,
                  labelOptions = labelOptions(
                    style = list("color" = "black",
                                 "font-size" = "12px",
                                 "font-weight" = "normal",
                                 "background-color" = "white",
                                 "border" = "1px solid black",
                                 "padding" = "5px",
                                 "border-radius" = "3px"),
                    textOnly = FALSE,
                    direction = "auto",
                    html = TRUE)) |>
      addLegend("bottomleft",
                pal = pal,
                values = select_df_agg$totaward,
                title = "NEH Funding") |>
      ## controls
      addControl(map_title,
                 position = "topleft",
                 className = "map-title")
  })
  
  ## -------------------------------------
  ## pdf reports
  ## -------------------------------------
  output$project_report <- downloadHandler(
    filename = "TEST PDF FOR SHINY APP.pdf",
    content = function(file) {
      source_file <- file.path(doc_dir, "TEST PDF FOR SHINY APP.pdf")
      file.copy(source_file, filename) 
    }
  )
  
  output$codebook <- downloadHandler(
    filename = "TEST PDF FOR SHINY APP.pdf",
    content = function(file) {
      source_file <- file.path(doc_dir, "TEST PDF FOR SHINY APP.pdf")
      file.copy(source_file, filename) 
    }
  )
  
  ## ---------------------------------------------------------------------------
  ## tables
  ## ---------------------------------------------------------------------------
  
  ## ---------------------------------------------------------------------------
  ## award overview
  ## ---------------------------------------------------------------------------

  output$summaryTable1 <- function() {
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(stname == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }
    
    neh_data |>
      subset(appalachia == 1) |>
      summarise(total_grants = n(),
                total_award = sum(totaward, na.rm = TRUE),
                average_award = mean(totaward, na.rm = TRUE),
                max_award = max(totaward, na.rm = TRUE)) |>
      select(total_grants, total_award, average_award, max_award) |>
      mutate(across(ends_with("_award"), ~ scales::dollar(.x))) |>
      knitr::kable("html", align = "lrrr", 
                   col.names = c("Total Grants", "Total Awarded", "Average", "Max")) |>
      kable_styling("striped", full_width = F) 
  }

  ## ---------------------------------------------------------------------------
  ## divisions and disciplines
  ## ---------------------------------------------------------------------------

  ## -------------------------------------
  ## discipline overview table
  ## -------------------------------------
  output$summaryTable2 <- function() {
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(stname == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }
    
    neh_data |>
      subset(appalachia == 1) |>
      group_by(parent_discipline) |>
      summarise(count = n(),
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded)) |>
      knitr::kable("html", align = "lcr", 
                   col.names = c("Discipline", "Count", "Amount Awarded")) |>
      kable_styling("striped", full_width = F) 
  }

  ## -------------------------------------
  ## division overview table
  ## -------------------------------------
  
  output$summaryTable3 <- function() {
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(stname == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }
    
    neh_data |>
      subset(appalachia == 1) |>
      group_by(division) |>
      summarise(count = n(),
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded)) |>
      knitr::kable("html", align = "lcr", 
                   col.names = c("Division", "Count", "Amount Awarded")) |>
      kable_styling("striped", full_width = F) 
  }
  
  ## ---------------------------------------------------------------------------
  ## organizations
  ## ---------------------------------------------------------------------------
  
  ## -------------------------------------
  ## ipeds overview table
  ## -------------------------------------
  
  output$summaryTable4 <- function() {
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(stname == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }
    
    neh_data |>
      subset(appalachia == 1) |>
      drop_na(ccb) |>
      mutate(ccnames = case_when( # Only coded the HEIs we actually have
        ccb == 1 ~ "Associate’s - Public", 
        ccb == 2 ~ "Associate’s - Private",
        ccb == 3 ~ "Research University (Very High)",
        ccb == 4 ~ "Research University (High)",
        ccb == 5 ~ "Doctoral/Research University",
        ccb == 6 ~ "Master’s (Large)",
        ccb == 7 ~ "Master’s (Medium)",
        ccb == 8 ~ "Master’s (Small)",
        ccb == 9 ~ "Baccalaureate Colleges",
        .default = as.character(ccb)
      )) |>
      group_by(ccnames) |>
      summarise(count = n(), 
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded)) |>
      knitr::kable("html", align = "lcr", 
                   col.names = c("HEI", "Count", "Amount Awarded")) |>
      kable_styling("striped", full_width = F) 
  }
  
  ## -------------------------------------
  ## hbcu overview table
  ## -------------------------------------
  
  output$summaryTable5 <- function() {
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(stname == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }
    
    neh_data |>
      subset(hbcu == 1 & appalachia == 1) |>
      group_by(hbcu) |>
      summarise(count = n(),
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded)) |>
      dplyr::select(-hbcu) |>
      knitr::kable("html", align = "lcr", 
                   col.names = c("Count", "Amount Awarded")) |>
      kable_styling("striped", full_width = F) 
  }
  
  
  ## -------------------------------------
  ## other orgs overview table
  ## -------------------------------------
  output$summaryTable6 <- DT::renderDT({
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(stname == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }
    
    # Create organization type summary stats
    org_table <- neh_data |>
      subset(appalachia == 1) |>
      filter(!org_type %in% c("Four-Year College", "Two-Year College", 
                              "University", "Professional School")) |>
      group_by(org_type) |>
      summarise(count = n(),
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded))
    
    # Format the table
    colnames(org_table) <- c("Organization", "Count", "Amount Awarded")
    print(org_table)
  })
  

  ## ---------------------------------------------------------------------------
  ## awardees
  ## ---------------------------------------------------------------------------
  ## -------------------------------------
  ## awardee overview table
  ## -------------------------------------
  output$summaryTable7 <- DT::renderDT({
    # Filter data based on selected state and year
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(stname == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }

    # Create summary table
    award_table <- neh_data |>
      subset(appalachia == 1) |>
      mutate(awarded = scales::dollar(totaward)) |>
      select(institution, yearawarded, title, awarded, instcity, stname, county)


    # Format the table
    colnames(award_table) <- c("Institution", "Year", "Project Title",
                               "Award Amount", "City", "State", "County")
    award_table[] <- lapply(award_table, as.character)
    print(award_table)
  })
}

## -----------------------------------------------------------------------------
## run application
## -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)

## -----------------------------------------------------------------------------
################################################################################
