## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] shiny_app.R
## [ AUTH ] Maya Dalton (mdalton@neh.gov) Benjamin Skinner (bskinner@neh.gov)
## [ INIT ] August 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "readxl", "sf", "leaflet", "shiny", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")
tab_dir <- file.path(root, "tables")

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

## award period
award_period <- df |> distinct(yearawarded) |> pull()

## -----------------------------------------------------------------------------
## read in data; pull aggregate data out; pull macros out
## -----------------------------------------------------------------------------

df <- read_csv(file.path(dat_dir, "analysis.csv"), show_col_types = FALSE)

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
map_ct <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) |>
  rename_all(tolower) |>
  left_join(maps::county.fips |>
              mutate(fips = sprintf("%05d", fips)),
            by = c("id" = "polyname")) |>
  separate(id, c("state", "county"), sep = ",") |>
  mutate(county = str_to_title(county),
         state = str_to_title(state)) |>
  filter(state %in% pull(app_st, state)) |>
  left_join(df_arc |>
              mutate(appalachia = 1) |>
              select(fips, appalachia),
            by = "fips") |>
  mutate(appalachia = ifelse(is.na(appalachia), 0, appalachia)) |>
  st_transform(crs = map_crs)

## states
map_st <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) |>
  rename_all(tolower) |>
  filter(id %in% pull(distinct(df_arc, state = tolower(state), state))) |>
  mutate(state = str_to_title(id)) |>
  select(-id) |>
  remove_rownames() |>
  st_transform(crs = map_crs)

## -----------------------------------------------------------------------------
## UI
## -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("NEH Funding in the Appalachian Region"),
  sidebarLayout(
    sidebarPanel(
      helpText("Welcome to a comprehensive dashboard of NEH funding in the Appalachian region. 
               To explore, please use the drop down menus below to select a state and year."),
      ## state dropdown menu
      selectInput("state", "Select a State:",
                  choices = c("All", app_st |> pull(state)),
                  selected = "All", width = "200px"),
      ## year dropdown menu
      selectInput("year", "Select a Year:",
                  choices = c("All", award_period),
                  selected = "All", width = "200px")
    ),
    mainPanel(
      ## main map (stays above)
      fluidRow(    # COUNTIES MAP
        column(width = 12,
               h4("Appalachian Regional Map"),
               leafletOutput("countyMap1")
               )
      ),
      ## tabbed panels that with different output per tab
      tabsetPanel(
        tabPanel("Overview of Awards Granted",
                 fluidRow(   # AWARDS STATS
                   column(width = 12,
                          h4("Award Overview"),
                          tableOutput("summaryTable1")
                          )
                 )
                 ),
        tabPanel("Award Summary Statistics",
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
                 )
                 )
      )
    )
  )
)

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
    pal <- colorNumeric(palette = "Spectral",
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

  ####################################################
  ########### Award & Econ Stats Table ###############
  #### Includes count of awards, max, and average ####
  ####################################################
  output$summaryTable1 <- renderTable({
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
    summary_table <- neh_data |>
      summarise(total_grants = n(),
                total_award = sum(totaward, na.rm = TRUE),
                average_award = mean(totaward, na.rm = TRUE),
                max_award = max(totaward, na.rm = TRUE)) |>
      select(total_grants, total_award, average_award, max_award) |>
      mutate(across(ends_with("_award"), ~ scales::dollar(.x)))

    colnames(summary_table) <- c("Total Grants", "Total Awarded", "Average", "Max")
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

    # Create discipline summary stats
    disc_table <- neh_data |>
      group_by(parent_discipline) |>
      summarise(count = n(),
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded))

    # Format the table
    colnames(disc_table) <- c("Discipline", "Count", "Amount Awarded")
    print(disc_table)
  })

  ####################################################
  ############ Division Statistics Table #############
  ##### Includes count of disciplines & divisions ####
  ####################################################
  output$summaryTable3 <- renderTable({
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(state == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }

    # Create discipline summary stats
    div_table <- neh_data |>
      group_by(division) |>
      summarise(count = n(),
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded))

    # Format the table
    colnames(div_table) <- c("Division", "Count", "Amount Awarded")
    print(div_table)
  })

  ####################################################
  ########## Organization Type Stats Table ###########
  ####### Includes count of organization types #######
  ####################################################
  output$summaryTable4 <- DT::renderDT({
    # Filter data based on selected state and year
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(state == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }

    # Create organization type summary stats
    org_table <- neh_data |>
      group_by(org_type) |>
      summarise(count = n(),
                awarded = sum(totaward, na.rm = TRUE)) |>
      arrange(desc(awarded)) |>
      mutate(awarded = scales::dollar(awarded))

    # Format the table
    colnames(org_table) <- c("Organization", "Count", "Amount Awarded")
    print(org_table)
  })

  ####################################################
  ########### Awardee Information Table ##############
  ## Includes institution, project title, and award ##
  ####################################################
  output$summaryTable5 <- DT::renderDT({
    # Filter data based on selected state and year
    neh_data <- df
    if (input$state != "All") {
      neh_data <- neh_data |> filter(state == input$state)
    }
    if (input$year != "All") {
      neh_data <- neh_data |> filter(yearawarded == as.numeric(input$year))
      if (nrow(neh_data) == 0) {
        return(data.frame("Message" = "No Data Available"))
      }
    }

    # Create summary table
    award_table <- neh_data |>
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
