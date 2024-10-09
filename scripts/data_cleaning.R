## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] data_cleaning.R
## [ AUTH ] Benjamin Skinner; bskinner@neh.gov & Maya Dalton; mdalton@neh.gov
## [ INIT ] 09 October 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "readxl", "sf", "crosswalkr", "plyr")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")
tab_dir <- file.path(root, "tables")

## -------------------------------------
## functions
## -------------------------------------

## to read shapefiles from zip file
st_read_zip <- function(zfile) {
  tmp <- tempfile()
  unzip(zfile, exdir = tmp)
  st_read(dsn = tmp)
}

## -------------------------------------
## macros
## -------------------------------------

## award period
award_period <- 2018:2023

## appalachian state abbreviations
app_st <- c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN",
            "VA", "WV")

## counties
cw_ct <- map(award_period,
             ~ read_delim(unz(file.path(dat_dir,
                                        "gaz",
                                        paste0(.x,
                                               "_Gaz_counties_national.zip")),
                              paste0(.x, "_Gaz_counties_national.txt")),
                          delim = "\t",
                          show_col_types = FALSE) |>
               rename_all(tolower) |>
               select(fips = geoid, name) |>
               mutate(year = .x)) |>
  bind_rows() |>
  arrange(fips, year)

## zctas
cw_zcta <- map(award_period,
             ~ read_delim(unz(file.path(dat_dir,
                                        "gaz",
                                        paste0(.x,
                                               "_Gaz_zcta_national.zip")),
                              paste0(.x, "_Gaz_zcta_national.txt")),
                          delim = "\t",
                          show_col_types = FALSE,
                          trim_ws = TRUE) |>
               rename_all(tolower) |>
               select(zip = geoid, ziplon = intptlong, ziplat = intptlat) |>
               mutate(year = .x)) |>
  bind_rows() |>
  arrange(zip, year)

## make a state crosswalk for use
cw_st_app <- stcrosswalk |>
  select(stfips, stabbr, stname) |>
  filter(stabbr %in% app_st) |>
  mutate(stfips = sprintf("%02d", stfips))

## -----------------------------------------------------------------------------
## Cleaning, subsetting NEH grant data for Appalachian States
## -----------------------------------------------------------------------------

## grant data files (use regular expression to pull only right ones
files <- list.files(file.path(dat_dir, "neh"), full.names = TRUE)

## map read all files
df_grant <- map(files,
                ~ read_csv(.x,
                           na = c("", "NA", "unknown", "Unknown"),
                           show_col_types = FALSE) |>
                  ## lower names
                  rename_all(tolower) |>
                  ## filter to only appalachian states
                  filter(inststate %in% app_st) |>
                  ## filter to years of inquiry
                  filter(yearawarded %in% award_period)) |>
  bind_rows() |>
  ## left join on zcta for correction of missing lon/lat
  mutate(zip = substr(instpostalcode, 1, 5)) |>
  left_join(cw_zcta, by = c("zip", "yearawarded" = "year")) |>
  mutate(lon = ifelse(!is.na(longitude), longitude, ziplon),
         lat = ifelse(!is.na(latitude), latitude, ziplat))

df_grant <- df_grant |> # Recoding new discipline variable
  mutate(newdiscipline = case_when(
    str_detect(primarydiscipline, "Art|Dance|Film|Arts|Media|Theatre|Ethnomusicology|Aesthetics") ~ "Arts",
    str_detect(primarydiscipline, "History|Civilization|Renaissance Studies|Medieval Studies") ~ "History",
    str_detect(primarydiscipline, "Literature|Literary|Classics|English|Composition and Rhetoric") ~ "Literature",
    str_detect(primarydiscipline, "Language|Linguistics|Languages|Linguistic") ~ "Language",
    str_detect(primarydiscipline, "Anthropology|Archaeology|Religion|Linguistic|Logic|Digital Preservation|
               |Journalism|Folklore and Folklife|Ethics|Philosophy|Law|Phenomenology - Existentialism") ~ "Humanities",
    str_detect(primarydiscipline, "Social Sciences|Architecture|Communications|Geography|Comparative Politics|
               |International|Political|Government|Conservation|Psychology|Sociology|Economics") ~ "Social Science",
    str_detect(primarydiscipline, "Interdisciplinary") ~ "Interdisciplinary",
    str_detect(primarydiscipline, "Studies") ~ "Area Studies",
    TRUE ~ primarydiscipline
  )
) |>
  filter(!grepl("Humanities Council", organizationtype)) # Remove humanities councils

## -----------------------------------------------------------------------------
## Cleaning, subsetting BLS economic data for Appalachian States and merging
## 2018-2023
## -----------------------------------------------------------------------------

## grant data files (use regular expression to pull only right ones
files <- list.files(file.path(dat_dir, "bls"), full.names = TRUE)

## map read all files
df_bls <- map(files,
              ~ read_csv(.x) |>
                rename_all(tolower) |>
                filter(stfips %in% cw_st_app[["stfips"]]) |>
                mutate(fips = paste0(stfips, ctfips)) |>
                mutate(year = year |> as.integer(),
                       unemp_rate = unemployed_rate |> as.numeric()) |>
                select(fips, year, unemp_rate)
              ) |>
  bind_rows() |>
  arrange(fips, year)

## -----------------------------------------------------------------------------
## Poverty data set
## -----------------------------------------------------------------------------

df_pov <- map(list.files(file.path(dat_dir, "saipe"), full.names = TRUE),
              ~ read_excel(.x, skip = 3, na = (".")) |>
                set_names(tolower) |>
                select(stfips = `state fips code`,
                       ctfips = `county fips code`,
                       poverty_rate = `poverty percent, all ages`) |>
                mutate(poverty_rate = poverty_rate |> as.numeric(),
                       fips = paste0(stfips, ctfips),
                       year = paste0("20",
                                     str_replace(.x,
                                                 "^.+(\\d{2})all\\.xls",
                                                 "\\1")) |> as.numeric()) |>
                filter(ctfips != "000") |>
                select(fips, year, poverty_rate)) |>
  bind_rows() |>
  arrange(fips, year)

## -----------------------------------------------------------------------------
## ARC data
## -----------------------------------------------------------------------------

# Load in ARC Data for counties in Appalachian region
df_arc <- read_excel(list.files(file.path(dat_dir, "arc"), full.names = TRUE),
                     skip = 4) |>
  rename_all(tolower) |>
  mutate(appalachia = 1) |>
  select(fips, appalachia)

## -----------------------------------------------------------------------------
## IPEDS data
## -----------------------------------------------------------------------------

df_ipeds <- map(award_period,
                ~ read_delim(unz(file.path(dat_dir,
                         "ipeds",
                         paste0("HD", .x, ".zip")),
               paste0(paste0("hd", .x, ".csv")))) |>
              mutate(CCBASIC2 = case_when(
                  CCBASIC %in% c(1:8, 11:12) ~ 1, # Associates - Public
                  CCBASIC %in% c(9:10, 13:14) ~ 2, # Associates - Private
                  CCBASIC == 15 ~ 3, # Research University (Very High Activity)
                  CCBASIC == 16 ~ 4, # Research University (High Activity)
                  CCBASIC == 17 ~ 5, # Doctoral/Research University
                  CCBASIC == 18 ~ 6, # Master’s (Large)
                  CCBASIC == 19 ~ 7, # Master’s (Medium)
                  CCBASIC == 20 ~ 8, # Master’s (Small)
                  CCBASIC %in% c(21:23) ~ 9, # Baccalaureate Colleges
                  CCBASIC == 24 ~ 10, # Faith-Related Institutions
                  CCBASIC == 25 ~ 11, # Medical Schools
                  CCBASIC == 26 ~ 12, # Other health profession schools
                  CCBASIC == 27 ~ 13, # Engineering schools
                  CCBASIC == 28 ~ 14, # Other tech-related schools
                  CCBASIC == 29 ~ 15, # Business/Management Schools 
                  CCBASIC == 30 ~ 16, # Art, Music, and Design Schools
                  CCBASIC == 31 ~ 17, # Law Schools
                  CCBASIC == 32 ~ 18, # Other special-focus institutions
                  CCBASIC == 33 ~ 19, # Tribal colleges
                  CCBASIC == -2 ~ NA, # Non-carnegie institutions
                ),
                FIPS = as.character(COUNTYCD),
                FIPS = ifelse(nchar(FIPS) == 4, # Add leading zero to FIPS codes with 4 characters for merge
                              paste0("0", FIPS),
                              FIPS)) |>
              select(UNITID, INSTNM, CITY, STABBR, HBCU, TRIBAL, CCBASIC2, COUNTYNM, LONGITUD, LATITUDE, ZIP, FIPS) |>
              rename_all(tolower)
              ) |>
  bind_rows() |>
  distinct() |>
  arrange(stabbr, fips)

## -----------------------------------------------------------------------------
## join BLS, poverty, appalachia, and state crosswalk data
## -----------------------------------------------------------------------------

df_eco <- df_bls |>
  left_join(df_pov, by = c("fips", "year")) |>
  left_join(df_arc, by = "fips") |>
  left_join(df_ipeds, by = "fips") |> 
  mutate(appalachia = ifelse(is.na(appalachia), 0, appalachia)) |>
  left_join(cw_ct, by = c("fips", "year")) |>
  mutate(stfips = substr(fips, 1, 2)) |>
  left_join(cw_st_app, by = c("stfips")) |>
  select(fips, county = name, appalachia, stname, stabbr.y, year, unemp_rate, poverty_rate,
         hbcu, tribal, ccbasic2)

## -----------------------------------------------------------------------------
## place applications in counties
## -----------------------------------------------------------------------------

## read in shapefiles
df_shp <- map(award_period,
              ~ st_read_zip(file.path(dat_dir,
                                      "tiger",
                                      paste0("tl_", .x, "_us_county.zip"))) |>
                rename_all(tolower) |>
                select(fips = geoid, geometry)) |>
  set_names(paste0("y", award_period))

## spatially join applications to get fips for lon/lat
df_grant_fips <- map(award_period,
                     ~ df_grant |>
                       filter(yearawarded == .x) |>
                       select(appnumber, lon, lat) |>
                       mutate(across(c("lon", "lat"), ~ as.numeric(.x))) |>
                       st_as_sf(coords = c("lon", "lat"), crs = "NAD83") |>
                       st_join(df_shp[[paste0("y", .x)]]) |>
                       st_drop_geometry()) |>
  bind_rows()

## join fips back in
df_grant <- df_grant |>
  left_join(df_grant_fips, by = "appnumber")

## -----------------------------------------------------------------------------
## join final data set and save
## -----------------------------------------------------------------------------

## join
df <- df_grant |>
  left_join(df_eco, by = c("fips", "yearawarded" = "year"))

## save
write_csv(df, file.path(dat_dir, "analysis.csv"))

## -----------------------------------------------------------------------------
## end script
## -----------------------------------------------------------------------------
