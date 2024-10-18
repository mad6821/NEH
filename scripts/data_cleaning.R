## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] data_cleaning.R
## [ AUTH ] Benjamin Skinner; bskinner@neh.gov & Maya Dalton; mdalton@neh.gov
## [ INIT ] 18 October 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "readxl", "sf", "crosswalkr")
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

## recode disciplines
## TODO: need to double check regexes
df_grant <- df_grant |>
  mutate(newdiscipline = case_when(
    ## art
    str_detect(primarydiscipline, paste("Art",
                                        "Dance",
                                        "Film",
                                        "Arts",
                                        "Media",
                                        "Theatre",
                                        "Ethnomusicology",
                                        "Aesthetics",
                                        sep = "|")) ~ "Arts",
    ## history
    str_detect(primarydiscipline, paste("History",
                                        "Civilization",
                                        "Renaissance Studies",
                                        "Medieval Studies",
                                        sep = "|")) ~ "History",
    ## literature
    str_detect(primarydiscipline, paste("Literature",
                                        "Literary",
                                        "Classics",
                                        "English",
                                        "Composition and Rhetoric",
                                        sep = "|")) ~ "Literature",
    ## language
    str_detect(primarydiscipline, paste("Language",
                                        "Linguistics",
                                        "Languages",
                                        "Linguistic",
                                        sep = "|")) ~ "Language",
    ## humanities
    str_detect(primarydiscipline, paste("Anthropology",
                                        "Archaeology",
                                        "Religion",
                                        "Linguistic",
                                        "Logic",
                                        "Digital Preservation",
                                        "Journalism",
                                        "Folklore and Folklife",
                                        "Ethics",
                                        "Philosophy",
                                        "Law",
                                        "Phenomenology - Existentialism",
                                        sep = "|")) ~ "Humanities",
    ## social science
    str_detect(primarydiscipline, paste("Social Sciences",
                                        "Architecture",
                                        "Communications",
                                        "Geography",
                                        "Comparative Politics",
                                        "International",
                                        "Political",
                                        "Government",
                                        "Conservation",
                                        "Psychology",
                                        "Sociology",
                                        "Economics",
                                        sep = "|")) ~ "Social Science",
    ## interdisciplinary
    str_detect(primarydiscipline, "Interdisciplinary") ~ "Interdisciplinary",
    ## area studies
    str_detect(primarydiscipline, "Studies") ~ "Area Studies",
    ## < remainder >
    TRUE ~ primarydiscipline
  ))

## filter our humanities councils
df_grant <- df_grant |>
  filter(!grepl("Humanities Council", organizationtype))

## TODO: some of the challenge programs receive $0 in award outright,
## so need to either pull matching or original amount. 

## -----------------------------------------------------------------------------
## Cleaning, subsetting BLS economic data for Appalachian States and merging
## 2018-2023
## -----------------------------------------------------------------------------

## grant data files (use regular expression to pull only right ones
files <- list.files(file.path(dat_dir, "bls"), full.names = TRUE)

## map read all files
df_bls <- map(files,
              ~ read_csv(.x, show_col_types = FALSE) |>
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
                ~ read_csv(unz(file.path(dat_dir,
                                         "ipeds",
                                         paste0("HD", .x, ".zip")),
                               paste0(paste0("hd", .x, ".csv"))),
                           show_col_types = FALSE) |>
                  rename_all(tolower) |>
                  ## subset
                  select(unitid, heiname = instnm, heicity = city,
                         heistate = stabbr,
                         heizip = zip, fips = countycd,
                         heilon = longitud, heilat = latitude,
                         hbcu, ccbasic) |>
                  ## add leading zero
                  mutate(fips = sprintf("%05d", fips)) |>
                  ## convert 1 Yes 2 No to 1 Yes 0 No
                  mutate(hbcu = ifelse(hbcu == 2, 0, hbcu)) |>
                  ## recode carnegie basic
                  mutate(ccb = case_when(
                    ccbasic %in% c(1:8, 11:12) ~ 1, # associates - public
                    ccbasic %in% c(9:10, 13:14) ~ 2, # associates - private
                    ccbasic == 15 ~ 3, # research university (very high activity)
                    ccbasic == 16 ~ 4, # research university (high activity)
                    ccbasic == 17 ~ 5, # doctoral/research university
                    ccbasic == 18 ~ 6, # master’s (large)
                    ccbasic == 19 ~ 7, # master’s (medium)
                    ccbasic == 20 ~ 8, # master’s (small)
                    ccbasic %in% c(21:23) ~ 9, # baccalaureate colleges
                    ccbasic == 24 ~ 10, # faith-related institutions
                    ccbasic == 25 ~ 11, # medical schools
                    ccbasic == 26 ~ 12, # other health profession schools
                    ccbasic == 27 ~ 13, # engineering schools
                    ccbasic == 28 ~ 14, # other tech-related schools
                    ccbasic == 29 ~ 15, # business/management schools
                    ccbasic == 30 ~ 16, # art, music, and design schools
                    ccbasic == 31 ~ 17, # law schools
                    ccbasic == 32 ~ 18, # other special-focus institutions
                    ccbasic == 33 ~ 19, # tribal colleges
                    ccbasic == -2 ~ NA  # non-carnegie institutions
                  )) |>
                  ## remove basic
                  select(-ccbasic) |>
                  ## add year
                  mutate(year = .x) |>
                  ## order vars
                  select(year, everything())) |>
  bind_rows() |>
  filter(heistate %in% pull(cw_st_app, stabbr)) |>
  arrange(unitid, year)

## -----------------------------------------------------------------------------
## join BLS, poverty, appalachia, and state crosswalk data
## -----------------------------------------------------------------------------

df_eco <- df_bls |>
  left_join(df_pov, by = c("fips", "year")) |>
  left_join(df_arc, by = "fips") |>
  mutate(appalachia = ifelse(is.na(appalachia), 0, appalachia)) |>
  left_join(cw_ct, by = c("fips", "year")) |>
  mutate(stfips = substr(fips, 1, 2)) |>
  left_join(cw_st_app, by = c("stfips")) |>
  select(year, fips, county = name, appalachia, unemp_rate, poverty_rate) |>
  mutate(county = str_remove_all(county, " County"))

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
## Crosswalk with NEH and IPEDS data
## -----------------------------------------------------------------------------

## make a UNITID crosswalk for use & save csv
cw_unitid <- df_ipeds |> select(heiname, unitid) |> distinct()
write_csv(cw_unitid, file.path(dat_dir, "cw_unitid.csv"))

## Pull out distinct institutions from grant data & save csv
df_grant_hei <- df_grant |>
  select(institution, instcity, inststate, zip) |>
  distinct()
write_csv(df_grant_hei, file.path(dat_dir, "df_grant_hei.csv"))

## read in NEH/UNITID data - manually added UNITID to NEH data
df_neh_unitid <- read_csv(file.path(dat_dir, "clean_neh_unitid.csv"),
                          show_col_types = FALSE,
                          col_select = c(institution, unitid, instcity)) |>
  drop_na() |>
  distinct(institution, instcity, unitid)

## Join UNITID into NEH grant data
df_grant_unitid <- df_grant |>
  left_join(df_neh_unitid, by = c("institution", "instcity"))

## Join NEH data and ipeds
df_grant_ipeds <- df_grant_unitid |>
  left_join(df_ipeds, by = c("unitid", "fips", "yearawarded" = "year"))

## -----------------------------------------------------------------------------
## join final data set and save
## -----------------------------------------------------------------------------

## join
df <- df_grant_ipeds |>
  left_join(df_eco, by = c("fips", "yearawarded" = "year")) |>
  left_join(cw_st_app |> select(stabbr, stname), by = c("inststate" = "stabbr")) |>
  select(appnumber, unitid, institution, orgtype = organizationtype, ccb, hbcu,
         instcity, inststate, stname, county, appalachia, fips, zip,
         lon, lat, yearawarded, title = projecttitle, program, division,
         ao = awardoutright, newdiscipline, primarydiscipline,
         disciplines, unemp_rate, poverty_rate)

## save
write_csv(df, file.path(dat_dir, "analysis.csv"))

## -----------------------------------------------------------------------------
## end script
## -----------------------------------------------------------------------------
