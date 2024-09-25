## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] data_cleaning.R
## [ AUTH ] Maya A. Dalton; mdalton@neh.gov
## [ INIT ] August 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "readxl", "sf", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
neh_dat_dir <- file.path(dat_dir, "NEHData")
eco_dat_dir <- file.path(dat_dir, "EconData")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")
tab_dir <- file.path(root, "tables")

## -------------------------------------
## macros
## -------------------------------------

## award period
award_period <- 2018:2023

## appalachian state abbreviations
app_st <- c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN",
            "VA", "WV")

## counties
ct_10 <- read_csv(file.path(dat_dir, "national_county.txt"),
                  col_names = c("stabbr", "stfips", "ctfips", "name", "status")) |>
  mutate(fips = paste0(stfips, ctfips)) |>
  select(fips, name)

ct_20 <- read_delim(file.path(dat_dir, "national_county2020.txt"),
                    delim = "|") |>
  rename_all(tolower) |>
  mutate(fips = paste0(statefp, countyfp)) |>
  select(fips, name = countyname)

## make a state crosswalk for use
cw_st_app <- stcrosswalk |>
  select(stfips, stabbr, stname) |>
  filter(stabbr %in% app_st) |>
  mutate(stfips = sprintf("%02d", stfips))

## make a county name crosswalk for use
## NB: being a little over conservative here to use name with year; doing so
## just in case of name change between 2010 and 2020 censuses
cw_ct_name <- bind_rows(expand_grid(ct_10, year = award_period[1:2]),
                        expand_grid(ct_20, year = award_period[3:5])) |>
  arrange(fips, year)

## -----------------------------------------------------------------------------
## Cleaning, subsetting NEH grant data for Appalachian States
## -----------------------------------------------------------------------------

## grant data files (use regular expression to pull only right ones
files <- list.files(neh_dat_dir, pattern = "NEH_Grants")

## map read all files
## NB: missing lon/lat for 4 records; will need to do fix
## with zip code geocode; filtering out for now but need to add back in
df_grant <- map(files,
                ~ read_csv(file.path(neh_dat_dir, .x),
                           na = c("", "NA", "unknown", "Unknown"),
                           show_col_types = FALSE) |>
                  ## lower names
                  rename_all(tolower) |>
                  ## add file name as column so you know later
                  mutate(file_name = .x) |>
                  ## filter to only appalachian states
                  filter(inststate %in% app_st) |>
                  ## filter to years of inquiry
                  filter(yearawarded %in% award_period) |>
                  ## filter out missing lon/lat (for now)
                  filter(!is.na(longitude), !is.na(latitude))) |>
  bind_rows()

## -----------------------------------------------------------------------------
## Cleaning, subsetting BLS economic data for Appalachian States and merging
## 2018-2023
## -----------------------------------------------------------------------------

## grant data files (use regular expression to pull only right ones
files <- list.files(eco_dat_dir, pattern = "bls_county")

## map read all files
df_bls <- map(files,
              ~ read_excel(file.path(eco_dat_dir, .x),
                           na = "N.A.") |>
                rename_all(tolower) |>
                filter(state_fips %in% cw_app[["stfips"]]) |>
                mutate(fips = paste0(state_fips, county_fips)) |>
                mutate(year = year |> as.integer()) |>
                select(fips, year, unemp_rate)
              ) |>
  bind_rows() |>
  arrange(fips, year)

## -----------------------------------------------------------------------------
## Poverty data set
## -----------------------------------------------------------------------------

df_pov <- read_excel(file.path(eco_dat_dir, "census_poverty.xlsx")) |>
  set_names(tolower) |>
  select(fips = id, year, poverty_rate = `percent in poverty`) |>
  mutate(fips = sprintf("%05d", fips)) |>
  select(fips, year, poverty_rate) |>
  arrange(fips, year)

## -----------------------------------------------------------------------------
## ARC data
## -----------------------------------------------------------------------------

# Load in ARC Data for counties in Appalachian region
df_arc <- read_csv(file.path(dat_dir, "arc_clean.csv"),
                   show_col_types = FALSE) |>
  rename_all(tolower) |>
  mutate(across(everything(), ~ str_trim(.x)),
         appalachia = 1) |>
  select(fips, appalachia)

## -----------------------------------------------------------------------------
## join BLS, poverty, appalachia, and state crosswalk data
## -----------------------------------------------------------------------------

df_eco <- df_bls |>
  left_join(df_pov, by = c("fips", "year")) |>
  left_join(df_arc, by = "fips") |>
  mutate(appalachia = ifelse(is.na(appalachia), 0, appalachia)) |>
  left_join(cw_ct_name, by = c("fips", "year")) |>
  mutate(stfips = substr(fips, 1, 2)) |>
  left_join(cw_st_app, by = c("stfips")) |>
  select(fips, county = name, stname, stabbr, year, unemp_rate, poverty_rate)

## -----------------------------------------------------------------------------
## place applications in counties
## -----------------------------------------------------------------------------

## read in shapefiles
df_shp <- map(award_period,
              ~ st_read(file.path(dat_dir, "tiger",
                                  paste0("tl_", .x, "_us_county"),
                                  paste0("tl_", .x, "_us_county.shp"))) |>
                rename_all(tolower) |>
                select(fips = geoid, geometry)) |>
  set_names(paste0("y", award_period))

## spatially join applications to get fips for lon/lat
df_grant_fips <- map(award_period,
                     ~ df_grant |>
                       filter(yearawarded == .x) |>
                       select(appnumber, lon = longitude, lat = latitude) |>
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
write_csv(df, file.path(dat_dir, "df_clean.csv"))

## -----------------------------------------------------------------------------
## end script
## -----------------------------------------------------------------------------
