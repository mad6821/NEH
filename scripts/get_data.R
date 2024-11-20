## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] get_data.R
## [ AUTH ] Benjamin Skinner; bskinner@neh.gov & Maya Dalton; mdalton@neh.gov
## [ INIT ] 02 October 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "curl")
sapply(libs, require, character.only = TRUE)

## paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")
tab_dir <- file.path(root, "tables")

## -------------------------------------
## macros
## -------------------------------------

## years of interest
yrs <- 2018:2023

## -----------------------------------------------------------------------------
## NEH data
## -----------------------------------------------------------------------------

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "neh"), showWarnings = FALSE)

## base url
base_url <- "https://apps.neh.gov/open/data"
walk(1:2,
     ~ download.file(file.path(base_url,
                               paste0("NEH_Grants20", .x, "0s.csv")),
                     file.path(dat_dir,
                               "neh",
                               paste0("NEH_Grants20", .x, "0s.csv")),
                     mode = "wb"))

## -----------------------------------------------------------------------------
## county shapefiles
## -----------------------------------------------------------------------------

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "tiger"), showWarnings = FALSE)

## base url
base_url <- "https://www2.census.gov/geo/tiger"

## loop through
walk(yrs,
     ~ download.file(file.path(base_url,
                               paste0("TIGER", .x),
                               "COUNTY",
                               paste0("tl_", .x, "_us_county.zip")),
                     file.path(dat_dir,
                               "tiger",
                               paste0("tl_", .x, "_us_county.zip")),
                     mode = "wb"))

## also get latest county / state for shiny app map
download.file(file.path(base_url,
                        paste0("GENZ", yrs[length(yrs)]),
                        "shp",
                        paste0("cb_", yrs[length(yrs)], "_us_county_500k.zip")),
              file.path(dat_dir,
                        "tiger",
                        paste0("cb_", yrs[length(yrs)], "_us_county_500k.zip")),
              mode = "wb")

download.file(file.path(base_url,
                        paste0("GENZ", yrs[length(yrs)]),
                        "shp",
                        paste0("cb_", yrs[length(yrs)], "_us_state_500k.zip")),
              file.path(dat_dir,
                        "tiger",
                        paste0("cb_", yrs[length(yrs)], "_us_state_500k.zip")),
              mode = "wb")

## -----------------------------------------------------------------------------
## gazetteer for zcta and counties
## -----------------------------------------------------------------------------

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "gaz"), showWarnings = FALSE)

## base url
base_url <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer"

## -------------------------------------
## zcta
## -------------------------------------

walk(yrs,
     ~ download.file(file.path(base_url,
                               paste0(.x, "_Gazetteer"),
                               paste0(.x, "_Gaz_zcta_national.zip")),
                     file.path(dat_dir,
                               "gaz",
                               paste0(.x, "_Gaz_zcta_national.zip")),
                     mode = "wb"))

## -------------------------------------
## counties
## -------------------------------------

walk(yrs,
     ~ download.file(file.path(base_url,
                               paste0(.x, "_Gazetteer"),
                               paste0(.x, "_Gaz_counties_national.zip")),
                     file.path(dat_dir,
                               "gaz",
                               paste0(.x, "_Gaz_counties_national.zip")),
                     mode = "wb"))

## -----------------------------------------------------------------------------
## unemployment data
## -----------------------------------------------------------------------------

## using BLS FTP
## period := M13 (annual average)
## measure := 3 (unemployment rate)
## series := LAUCN (county) + FIPS (01001)

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "bls"), showWarnings = FALSE)

## base url
base_url <- "https://download.bls.gov/pub/time.series/la/la.data.64.County"

## download and clean
## con <- curl(base_url, "rb")

out <- read_delim(file.path(dat_dir, "bls/la.data.64.County"), delim = "\t",
                  col_names = c("series_id", "year", "period", "value", "footnote_codes"),
                  skip = 1,
                  trim_ws = TRUE) |>
  mutate(fips = str_sub(series_id, 6, 10),
         measure = str_sub(series_id, start = -1) |> as.integer()) |>
  filter(period == "M13", year %in% yrs, measure == 3) |>
  select(fips, year, unemp_rate = value)
close(con)

## writing to disk
write_csv(out, file.path(dat_dir, "bls", "bls_lauc.csv"))

## -----------------------------------------------------------------------------
## poverty data
## -----------------------------------------------------------------------------

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "saipe"), showWarnings = FALSE)

## base url
base_url <- "https://www2.census.gov/programs-surveys/saipe/datasets"

## loop through
walk(yrs[-length(yrs)],
     ~ download.file(file.path(base_url,
                               .x,
                               paste0(.x, "-state-and-county"),
                               paste0("est", substr(.x,3,4), "all.xls")),
                     file.path(dat_dir,
                               "saipe",
                               paste0("est", substr(.x,3,4), "all.xls")),
                     mode = "wb"))

## -----------------------------------------------------------------------------
## ARC data
## -----------------------------------------------------------------------------

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "arc"), showWarnings = FALSE)

## base url
base_url <- "https://www.arc.gov/wp-content/uploads/2021/11"

## download
download.file(file.path(base_url,
                        "Appalachian-Counties-Served-by-ARC_2021.xlsx"),
              file.path(dat_dir,
                        "arc",
                        "Appalachian-Counties-Served-by-ARC_2021.xlsx"),
              mode = "wb")

## -----------------------------------------------------------------------------
## IPEDS data
## -----------------------------------------------------------------------------

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "ipeds"), showWarnings = FALSE)

## base url
base_url <- "https://nces.ed.gov/ipeds/datacenter/data"

## download
walk(yrs,
     ~ download.file(file.path(base_url,
                               paste0("HD", .x, ".zip")),
                     file.path(dat_dir,
                               "ipeds",
                               paste0("HD", .x, ".zip")),
                     mode = "wb"))

## -----------------------------------------------------------------------------
## end script
################################################################################
