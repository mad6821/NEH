## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] get_data.R
## [ AUTH ] Benjamin Skinner; bskinner@neh.gov & Maya Dalton; mdalton@neh.gov
## [ INIT ] 02 October 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse")
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

## make directory if it doesn't exist
dir.create(file.path(dat_dir, "bls"), showWarnings = FALSE)

## base url
base_url <- "https://www.bls.gov/lau"

## have to read b/c BLS won't allow direct downloads
out <- map(yrs,
           ~ read_fwf(file.path(base_url,
                                paste0("laucnty", substr(.x, 3, 4), ".txt")),
                      fwf_cols(lauc = c(1,15),
                               stfips = c(19,20),
                               ctfips = c(26,28),
                               name = c(32,81),
                               year = c(82,85),
                               labor = c(86,99),
                               employed = c(100,112),
                               unemployed_level = c(113,123),
                               unemployed_rate = c(124,132)),
                      skip = 6) |>
             filter(!is.na(stfips))) |>
  set_names(paste0("bls_lauc_", yrs))

## writing to disk
walk2(out,
      names(out),
      ~ write_csv(.x, file.path(dat_dir, "bls", paste0(.y, ".csv"))))

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

## Source code from https://github.com/btskinner/downloadipeds

source("downloadipeds.R")

## -----------------------------------------------------------------------------
## end script
################################################################################
