###############################################
# Data Cleaning for Intern Project
# Maya A. Dalton
# August 2024
###############################################
rm(list=ls())

# Load libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(usmap)
library(sp)

# Set working directory (may have to set own path file on personal device)
setwd("~/MD_InternProject")

###############################################
# Cleaning, subsetting NEH grant data for  
# Appalachian States
###############################################

# Load dataset for 2020s
grants_20s <- read_csv("NEH Grant Data/NEH_Grants2020s.csv")

# Clean for Appalachian states
grants.app <- grants_20s %>%
  subset(InstState %in% c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN", "VA", "WV"))

write.csv(grants.app, "NEH Grant Data/app_data_20s.csv", row.names=FALSE) # export Appalachian states for cleaning

# Load dataset for 2010s
grants_10s <- read_csv("NEH Grant Data/NEH_Grants2010s.csv")

# Clean for Appalachian states
grants.app <- grants_10s %>%
  subset(InstState %in% c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN", "VA", "WV")) %>%
  subset(YearAwarded >= 2018)

write.csv(grants.app, "NEH Grant Data/app_data_10s.csv", row.names=FALSE) # export Appalachian states for cleaning

###############################################
# Cleaning, subsetting BLS economic data for  
# Appalachian States and merging 2018-2023
###############################################
# Load in unemployment datasets 2018-2023
bls_county_18 <- read_excel("Econ Data/bls_county_18.xlsx")
bls_county_19 <- read_excel("Econ Data/bls_county_19.xlsx")
bls_county_20 <- read_excel("Econ Data/bls_county_20.xlsx")
bls_county_21 <- read_excel("Econ Data/bls_county_21.xlsx")
bls_county_22 <- read_excel("Econ Data/bls_county_22.xlsx")
bls_county_23 <- read_excel("Econ Data/bls_county_23.xlsx")

# Full join of all datasets
bls_county <- full_join(bls_county_18, full_join(bls_county_19, full_join(bls_county_20, full_join(bls_county_21, full_join(bls_county_22, bls_county_23)))))
bls_county <- separate(bls_county, County_State, c("County" , "State"), ", ") # Separate state and county

bls_county$FIPS <- paste(bls_county$State_FIPS, bls_county$County_FIPS, sep="") # Combine FIPS
bls_county <- bls_county %>% # Drop DC and NAs
  drop_na()

bls_county.c <- bls_county %>% # Remove 'county' in text
  mutate(County = str_remove_all(County, " County")) 

bls_county.c <- bls_county.c %>% # Subset for Appalachian states
  subset(State %in% c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN", "VA", "WV"))

bls_county.c$Year <- as.numeric(bls_county.c$Year) # Convert year to numeric for merge
bls_county.c <- bls_county.c %>% select(FIPS, County, State, Year, Unemp_Rate) # Reorder columns

write.csv(bls_county.c, "Econ Data/bls_clean.csv", row.names=FALSE) # export unemp data

# Load in poverty dataset 2018-2022
census_poverty <- read_excel("Econ Data/census_poverty.xlsx")
colnames(census_poverty) <- c("Year", "FIPS", "County", "Poverty_Rate") # Change column names
census_poverty$FIPS <- as.character(census_poverty$FIPS) # Convert FIPS to character for merge

census_poverty$FIPS <- ifelse(nchar(census_poverty$FIPS) == 4,
                              paste0("0", census_poverty$FIPS),
                              census_poverty$FIPS)

census_poverty <- census_poverty %>% # Remove 'county' in text
  mutate(County = str_remove_all(County, " County")) 

census_poverty <- census_poverty %>% select(FIPS, County, Year, Poverty_Rate) # Reorder columns
write.csv(census_poverty, "Econ Data/census_clean.csv", row.names=FALSE) # export poverty data

econ_full <- left_join(bls_county.c, census_poverty, by=c("FIPS", "Year", "County"))

# Create new column of state full names using crosswalk table
st_crosswalk <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC"))

econ_full <- left_join(st_crosswalk, econ_full, by=c("abb"="State")) # Merge
colnames(econ_full) <- c("State", "St_Abbr", "FIPS", "County", "Year", "Unemp_Rate", "Poverty_Rate") # Column rename

econ_full <- econ_full %>% select(FIPS, State, St_Abbr, County, Year, Unemp_Rate, Poverty_Rate) # Reorder columns
write.csv(econ_full, "Econ Data/econ_df.csv", row.names=FALSE) # export all econ data

# Load in ARC Data for counties in Appalachian region
arc_clean <- read_csv("arc_clean.csv")
arc_clean <- arc_clean %>%
  mutate_if(is.character, str_trim) # Trim whitespace off FIPS

appalachia_full <- subset(econ_full, FIPS %in% arc_clean$FIPS) # Subset econ data for App counties

write.csv(appalachia_full, "Econ Data/appalachian_econ_df.csv", row.names=FALSE)

###############################################
# Merge NEH and BLS datasets
###############################################
# Load in NEH data from 2018-2023
app_data_clean <- read_excel("NEH Grant Data/neh_appalachian_data.xlsx")

app_data_clean <- app_data_clean %>%
  mutate(Discipline = case_when(
    Discipline == "NA" ~ "Unknown",
    .default = as.character(Discipline)
  ))

df <- left_join(app_data_clean, appalachia_full, by=c("County", "State"="St_Abbr", "YearAwarded"="Year"))
names(df)[names(df) == "State"] <- "St_Abbr"
names(df)[names(df) == "State.y"] <- "State"

write.csv(df, "df_clean.csv", row.names=FALSE)



