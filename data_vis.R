###############################################
# Data Visualization for Intern Project
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
library(sf)
library(maps)
library(formattable)
library(plotly)

# Set working directory (may have to set own path file on personal device)
setwd("~/MD_InternProject")

# Load datasets
app_econ_df <- read_csv("Econ Data/appalachian_econ_df.csv") # Census economic data for merging
arc_clean <- read_csv("arc_clean.csv") # ARC Data for counties in Appalachia
df_clean <- read_csv("df_clean.csv") # NEH merge with Econ Stats
df_clean <- df_clean %>%
  mutate_if(is.character, str_trim) # Trim whitespace off names

###############################################
###############################################
#
# Initial plots
#
###############################################
###############################################

df_clean %>% # Avg Poverty Rate
  group_by(YearAwarded) %>%
  mutate(avg_pov = mean(Poverty_Rate, na.rm=T)) %>%
  ggplot(aes(x=YearAwarded, y=avg_pov)) + 
  stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
  scale_y_continuous(labels = scales::comma) + 
  labs(x="Year", y="Avg. Poverty Rate (%)", 
       title="Figure 1a. Appalachian Poverty Rate Over Time") +
  theme_minimal()

df_clean %>% # Avg Unemployment Rate
  group_by(YearAwarded) %>%
  mutate(avg_unemp = mean(Unemp_Rate, na.rm=T)) %>%
  ggplot(aes(x=YearAwarded, y=avg_unemp)) + 
  stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
  scale_y_continuous(labels = scales::comma) + 
  labs(x="Year", y="Avg. Unemployment Rate (%)", 
       title="Figure 2b. Appalachian Unemployment Rate Over Time") +
  theme_minimal()

df_clean %>% # Total NEH Awarded 
  group_by(YearAwarded) %>%
  mutate(total_award = sum(AwardOutright)) %>%
  ggplot(aes(x=YearAwarded, y=total_award)) + 
  stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
  scale_y_continuous(labels = scales::comma) + 
  labs(x="Year", y="Total NEH Awarded (USD)", 
       title="Figure 2. NEH Awards to Appalachia Over Time") +
  theme_minimal()

df_clean %>% # Poverty Rate & NEH Funding
  ggplot(aes(x=Poverty_Rate, y=AwardOutright)) + 
  geom_bar(fill="slateblue", stat="identity", width=0.6, alpha=0.4) +
  scale_y_continuous(labels = scales::comma) + 
  labs(y="NEH Funding", x="Poverty Rate (%)", 
       title="Figure 3a. NEH Funding and Poverty Rates\nin Appalachia") +
  theme_minimal()

df_clean %>% # Unemployment Rate & NEH Funding
  ggplot(aes(x=Unemp_Rate, y=AwardOutright)) + 
  geom_bar(fill="slateblue", stat="identity", width=0.4, alpha=0.4) +
  scale_y_continuous(labels = scales::comma) + 
  labs(y="NEH Funding", x="Unemployment Rate (%)", 
       title="Figure 3b. NEH Funding and Unemployment Rates\nin Appalachia") +
  theme_minimal()

df_clean %>% # Unemployment & Poverty - APPENDIX
  ggplot(aes(x=Unemp_Rate, y=Poverty_Rate)) + 
  geom_point(fill="black") +
  scale_y_continuous(labels = scales::comma) + 
  labs(y="Poverty Rate (%)", x="Unemployment Rate (%)", 
       title="Figure 4. Poverty and Unemployment Rates in Appalachia") +
  theme_minimal()

###############################################
###############################################
#
# Regional Maps
#
###############################################
###############################################
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE)) # STATE DATA
app_states <- unique(tolower(df_clean$State)) # List of lower case App states for merge
app_counties <- unique(df_clean$County) # List of counties
appalachia <- usa %>%
  subset(ID %in% app_states) # Subset sf data to App region STATES ONLY

app_states <- unique(tolower(df_clean$State)) # List of lower case App states for merge
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) # COUNTIES DATA
counties <- separate(counties, ID, c("state", "county"), ",") # Separate into two columns
counties <- subset(counties, state %in% tolower(app_states)) # Subset sf to ALL COUNTIES & STATES in region

###############################################
# Cleaning counties to Appalachia
###############################################
counties <- counties %>% # Fix anomalies
  mutate(county = case_when( 
    county == "de kalb" ~ "dekalb",
    county == "st clair" ~ "st. clair",
    .default = as.character(county)
  ))

# Subsetting Econ and sf data in a longer format, then re-binding together to avoid counties with the same
# name in different states overlapping one another (i.e., Jefferson Co. in AL, OH, and WV)

# Alabama
econ_al <- subset(app_econ_df, State == "Alabama")
counties_al <- subset(counties, county %in% tolower(econ_al$County) & state == "alabama")

# Georgia
econ_ga <- subset(app_econ_df, State == "Georgia")
counties_ga <- subset(counties, county %in% tolower(econ_ga$County) & state == "georgia")

# Kentucky
econ_ky <- subset(app_econ_df, State == "Kentucky")
counties_ky <- subset(counties, county %in% tolower(econ_ky$County) & state == "kentucky")

# Maryland
econ_md <- subset(app_econ_df, State == "Maryland")
counties_md <- subset(counties, county %in% tolower(econ_md$County) & state == "maryland")

# Mississippi
econ_ms <- subset(app_econ_df, State == "Mississippi")
counties_ms <- subset(counties, county %in% tolower(econ_ms$County) & state == "mississippi")

# New York
econ_ny <- subset(app_econ_df, State == "New York")
counties_ny <- subset(counties, county %in% tolower(econ_ny$County) & state == "new york")

# North Carolina
econ_nc <- subset(app_econ_df, State == "North Carolina")
counties_nc <- subset(counties, county %in% tolower(econ_nc$County) & state == "north carolina")

# Ohio
econ_oh <- subset(app_econ_df, State == "Ohio")
counties_oh <- subset(counties, county %in% tolower(econ_oh$County) & state == "ohio")

# Pennsylvania
econ_pa <- subset(app_econ_df, State == "Pennsylvania")
counties_pa <- subset(counties, county %in% tolower(econ_pa$County) & state == "pennsylvania")

# South Carolina
econ_sc <- subset(app_econ_df, State == "South Carolina")
counties_sc <- subset(counties, county %in% tolower(econ_sc$County) & state == "south carolina")

# Tennessee
econ_tn <- subset(app_econ_df, State == "Tennessee")
counties_tn <- subset(counties, county %in% tolower(econ_tn$County) & state == "tennessee")

# Virginia
econ_va <- subset(app_econ_df, State == "Virginia")
counties_va <- subset(counties, county %in% tolower(econ_va$County) & state == "virginia")

# West Virginia
econ_wv <- subset(app_econ_df, State == "West Virginia")
counties_wv <- subset(counties, county %in% tolower(econ_wv$County) & state == "west virginia")

counties_app <- rbind(counties_al, counties_ga, counties_ky, counties_md, counties_ms, counties_nc,
                      counties_ny, counties_oh, counties_pa, counties_sc, counties_tn, counties_va, counties_wv)

df_clean$county <- tolower(df_clean$County) # Lowercase county in NEH/Econ merge
df_clean$state <- tolower(df_clean$State) # Lowercase state in NEH/Econ merge

app_merge <- left_join(counties_app, df_clean, by = c("state", "county"))

###############################################
# NEH Funding in Appalachia (2018-2023) - APPENDIX
###############################################

t <- app_merge %>%
  group_by(County, State) %>%
  mutate(total_award = sum(OriginalAmount, na.rm=T)) # Subset df for total NEH award

ggplot() +
  geom_sf(data = counties, fill = "grey", color = "black", alpha=0.2) + # ALL Counties
  geom_sf(data = counties_app, fill = "yellow", color = "black", alpha=0.2) + # App counties
  geom_sf(data = t, aes(fill=as.numeric(total_award)), color="black", alpha=0.4) + # NEH Data
  geom_sf(data = appalachia, fill = "white", color = "black", alpha=0.1, linewidth=0.5) + # State borders
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  ggthemes::theme_map()+
  labs(title="Figure 5. Appalachian Counties with NEH Funding", fill="Award Amount")+
  scale_fill_continuous(labels=scales::label_comma()) +
  theme(legend.position = "right")

###############################################
# Appalachia Average Poverty Rate (2018-2022) - APPENDIX
###############################################

t <- app_merge %>%
  group_by(county, state) %>%
  mutate(avg_pov = mean(Poverty_Rate, na.rm=T)) # Subset df for avg poverty rate

ggplot() +
  geom_sf(data = counties, fill = "grey", color = "black", alpha=0.2) +
  geom_sf(data = counties_app, color = "black", alpha=0.4) +
  geom_sf(data = t, aes(fill=as.numeric(avg_pov)), color="black", alpha=0.4) +
  geom_sf(data = appalachia, fill = "white", color = "black", alpha=0.1, linewidth=0.5) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  ggthemes::theme_map()+
  labs(title="Figure 6. Appalachian Poverty Rates", fill="Poverty Rate")+
  scale_fill_viridis_c(labels=scales::label_comma()) +
  theme(legend.position = "right")

###############################################
# Appalachia Average Unemployment Rate (2018-2023) - APPENDIX
###############################################

t <- app_merge %>%
  group_by(county, state) %>%
  mutate(avg_unemp = mean(Unemp_Rate, na.rm=T)) # Subset df for avg unemp rate

ggplot() +
  geom_sf(data = counties, fill = "grey", color = "black", alpha=0.2) +
  geom_sf(data = counties_app, color = "black", alpha=0.4) +
  geom_sf(data = t, aes(fill=as.numeric(avg_unemp)), color="black", alpha=0.4) +
  geom_sf(data = appalachia, fill = "white", color = "black", alpha=0.1, linewidth=0.5) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  ggthemes::theme_map()+
  labs(title="Figure 7. Appalachian Unemployment Rates", fill="Unemployment Rate")+
  scale_fill_viridis_c(labels=scales::label_comma()) +
  theme(legend.position = "right")

###############################################
###############################################
#
# Summary Tables 
#
###############################################
###############################################

###############################################
# Entire Region (2018-2023)
###############################################

#### Award Details
summary_table <- df_clean %>%
  summarise(
    Total_Grants = n(),
    Total_Amount = sum(AwardOutright, na.rm = TRUE),
    Average_Amount = mean(AwardOutright, na.rm = TRUE),
    Max_Amount = max(AwardOutright, na.rm = TRUE),
  ) %>%
  select(Total_Grants, Total_Amount, Average_Amount, Max_Amount)

summary_table$Total_Amount <- paste('$',formatC(summary_table$Total_Amount, big.mark=',', format = 'f', digits=2))
summary_table$Average_Amount <- paste('$',formatC(summary_table$Average_Amount, big.mark=',', format = 'f', digits=2))
summary_table$Max_Amount <- paste('$',formatC(summary_table$Max_Amount, big.mark=',', format = 'f', digits=2))

colnames(summary_table) <- c("Total Grants", "Total Award", "Average", "Max")
summary_table[] <- lapply(summary_table, as.character)
print(summary_table)

#### Discipline Details
disc_table <- df_clean %>%
  group_by(Discipline) %>%
  summarise(Count = n(), Awarded = sum(AwardOutright)) %>%
  arrange(desc(Count))

disc_table$Awarded <- paste('$',formatC(disc_table$Awarded, big.mark=',', format = 'f', digits=2))

print(disc_table)

#### Division Details
div_table <- df_clean %>%
  group_by(Division) %>%
  summarise(Count = n(), Awarded = sum(AwardOutright)) %>%
  arrange(desc(Count)) %>%
  head(5)

div_table$Awarded <- paste('$',formatC(div_table$Awarded, big.mark=',', format = 'f', digits=2))

print(div_table)

#### Organization Details
org_table <- df_clean %>%
  group_by(OrganizationType) %>%
  summarise(Count = n(), Awarded = sum(AwardOutright)) %>%
  arrange(desc(Count)) %>%
  head(10)

org_table$Awarded <- paste('$',formatC(org_table$Awarded, big.mark=',', format = 'f', digits=2))

print(org_table)

#### Awardee Details by state
# Want to add CARES designation... if CARES == 1, paste * next to institution name? 
award_table <- df_clean %>%
  mutate(total_award = ifelse(OriginalAmount == 0, ApprovedOutright, OriginalAmount)) %>%
  select(Institution, City, State, County, YearAwarded, ProjectTitle, total_award) 

award_table$total_award <- paste('$',formatC(award_table$total_award, big.mark=',', format = 'f', digits=2))

# Format the table
colnames(award_table) <- c("Institution", "City", "State", "County", 
                           "Year", "Project Title", "Award Amount")
award_table[] <- lapply(award_table, as.character)
print(award_table)



