## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] data_vis.R
## [ AUTH ] Benjamin Skinner; bskinner@neh.gov & Maya Dalton; mdalton@neh.gov
## [ INIT ] 09 October 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "readr", "sf", "usmap", "maps", 
          "formattable", "crosswalkr", "plyr", "plotly", "ggplot2")

sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
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

## -----------------------------------------------------------------------------
## Analysis data frame
## -----------------------------------------------------------------------------

df <- read_csv(file.path(dat_dir, "analysis.csv")) |>
  distinct()

df_arc <- read_excel(list.files(file.path(dat_dir, "arc"), full.names = TRUE),
                     skip = 4) |>
  rename_all(tolower) |>
  mutate(appalachia = 1) |>
  mutate(county = case_when( # Fixing VA anomalies
    county == "Alleghany + Covington city" ~ "Alleghany",
    county == "Wise + Norton city" ~ "Wise",
    county == "Washington + Bristol city" ~ "Washington",
    county == "Carroll + Galax city" ~ "Carroll",
    county == "Henry + Martinsville city" ~ "Henry",
    county == "Montgomery + Radford city" ~ "Montgomery",
    county == "Rockbridge + Buena Vista city + Lexington city" ~ "Rockbridge",
    county == "St. Clair" ~ "St Clair",
    .default = as.character(county)
  ))

## -----------------------------------------------------------------------------
## Initial Trend Plots
## -----------------------------------------------------------------------------

## Avg Poverty Rate
png(file.path(fig_dir, "pov_rate.png"), res=100)
df |> 
  subset(appalachia == 1) |>
  group_by(yearawarded) |>
  mutate(avg_pov = mean(poverty_rate, na.rm=T)) |>
  ggplot(aes(x=yearawarded, y=avg_pov)) + 
    stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="Year", y="Avg. Poverty Rate (%)", 
         title="Figure 1a. Appalachian Poverty Rate Over Time") +
    theme_minimal()
dev.off()

## Avg Unemployment Rate
png(file.path(fig_dir, "unemp_rate.png"), res=90)
df |>
  subset(appalachia == 1) |>
  group_by(yearawarded) |>
  mutate(avg_unemp = mean(unemp_rate, na.rm=T)) |>
  ggplot(aes(x=yearawarded, y=avg_unemp)) + 
    stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="Year", y="Avg. Unemployment Rate (%)", 
         title="Figure 1b. Appalachian Unemployment Rate Over Time") +
    theme_minimal()
dev.off()

## Total NEH Awarded 
png(file.path(fig_dir, "neh_awards.png"), res=90)
df |>
  subset(appalachia == 1) |>
  group_by(yearawarded) |>
  mutate(total_award = sum(ao)) |>
  ggplot(aes(x=yearawarded, y=total_award)) + 
    stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="Year", y="Total NEH Awarded (USD)", 
         title="Figure 2. NEH Awards to Appalachia Over Time") +
    theme_minimal()
dev.off()

## Poverty Rate & NEH Funding
png(file.path(fig_dir, "poverty_awards.png"), res=90)
df |>
  subset(appalachia == 1) |>
  ggplot(aes(x=poverty_rate, y=ao)) + 
    geom_bar(fill="slateblue", stat="identity", width=0.6, alpha=0.4) +
    scale_y_continuous(labels = scales::comma) + 
    labs(y="NEH Funding", x="Poverty Rate (%)", 
         title="Figure 3a. NEH Funding and Poverty Rates\nin Appalachia") +
    theme_minimal()
dev.off()

## Unemployment Rate & NEH Funding
png(file.path(fig_dir, "unemp_awards.png"), res=90)
df |>
  subset(appalachia == 1) |>
  ggplot(aes(x=unemp_rate, y=ao)) + 
    geom_bar(fill="slateblue", stat="identity", width=0.4, alpha=0.4) +
    scale_y_continuous(labels = scales::comma) + 
    labs(y="NEH Funding", x="Unemployment Rate (%)", 
         title="Figure 3b. NEH Funding and Unemployment Rates\nin Appalachia") +
    theme_minimal()
dev.off()

## -----------------------------------------------------------------------------
## Regional Maps
## -----------------------------------------------------------------------------

## TODO 
# Issue with duplicate county names in different states (i.e. Barbour Co in Alabama isn't in region
# but Barbour Co. in WV is)... need to merge on FIPS somehow 

## NEH Funding in Appalachia (2018-2023) - APPENDIX

# Subset main df to only Appalachia
df_app <- df |>
  subset(appalachia == 1)

# Get the state & counties boundaries data from the maps package
st_app <- map_data("state", region = tolower(df_app$stname)) # State boundaries separate
st_ct_map <- map_data("county", region=tolower(df_app$stname)) # State & county boundaries
ct_app <- st_ct_map |> # subset for all appalachian counties
  group_by(region, subregion) |>
  subset(region %in% unique(tolower(df_arc$state)) & 
           paste(region, subregion) %in% 
           paste(tolower(df_arc$state), tolower(df_arc$county)))


# Plot 
ggplot() +
  geom_polygon(data = st_ct_map, aes(x = long, y = lat, group = group),  # Add county boundaries
               fill = "white", color = "black", size = 0.2) +
  geom_polygon(data = ct_app, aes(x = long, y = lat, group = group),  # Add counties in region
               fill = "lightblue", color = "black", size = 0.2) +
  geom_polygon(data = st_app, aes(x = long, y = lat, group = group),  # Add state boundaries
               fill = NA, color = "black", size = 0.5) +
  theme_void()

## Appalachia Average Poverty Rate (2018-2022) - APPENDIX


## Appalachia Average Unemployment Rate (2018-2023) - APPENDIX


## -----------------------------------------------------------------------------
## Summary Tables
## -----------------------------------------------------------------------------

## Award Overview
summary_table <- df |>
  subset(appalachia == 1) |>
  summarise(
    Total_Grants = n(),
    Total_Amount = sum(ao, na.rm = TRUE),
    Average_Amount = mean(ao, na.rm = TRUE),
    Max_Amount = max(ao, na.rm = TRUE)
  ) |>
  select(Total_Grants, Total_Amount, Average_Amount, Max_Amount)

summary_table$Total_Amount <- paste('$',formatC(summary_table$Total_Amount, big.mark=',', format = 'f', digits=2))
summary_table$Average_Amount <- paste('$',formatC(summary_table$Average_Amount, big.mark=',', format = 'f', digits=2))
summary_table$Max_Amount <- paste('$',formatC(summary_table$Max_Amount, big.mark=',', format = 'f', digits=2))

colnames(summary_table) <- c("Total Grants", "Total Awarded", "Average", "Max")
summary_table[] <- lapply(summary_table, as.character)
print(summary_table)

## Disciplines Overview
disc_table <- df |>
  subset(appalachia == 1) |>
  group_by(newdiscipline) |>
  summarise(Count = n(), Awarded = sum(ao)) |>
  arrange(desc(Count))

disc_table$Awarded <- paste('$',formatC(disc_table$Awarded, big.mark=',', format = 'f', digits=2))

print(disc_table)

## Division Overview
div_table <- df |>
  subset(appalachia == 1) |>
  group_by(division) |>
  summarise(Count = n(), Awarded = sum(ao)) |>
  arrange(desc(Count)) |>
  head(5)

div_table$Awarded <- paste('$',formatC(div_table$Awarded, big.mark=',', format = 'f', digits=2))

print(div_table)

## Organizations Overview
org_table <- df |>
  subset(appalachia == 1) |>
  group_by(orgtype) |>
  summarise(Count = n(), Awarded = sum(ao)) |>
  arrange(desc(Awarded))

org_table$Awarded <- paste('$',formatC(org_table$Awarded, big.mark=',', format = 'f', digits=2))

print(org_table)

## IPEDS Overview
ipeds_table <- df |>
  subset(appalachia == 1) |>
  drop_na(ccb) |>
  mutate(ccnames = case_when(
    ccb == 1 ~ "Associate’s - Public", 
    ccb == 2 ~ "Associate’s - Private",
    ccb == 3 ~ "Research University (Very High)",
    ccb == 4 ~ "Research University (High)",
    ccb == 5 ~ "Doctoral/Research University",
    ccb == 6 ~ "Master’s (Large)",
    ccb == 7 ~ "Master’s (Medium)",
    ccb == 8 ~ "Master’s (Small)",
    ccb == 9 ~ "Baccalaureate Colleges",
    ccb == 10 ~ "Faith-Related Institutions",
    ccb == 11 ~ "Medical Schools",
    ccb == 12 ~ "Other health profession schools",
    ccb == 13 ~ "Engineering Schools",
    ccb == 14 ~ "Other tech-related schools",
    ccb == 15 ~ "Business/Management Schools",
    ccb == 16 ~ "Art, Music, and Design Schools",
    ccb == 17 ~ "Law Schools",
    ccb == 18 ~ "Other Special-Focus Institutions",
    ccb == 19 ~ "Tribal Colleges"
  )) |>
  group_by(ccnames) |>
  summarise(Count = n(), Awarded = sum(ao)) |>
  arrange(desc(Awarded))

ipeds_table$Awarded <- paste('$',formatC(ipeds_table$Awarded, big.mark=',', format = 'f', digits=2))

print(ipeds_table)

## HBCU Overview
hbcu_table <- df |>
  subset(hbcu == 1 & appalachia == 1) |>
  group_by(hbcu) %>% # need to add in HBCU and Tribals
  summarise(Count = n(), Awarded = sum(ao)) |>
  arrange(desc(Awarded))

hbcu_table$Awarded <- paste('$',formatC(hbcu_table$Awarded, big.mark=',', format = 'f', digits=2))

print(hbcu_table[,2:3])
