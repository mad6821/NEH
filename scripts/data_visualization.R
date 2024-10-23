## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] data_vis.R
## [ AUTH ] Benjamin Skinner; bskinner@neh.gov & Maya Dalton; mdalton@neh.gov
## [ INIT ] 18 October 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "sf", "plotly", "readxl")

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

df <- read_csv(file.path(dat_dir, "analysis.csv")) %>%
  distinct() # need distinct obs, because there are some duplicates

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

## -------------------------------------
## average poverty rate
## -------------------------------------

g <- df |>
  filter(appalachia == 1, yearawarded < 2023) |>
  group_by(yearawarded) |>
  mutate(avg_pov = mean(poverty_rate)) |>
  ggplot(aes(x = yearawarded, y = avg_pov)) +
  stat_smooth(color = "black", linewidth = 0.5, se = FALSE, method = "loess") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = "Year",
       y = "Avg. Poverty Rate (%)",
       title = "Appalachian Poverty Rate Over Time") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())

ggsave(filename = file.path(fig_dir, "pov_rate.png"),
       g,
       units = "in",
       height = 4,
       width = 6,
       dpi = "retina")

## -------------------------------------
## average unemployment rate
## -------------------------------------

g <- df |>
  filter(appalachia == 1) |>
  group_by(yearawarded) |>
  mutate(avg_unemp = mean(unemp_rate)) |>
  ggplot(aes(x = yearawarded, y = avg_unemp)) +
  stat_smooth(color = "black", linewidth = 0.5, se = FALSE, method = "loess") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = "Year",
       y = "Avg. Unemployment Rate (%)",
       title = "Appalachian Unemployment Rate Over Time") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())

ggsave(filename = file.path(fig_dir, "unemp_rate.png"),
       g,
       units = "in",
       height = 4,
       width = 6,
       dpi = "retina")

## -------------------------------------
## total NEH awarded 
## -------------------------------------

g <- df |>
  filter(appalachia == 1) |>
  group_by(yearawarded) |>
  mutate(total_award = sum(ao)) |>
  ggplot(aes(x = yearawarded, y = total_award)) +
  stat_smooth(color = "black", linewidth = 0.5, se = F, method = "loess") +
  scale_y_continuous(labels = scales::label_comma(scale = 1)) +
  labs(x = "Year",
       y = "Total NEH Awarded (USD)",
       title = "NEH Awards to Appalachia Over Time") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())

ggsave(filename = file.path(fig_dir, "neh_awards.png"),
       g,
       units = "in",
       height = 4,
       width = 6,
       dpi = "retina")

## -------------------------------------
## poverty rate and NEH funding
## -------------------------------------

g <- df |>
  filter(appalachia == 1) |>
  ggplot(aes(x=poverty_rate, y=ao)) +
  geom_bar(fill="slateblue", stat="identity", width=0.6, alpha=0.4) +
  scale_y_continuous(labels = scales::label_comma(scale = 1)) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = "Poverty Rate (%)",
       y = "Total NEH Awarded (USD)",
       title = "NEH Funding and Poverty Rates in Appalachia") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())

ggsave(filename = file.path(fig_dir, "poverty_awards.png"),
       g,
       units = "in",
       height = 4,
       width = 6,
       dpi = "retina")

## -------------------------------------
## unemployment rates and NEH funding
## -------------------------------------

g <- df |>
  filter(appalachia == 1) |>
  ggplot(aes(x=unemp_rate, y=ao)) +
  geom_bar(fill="slateblue", stat="identity", width=0.6, alpha=0.4) +
  scale_y_continuous(labels = scales::label_comma(scale = 1)) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = "Poverty Rate (%)",
       y = "Total NEH Awarded (USD)",
       title = "NEH Funding and Unemployment Rates in Appalachia") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank())

ggsave(filename = file.path(fig_dir, "unemp_awards.png"),
       g,
       units = "in",
       height = 4,
       width = 6,
       dpi = "retina")

## -----------------------------------------------------------------------------
## Regional Maps
## -----------------------------------------------------------------------------

# Subset main df to only Appalachia & make some variables
df_app <- df |>
  subset(appalachia == 1) |>
  group_by(county, stname) |>
  mutate(region = tolower(stname), # Lowercase states for merge
         subregion = tolower(county), # Lowercase counties for merge
         total_award = sum(ao, na.rm=T), # Total funding per county
         avg_pov = mean(poverty_rate, na.rm=T), # Avg poverty rate per county
         avg_unemp = mean(unemp_rate, na.rm=T)) # Avg unemp rate per county

# Get the state & counties boundaries data from the maps package
st_app <- map_data("state", region = tolower(df_app$stname)) # State boundaries separate
st_ct_map <- map_data("county", region=tolower(df_app$stname)) # State & county boundaries

ct_app <- st_ct_map |> # subset for all appalachian counties in ARC
  group_by(region, subregion) |>
  subset(region %in% unique(tolower(df_arc$state)) & 
           paste(region, subregion) %in% 
           paste(tolower(df_arc$state), tolower(df_arc$county)))

# Merge counties data w/ NEH data for fill
df_app_ct <- left_join(ct_app, df_app, by=c("region", "subregion")) %>%
  drop_na(ao) 

## -------------------------------------
## NEH Funding (2018-2023) - APPENDIX
## -------------------------------------
g <- ggplot() +
  geom_polygon(data = st_ct_map, aes(x = long, y = lat, group = group),  # Add county boundaries
               fill = "white", color = "black", size = 0.2) +
  geom_polygon(data = ct_app, aes(x = long, y = lat, group = group),  # Add counties in region
               fill = "grey", color = "black", size = 0.2) +
  geom_polygon(data = st_app, aes(x = long, y = lat, group = group),  # Add state boundaries
               fill = NA, color = "black", size = 0.5) +
  geom_polygon(data = df_app_ct, aes(x = long, y = lat.x, group=group, # Add NEH funding fill
                                     fill=as.numeric(total_award)), 
               color="black", size = 0.2, alpha=0.8) +
  scale_fill_distiller(type = "seq", palette = "Reds", labels=scales::label_comma()) + 
  labs(title="Appalachian Counties with NEH Funding", 
       fill="Award Amount")+
  ggthemes::theme_map()+
  theme(legend.position = "right")

ggsave(filename = file.path(fig_dir, "neh_map.png"),
       g,
       units = "in",
       height = 5,
       width = 6,
       dpi = "retina")

## -------------------------------------
## Avg Poverty Rate (2018-2022) 
## -------------------------------------

g <- ggplot() +
  geom_polygon(data = st_ct_map, aes(x = long, y = lat, group = group),  # Add county boundaries
               fill = "white", color = "black", size = 0.2) +
  geom_polygon(data = ct_app, aes(x = long, y = lat, group = group),  # Add counties in region
               fill = "grey", color = "black", size = 0.2) +
  geom_polygon(data = st_app, aes(x = long, y = lat, group = group),  # Add state boundaries
               fill = NA, color = "black", size = 0.5) +
  geom_polygon(data = df_app_ct, aes(x = long, y = lat.x, group=group, # Add NEH funding fill
                                     fill=as.numeric(avg_pov)), 
               color="black", size = 0.2, alpha=0.8) +
  scale_fill_distiller(type = "seq", palette = "Reds", labels = scales::label_percent(scale = 1)) + 
  labs(title="Appalachian Poverty Rates", fill="Poverty Rate")+
  ggthemes::theme_map()+
  theme(legend.position = "right")

ggsave(filename = file.path(fig_dir, "poverty_map.png"),
       g,
       units = "in",
       height = 5,
       width = 6,
       dpi = "retina")

## -------------------------------------
## Avg Unemployment Rate (2018-2022) 
## -------------------------------------

g <- ggplot() +
  geom_polygon(data = st_ct_map, aes(x = long, y = lat, group = group),  # Add county boundaries
               fill = "white", color = "black", size = 0.2) +
  geom_polygon(data = ct_app, aes(x = long, y = lat, group = group),  # Add counties in region
               fill = "grey", color = "black", size = 0.2) +
  geom_polygon(data = st_app, aes(x = long, y = lat, group = group),  # Add state boundaries
               fill = NA, color = "black", size = 0.5) +
  geom_polygon(data = df_app_ct, aes(x = long, y = lat.x, group=group, # Add NEH funding fill
                                     fill=as.numeric(avg_unemp)), 
               color="black", size = 0.2, alpha=0.8) +
  scale_fill_distiller(type = "seq", palette = "Reds", labels = scales::label_percent(scale = 1)) + 
  labs(title="Appalachian Unemployment Rates", fill="Unemployment Rate")+
  ggthemes::theme_map()+
  theme(legend.position = "right")

ggsave(filename = file.path(fig_dir, "unemp_map.png"),
       g,
       units = "in",
       height = 5,
       width = 6,
       dpi = "retina")

## -----------------------------------------------------------------------------
## Summary Tables
## -----------------------------------------------------------------------------

## -------------------------------------
## Award Overview 
## ------------------------------------
summary_table <- df |>
  subset(appalachia == 1) |>
  summarise(total_grants = n(),
            total_award = sum(totaward, na.rm = TRUE),
            average_award = mean(totaward, na.rm = TRUE),
            max_award = max(totaward, na.rm = TRUE)) |>
  select(total_grants, total_award, average_award, max_award) |>
  mutate(across(ends_with("_award"), ~ scales::dollar(.x)))

colnames(summary_table) <- c("Total Grants", "Total Awarded", "Average", "Max")
summary_table[] <- lapply(summary_table, as.character)
print(summary_table)

## -------------------------------------
## Disciplines Overview 
## ------------------------------------

disc_table <- df |>
  subset(appalachia == 1) |>
  group_by(parent_discipline) |>
  summarise(count = n(),
            awarded = sum(totaward, na.rm = TRUE)) |>
  arrange(desc(awarded)) |>
  mutate(awarded = scales::dollar(awarded))

# Format the table
colnames(disc_table) <- c("Discipline", "Count", "Amount Awarded")
print(disc_table)

## -------------------------------------
## Divisions Overview 
## ------------------------------------

div_table <- df |>
  subset(appalachia == 1) |>
  group_by(division) |>
  summarise(count = n(),
            awarded = sum(totaward, na.rm = TRUE)) |>
  arrange(desc(awarded)) |>
  mutate(awarded = scales::dollar(awarded))

# Format the table
colnames(div_table) <- c("Division", "Count", "Amount Awarded")
print(div_table)

## -------------------------------------
## Organizations Overview 
## ------------------------------------

org_table <- df |>
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

## -------------------------------------
## IPEDS HEI Overview 
## ------------------------------------

ipeds_table <- df |>
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
  mutate(awarded = scales::dollar(awarded))

# Format the table
colnames(ipeds_table) <- c("HEI", "Count", "Amount Awarded")
print(ipeds_table)

## -------------------------------------
## HBCU HEI Overview 
## ------------------------------------

hbcu_table <- df |>
  subset(hbcu == 1 & appalachia == 1) |>
  group_by(hbcu) |>
  summarise(count = n(),
            awarded = sum(totaward, na.rm = TRUE)) |>
  arrange(desc(awarded)) |>
  mutate(awarded = scales::dollar(awarded))

## -------------------------------------
## Awardee Table Overview 
## ------------------------------------
award_table <- df |>
  subset(appalachia == 1) |>
  mutate(awarded = scales::dollar(totaward)) |>
  select(institution, yearawarded, title, awarded, instcity, stname, county)


# Format the table
colnames(award_table) <- c("Institution", "Year", "Project Title",
                           "Award Amount", "City", "State", "County")
award_table[] <- lapply(award_table, as.character)
print(award_table)
