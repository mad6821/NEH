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

df <- read_csv(file.path(dat_dir, "analysis.csv")) %>%
  distinct()

## -----------------------------------------------------------------------------
## Initial Trend Plots
## -----------------------------------------------------------------------------

## Avg Poverty Rate
png(file.path(fig_dir, "pov_rate.png"), res=100)
df %>% 
  subset(appalachia == 1) %>%
  group_by(yearawarded) %>%
  mutate(avg_pov = mean(poverty_rate, na.rm=T)) %>%
  ggplot(aes(x=yearawarded, y=avg_pov)) + 
    stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="Year", y="Avg. Poverty Rate (%)", 
         title="Figure 1a. Appalachian Poverty Rate Over Time") +
    theme_minimal()
dev.off()

## Avg Unemployment Rate
png(file.path(fig_dir, "unemp_rate.png"), res=90)
df %>% 
  subset(appalachia == 1) %>%
  group_by(yearawarded) %>%
  mutate(avg_unemp = mean(unemp_rate, na.rm=T)) %>%
  ggplot(aes(x=yearawarded, y=avg_unemp)) + 
    stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="Year", y="Avg. Unemployment Rate (%)", 
         title="Figure 1b. Appalachian Unemployment Rate Over Time") +
    theme_minimal()
dev.off()

## Total NEH Awarded 
png(file.path(fig_dir, "neh_awards.png"), res=90)
df %>% 
  subset(appalachia == 1) %>%
  group_by(yearawarded) %>%
  mutate(total_award = sum(awardoutright)) %>%
  ggplot(aes(x=yearawarded, y=total_award)) + 
    stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="Year", y="Total NEH Awarded (USD)", 
         title="Figure 2. NEH Awards to Appalachia Over Time") +
    theme_minimal()
dev.off()

## Poverty Rate & NEH Funding
png(file.path(fig_dir, "poverty_awards.png"), res=90)
df %>% 
  subset(appalachia == 1) %>%
  ggplot(aes(x=poverty_rate, y=awardoutright)) + 
    geom_bar(fill="slateblue", stat="identity", width=0.6, alpha=0.4) +
    scale_y_continuous(labels = scales::comma) + 
    labs(y="NEH Funding", x="Poverty Rate (%)", 
         title="Figure 3a. NEH Funding and Poverty Rates\nin Appalachia") +
    theme_minimal()
dev.off()

## Unemployment Rate & NEH Funding
png(file.path(fig_dir, "unemp_awards.png"), res=90)
df %>% 
  subset(appalachia == 1) %>%
  ggplot(aes(x=unemp_rate, y=awardoutright)) + 
    geom_bar(fill="slateblue", stat="identity", width=0.4, alpha=0.4) +
    scale_y_continuous(labels = scales::comma) + 
    labs(y="NEH Funding", x="Unemployment Rate (%)", 
         title="Figure 3b. NEH Funding and Unemployment Rates\nin Appalachia") +
    theme_minimal()
dev.off()

## -----------------------------------------------------------------------------
## Regional Maps
## -----------------------------------------------------------------------------

## NEH Funding in Appalachia (2018-2023) - APPENDIX



## Appalachia Average Poverty Rate (2018-2022) - APPENDIX



## Appalachia Average Unemployment Rate (2018-2023) - APPENDIX


## -----------------------------------------------------------------------------
## Summary Tables
## -----------------------------------------------------------------------------

## Award Overview
summary_table <- df %>%
  subset(appalachia == 1) %>%
  summarise(
    Total_Grants = n(),
    Total_Amount = sum(awardoutright, na.rm = TRUE),
    Average_Amount = mean(awardoutright, na.rm = TRUE),
    Max_Amount = max(awardoutright, na.rm = TRUE)
  ) %>%
  select(Total_Grants, Total_Amount, Average_Amount, Max_Amount)

summary_table$Total_Amount <- paste('$',formatC(summary_table$Total_Amount, big.mark=',', format = 'f', digits=2))
summary_table$Average_Amount <- paste('$',formatC(summary_table$Average_Amount, big.mark=',', format = 'f', digits=2))
summary_table$Max_Amount <- paste('$',formatC(summary_table$Max_Amount, big.mark=',', format = 'f', digits=2))

colnames(summary_table) <- c("Total Grants", "Total Awarded", "Average", "Max")
summary_table[] <- lapply(summary_table, as.character)
print(summary_table)

## Disciplines Overview
disc_table <- df %>%
  subset(appalachia == 1) %>%
  group_by(newdiscipline) %>%
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Count))

disc_table$Awarded <- paste('$',formatC(disc_table$Awarded, big.mark=',', format = 'f', digits=2))

print(disc_table)

## Division Overview
div_table <- df %>%
  subset(appalachia == 1) %>%
  group_by(division) %>%
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Count)) %>%
  head(5)

div_table$Awarded <- paste('$',formatC(div_table$Awarded, big.mark=',', format = 'f', digits=2))

print(div_table)

## Organizations Overview
org_table <- df %>%
  subset(appalachia == 1) %>%
  group_by(organizationtype) %>%
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Awarded))

org_table$Awarded <- paste('$',formatC(org_table$Awarded, big.mark=',', format = 'f', digits=2))

print(org_table)

## IPEDS Overview
ipeds_table <- df %>%
  subset(appalachia == 1) %>%
  drop_na(ccbasic2) %>%
  mutate(ccnames = case_when(
    ccbasic2 == 1 ~ "Associate’s - Public", 
    ccbasic2 == 2 ~ "Associate’s - Private",
    ccbasic2 == 3 ~ "Research University (Very High)",
    ccbasic2 == 4 ~ "Research University (High)",
    ccbasic2 == 5 ~ "Doctoral/Research University",
    ccbasic2 == 6 ~ "Master’s (Large)",
    ccbasic2 == 7 ~ "Master’s (Medium)",
    ccbasic2 == 8 ~ "Master’s (Small)",
    ccbasic2 == 9 ~ "Baccalaureate Colleges",
    ccbasic2 == 10 ~ "Faith-Related Institutions",
    ccbasic2 == 11 ~ "Medical Schools",
    ccbasic2 == 12 ~ "Other health profession schools",
    ccbasic2 == 13 ~ "Engineering Schools",
    ccbasic2 == 14 ~ "Other tech-related schools",
    ccbasic2 == 15 ~ "Business/Management Schools",
    ccbasic2 == 16 ~ "Art, Music, and Design Schools",
    ccbasic2 == 17 ~ "Law Schools",
    ccbasic2 == 18 ~ "Other Special-Focus Institutions",
    ccbasic2 == 19 ~ "Tribal Colleges"
  )) %>%
  group_by(ccnames) %>% # need to add in HBCU and Tribals
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Awarded))

ipeds_table$Awarded <- paste('$',formatC(ipeds_table$Awarded, big.mark=',', format = 'f', digits=2))

print(ipeds_table)

## HBCU Overview
hbcu_table <- df %>%
  subset(hbcu == 1 & appalachia == 1) %>%
  group_by(hbcu) %>% # need to add in HBCU and Tribals
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Awarded))

hbcu_table$Awarded <- paste('$',formatC(hbcu_table$Awarded, big.mark=',', format = 'f', digits=2))

print(hbcu_table[,2:3])