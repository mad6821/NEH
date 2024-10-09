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

df <- read_csv(file.path(dat_dir, "analysis.csv"))

## -----------------------------------------------------------------------------
## Initial Trend Plots
## -----------------------------------------------------------------------------

## Avg Poverty Rate
png(file.path(fig_dir, "pov_rate.png"), res=100)
df %>% 
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
  group_by(yearawarded) %>%
  mutate(avg_unemp = mean(unemp_rate, na.rm=T)) %>%
  ggplot(aes(x=yearawarded, y=avg_unemp)) + 
    stat_smooth(color="black", linewidth=0.5, se=F, method = "loess") +
    scale_y_continuous(labels = scales::comma) + 
    labs(x="Year", y="Avg. Unemployment Rate (%)", 
         title="Figure 2b. Appalachian Unemployment Rate Over Time") +
    theme_minimal()
dev.off()

## Total NEH Awarded 
png(file.path(fig_dir, "neh_awards.png"), res=90)
df %>% 
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
  ggplot(aes(x=unemp_rate, y=awardoutright)) + 
    geom_bar(fill="slateblue", stat="identity", width=0.4, alpha=0.4) +
    scale_y_continuous(labels = scales::comma) + 
    labs(y="NEH Funding", x="Unemployment Rate (%)", 
         title="Figure 3b. NEH Funding and Unemployment Rates\nin Appalachia") +
    theme_minimal()
dev.off()

## Unemployment & Poverty - APPENDIX
png(file.path(fig_dir, "unemp_pov.png"), res=90)
df %>% 
  ggplot(aes(x=unemp_rate, y=poverty_rate)) + 
    geom_point(fill="black") +
    scale_y_continuous(labels = scales::comma) + 
    labs(y="Poverty Rate (%)", x="Unemployment Rate (%)", 
         title="Figure 4. Poverty and Unemployment Rates in Appalachia") +
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
  group_by(disciplines) %>%
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Count))

disc_table$Awarded <- paste('$',formatC(disc_table$Awarded, big.mark=',', format = 'f', digits=2))

print(disc_table)

## Division Overview
div_table <- df %>%
  group_by(division) %>%
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Count)) %>%
  head(5)

div_table$Awarded <- paste('$',formatC(div_table$Awarded, big.mark=',', format = 'f', digits=2))

print(div_table)

## Organizations Overview
org_table <- df %>%
  group_by(organizationtype) %>%
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Awarded))

org_table$Awarded <- paste('$',formatC(org_table$Awarded, big.mark=',', format = 'f', digits=2))

print(org_table)

## IPEDS Overview
ipeds_table <- df %>%
  group_by(ccbasic2) %>% # need to add in HBCU and Tribals
  summarise(Count = n(), Awarded = sum(awardoutright)) %>%
  arrange(desc(Awarded))

ipeds_table$Awarded <- paste('$',formatC(ipeds_table$Awarded, big.mark=',', format = 'f', digits=2))

print(ipeds_table)

