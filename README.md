## Health of the Humanities in Appalachia

This repository contains code to replicate analyses for Health of the Humanities in Appalachia (published link forthcoming). 

### Data
- Econ Data includes all economic data used in the analysis.
- NEH Data includes all publicly available grant data.
- `df_clean.csv` includes the merged economic and NEH data for all counties served by the [Appalachian Regional Commission (ARC)](https://www.arc.gov/appalachian-counties-served-by-arc/). This is the main dataset for analysis.
- `arc_clean.csv` includes Appalachian states and counties, using designation from ARC.

## Scripts
- `data_cleaning.R` includes merging and cleaning all economic and NEH data into one dataset for main analysis.
- `data_vis.R` includes preliminary data visualization for stagnant plots and tables included in the main analysis.
- `shiny_app.R` includes the UI and server initialization for the dashboard. 

