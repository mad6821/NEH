## -----------------------------------------------------------------------------
##
## [ PROJ ] Appalachian funding
## [ FILE ] utils.R
## [ AUTH ] Benjamin Skinner; bskinner@neh.gov
## [ INIT ] 18 November 2024
##
## -----------------------------------------------------------------------------

## to read shapefiles from zip file
st_read_zip <- function(zfile) {
  tmp <- tempfile()
  unzip(zfile, exdir = tmp)
  st_read(dsn = tmp)
}

## -----------------------------------------------------------------------------
## end script
################################################################################
