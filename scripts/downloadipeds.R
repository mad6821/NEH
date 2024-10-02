################################################################################
##
## <PROJ> Batch download IPEDS files
## <FILE> downloadipeds.R
## <AUTH> Benjamin Skinner (@btskinner)
## <INIT> 21 July 2015
## <REVN> 30 June 2018
##
################################################################################

## PURPOSE ---------------------------------------------------------------------
##
## Use this script to batch download IPEDS files. Only those files listed
## in `ipeds_file_list.txt` will be downloaded. The default behavior is to
## download each of the following files into their own subdirectories:
##
## (1) Data file
## (2) Dictionary file
##
## You can also choose to download other data versions and/or program files:
##
## (1) Data file (STATA version)
## (2) STATA program file (default if you ask for DTA version data)
## (3) SPSS program file
## (4) SAS program file
##
## The default behavior is download ALL OF IPEDS. If you don't want everything,
## modify `ipeds_file_list.txt` to only include those files that you want.
## Simply erase those you don't want, keeping one file name per row, or
## comment them out using a hash symbol (#).
##
## You also have the option of whether you wish to overwrite existing files.
## If you do, change the -overwrite- option to TRUE. The default behavior is
## to only download files listed in `ipeds_file_list.txt` that have not already
## been downloaded.
## -----------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## CHOOSE WHAT YOU WANT (TRUE == Yes, FALSE == No)
## -----------------------------------------------------------------------------

## default
primary_data = TRUE
dictionary = TRUE

## STATA version
## (NB: downloading Stata version of data will also get Stata program files)
stata_data = FALSE

## other program files
prog_spss = FALSE
prog_sas  = FALSE

## overwrite already downloaded files
overwrite = FALSE

## -----------------------------------------------------------------------------
## CHOOSE OUTPUT DIRECTORY (DEFAULT == '.', which is current directory)
## -----------------------------------------------------------------------------

out_dir = dat_dir

## =============================================================================
## FUNCTIONS
## =============================================================================

## message
mess <- function(to_screen) {
    message(rep('-',80))
    message(to_screen)
    message(rep('-',80))
}

## create subdirectories
make_dir <- function(opt, dir_name) {
    if (opt & dir.exists(dir_name)) {
        message(paste0('Already have directory: ', dir_name))
    } else if (opt & !dir.exists(dir_name)) {
        message(paste0('Creating directory: ', dir_name))
        dir.create(dir_name)
    }
}

## download file
get_file <- function(opt, dir_name, url, file, suffix, overwrite) {
    if (opt) {
        dest <- file.path(dir_name, paste0(file, suffix))
        if (file.exists(dest) & !overwrite) {
            message(paste0('Already have file: ', dest))
            return(0)
        } else {
            download.file(paste0(url, file, suffix), dest)
            Sys.sleep(1)
            return(1)
        }
    }
}

## countdown
countdown <- function(pause, text) {
    cat('\n')
    for (i in pause:0) {
        cat('\r', text, i)
        Sys.sleep(1)
        if (i == 0) { cat('\n\n') }
    }
}

## =============================================================================
## RUN
## =============================================================================

## read in files; remove blank lines & lines starting with #
ipeds <- readLines('./ipeds_file_list.txt')
ipeds <- ipeds[ipeds != '' & !grepl('^#', ipeds)]

## data url
url <- 'https://nces.ed.gov/ipeds/datacenter/data/'

## init potential file paths
data_dir <- file.path(out_dir, 'ipeds')

## create folders if they don't exist
mess('Creating directories for downloaded files')
make_dir(TRUE, out_dir)
make_dir(primary_data, data_dir)

## get timer (pause == max(# of options, 3))
opts <- c(primary_data)

## loop through files
for(i in 1:length(ipeds)) {

    ow <- overwrite
    f <- ipeds[i]
    mess(paste0('Now downloading: ', f))

    ## data
    d1 <- get_file(primary_data, data_dir, url, f, '.zip', ow)

    ## get number of download requests
    dls <- sum(d1)

    if (dls > 0) {
        ## set pause based on number of download requests
        pause <- max(dls, 3)
        ## pause and countdown
        countdown(pause, 'Give IPEDS a little break ...')
    } else {
        message('No downloads necessary; moving to next file')
    }
}

mess('Finished!')

## =============================================================================
## END
################################################################################
