#' Climate input file reading function
#' 
#' @param filename
#'  reads in a csv data file and writes it to the RStudio global environment

ReadData <- function(filename) {
  dataset <- read.table(filename,header=TRUE, quote="\"")
    assign("clim.short.2", dataset, .GlobalEnv)
}