# Assignment #2 - R
# Claire Dooley April 30 2015

#' Spring Temperature and Percipitation Function
#' 
#' This function (based on user-selected spring months):
#' 1. calculates average spring temperature
#' 2. determines the year with the lowest spring temperature
#' 3. calculated mean spring precipitation
#' 4. determines the year with the lowest spring precipitation
#'
#' @param spring.months User-selected spring months numbers (1 = January, 2 = February, etc)
#' @references
#' clim.short data set containing temperature and precipitation for every day for 20 years 
#' \url{https://gauchospace.ucsb.edu/courses/course/view.php?id=7423}
#' @author Dooley, Claire          

# Data cleaning

# calculates values in a new column "tavg" to be the average of "tmax" and "tmin"
clim.short$tavg <- rowMeans(subset(clim.short, select = c("tmin", "tmax"))) 
             
# set the user-defined months of spring
spring.months <- c(3,4,5) 

# write the function
spring.avg.temp <- function(spring.months, clim.data=clim.short){
  spring.clim <- subset(clim.data, month %in% spring.months) # subsets the clim short to only include user-selected months
  
  annual.temps <- aggregate(tavg ~ year, spring.clim, mean) # calculate mean temperature for each year
  colnames(annual.temps)=c("year","tavg")
  avg.temp <- mean(annual.temps$tavg) # calculate mean of all years' mean temperatures
  
  annual.precip <- aggregate(rain ~ year, spring.clim, mean) # calculate mean precipitation for each year
  avg.precip <- mean(annual.precip$rain) # calculate mean of all years' mean precipitation
  
  low.temp <- annual.temps[which.min(annual.temps$tavg), ] # returns the row of the annual.temps dataframe with the  lowest temperature

  high.precip <- annual.precip[which.max(annual.precip$rain), ] # returns the row of the annual.precip dataframe with the highest rainfall
  
  return(c(AverT=avg.temp, AverP= avg.precip, lowTyr=low.temp, highPyr=high.precip)) # answers the questions of the assignment
}

# Generate assignment output
spring.avg.temp(spring.months)
