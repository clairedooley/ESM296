#' Combines climate input data to report temp data from coldest spring and precip. data from wettest spring
#' 
#' @param timestep = d, m, or y (for daily, monthly, or yearly output)
#' @param spring.months User-selected spring months numbers (1 = January, 2 = February, etc). The default 
#' is 3,4,5
#' @param dataset = climate dataset that must have the have tmax, tmin, rain (precip in mm), year, month 
#' (integer), day. The default is clim.short, the dataset created by the ReadData function
#'  
#' @return daily temperature from the coldest spring combined with daily precipitation from the wettest spring
#' allow the user to specify whether output pseudo climate data is daily, monthly or yearly

Combo <- function(dataset=clim.short, timestep, spring.months=c(3,4,5)) {
  
  spring.clim <- subset(dataset, month %in% spring.months) # subsets the clim short to only include user-selected months
  
  spring.clim$tavg <- rowMeans(subset(spring.clim, select = c("tmin", "tmax"))) # calculates values in a new column "tavg" to be the average of "tmax" and "tmin"
  
  annual.temps <- aggregate(tavg ~ year, spring.clim, mean) # calculate mean spring temperature for each year
  
  annual.precip <- aggregate(rain ~ year, spring.clim, mean) # calculate mean spring precipitation for each year
  
  low.temp <- annual.temps[which.min(annual.temps$tavg), ] # returns the year of the annual.temps dataframe with the lowest temperature
  
  high.precip <- annual.precip[which.max(annual.precip$rain), ] # returns the year of the annual.precip dataframe with the highest rainfall
  
#wet.spring = subset(spring.clim, spring.clim$year == high.precip$year, select = c(day, month, year, rain)) #creates a subset of spring.clim of day, month, year, rain data for the year with highest spring precipitation
  
#cold.spring = subset(spring.clim, spring.clim$year == low.temp$year, select = c(day, month, year, tavg)) #creates a subset of spring.clim of day, month, year, rain data for the year with lowest spring temps

if (timestep=="d") {
  precip.output = subset(spring.clim, spring.clim$year == high.precip$year, select = c(day, month, year, rain)) #creates a subset of spring.clim of day, month, year, rain data for the year with highest spring precipitation
  colnames(precip.output) = c('day','month','year','rain')
  temp.output = subset(spring.clim, spring.clim$year == low.temp$year, select = c(day, month, year, tavg)) #creates a subset of spring.clim of day, month, year, rain data for the year with lowest spring temps
  colnames(temp.output) = c('day','month','year','tavg')
}

if (timestep=="m") {
  precip.data = subset(spring.clim, spring.clim$year == high.precip$year, select = c(month, year, rain)) #creates a subset of spring.clim of month, year, rain data for the year with highest spring precipitation
  precip.output = aggregate(precip.data$rain, by = list(precip.data$month, precip.data$year), mean)
  colnames(precip.output) = c('month','year','rain')
  
  temp.data = subset(spring.clim, spring.clim$year == low.temp$year, select = c(month, year, tavg)) #creates a subset of spring.clim of month, year, temp data for the year with lowest spring temp
  temp.output = aggregate(precip.data$rain, by = list(temp.data$month, temp.data$year), mean)
  colnames(temp.output) = c('month','year','tavg')
}

if (timestep=="y") {
  precip.data = subset(spring.clim, spring.clim$year == high.precip$year, select = c(year, rain)) #creates a subset of spring.clim of year, rain data for the year with highest spring precipitation
  precip.output = aggregate(precip.data$rain, by = list(precip.data$year), sum)
  colnames(precip.output) = c('year','rain')
  
  temp.data = subset(spring.clim, spring.clim$year == low.temp$year, select = c(year, tavg)) #creates a subset of spring.clim of month, year, temp data for the year with lowest spring temp
  temp.output = aggregate(precip.data$rain, by = list(temp.data$year), sum)
  colnames(temp.output) = c('year','tavg')
}

return(c(Wettest.spring.rain = precip.output, Coldest.spring.temp = temp.output))
}