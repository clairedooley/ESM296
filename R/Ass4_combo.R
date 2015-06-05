#' Combines climate input data to report temp data from coldest spring and precip. data from wettest spring
#' 
#' @param timestep = d, m, or y (for daily, monthly, or yearly output)
#' @param spring.months User-selected spring months numbers (1 = January, 2 = February, etc). Default is 3,4,5
#' @param dataset = climate dataset that must have the have tmax, tmin, rain (precip in mm), year, month 
#' #' (integer), day. The default is clim.short, the dataset created by the ReadData function
#'  
#' @return daily temperature from the coldest spring combined with daily precipitation from the wettest spring
#' allow the user to specify whether output pseudo climate data is daily, monthly or yearly

Combo <- function(dataset=clim.short, timestep, spring.months=c(3,4,5)) {
  
  spring.clim <- subset(dataset, month %in% spring.months) # subsets the clim short to only include user-selected months
  
  spring.clim$tavg <- rowMeans(subset(spring.clim, select = c("tmin", "tmax"))) # calculates values in a new column "tavg" to be the average of "tmax" and "tmin"
  
  annual.temps <- aggregate(tavg ~ year, spring.clim, mean) # calculate mean spring temperature for each year
  annual.precip <- aggregate(rain ~ year, spring.clim, mean) # calculate mean spring precipitation for each year
  low.temp <- annual.temps[which.min(annual.temps$tavg), ] # returns the row of the annual.temps dataframe with the  lowest temperature
  
  high.precip <- annual.precip[which.max(annual.precip$rain), ] # returns the row of the annual.precip dataframe with the highest rainfall

if (timestep=="d") {
  tmp = aggregate(dataset[,c("rain","month")], by=list(dataset$day),sum)
  newdata=tmp$rain
}

if (timestep=="m") {
  
}

if (timestep=="y") {
  
}
return(newdata)
}

#    newdata = aggregate(dataset[,c("tmax","tmin","month")], by=list(dataset$month), mean)
#    tmp = aggregate(dataset[,c("rain","month")], by=list(dataset$month),sum)
#    newdata=tmp$rain
#  }
#  return(newdata)
#}