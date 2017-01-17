##### MAP OF A COUNTRY/STATE #####################
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")
library(ggplot2)
library(ggmap)
setwd("C:/Users/jacke/Documents/R/Guest Strategies")
florida.map = get_map(location = "Florida", zoom = 7, color="bw")  ## get MAP data

p <- ggmap(florida.map)
p <- p + geom_point(data=dfff, aes(y=lat, x=lon, color=total))
p <- p +scale_color_gradient(low = "yellow", high = "red", guide=guide_legend(title = "Revenue"))
p  + theme(axis.title=element_blank(),
           axis.text=element_blank(),
           axis.ticks=element_blank()) + ggtitle("All Highways")

##### CHASING WEATHER DATA #######################
library(countyweather)
options("noaakey" = Sys.getenv("noaakey"))
#Sys.getenv("noaakey")
##### Miami-Dade County code=12086
precip <- daily_fips(fips="12086", date_min="2016-10-01", date_max="2016-10-31", var="prcp")
tmpmax <- daily_fips(fips="12086", date_min="2016-10-01", date_max="2016-10-31", var="tmax")
tmpmin <- daily_fips(fips="12086", date_min="2016-10-01", date_max="2016-10-31", var="tmin")

ggplot(precip$daily_data, aes(x = date, y = prcp, color = prcp_reporting)) + 
  geom_line() + geom_point() + theme_minimal() + 
  xlab("Date in 2016") + ylab("Daily rainfall (mm)") + 
  scale_color_continuous(name = "# stations\nreporting")

ggplot(tmpmax$daily_data, aes(x = date, y = tmax, color = tmax_reporting)) + 
  geom_line() + geom_point() + theme_minimal() + 
  xlab("Date in 2016") + ylab("Daily Max Temperature (C)") + 
  scale_color_continuous(name = "# stations\nreporting")

ggplot(tmpmin$daily_data, aes(x = date, y = tmin, color = tmin_reporting)) + 
  geom_line() + geom_point() + theme_minimal() + 
  xlab("Date in 2016") + ylab("Daily Min Temperature (C)") + 
  scale_color_continuous(name = "# stations\nreporting")

precip$station_map
