setwd("C:/Users/Zander/Uni/R-Projekt")

data <- readRDS("eightruns.rds")
head(data[[1]])

setClass("activity", representation(time="POSIXt", latitude="numeric",longitude="numeric",altitude="numeric",distance="numeric",heart_rate="numeric",speed="numeric",cadence="numeric"))

a <- new("activity", time=data[[1]]$time, latitude=data[[1]]$latitude, longitude=data[[1]]$longitude, altitude=data[[1]]$altitude, distance=data[[1]]$distance, heart_rate=data[[1]]$heart_rate, speed=data[[1]]$speed, cadence=data[[1]]$cadence)
a@time
a@distance[1]

# function initialize activity class with dist in km and speed in km/h
toactivity <- function(df) {
  activity <- new("activity", time=df$time,latitude=df$latitude, longitude=df$longitude, altitude=df$altitude, distance=df$distance/1000, heart_rate=df$heart_rate, speed=df$speed*3.6, cadence=df$cadence)
  return(activity)
}   
b <- toactivity(data[[1]])
b


#udate
