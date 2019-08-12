setwd("C:/Users/Zander/Uni_Master/Semester 2/Statistisches Programmieren mit R")

data <- readRDS("eightruns.rds")
head(data[[1]])

activity <- data.frame(date=c("1.1.2019","1.1.2019"), time=c("12:13","12:14"),latitude=c(133,133), longitude=c(134,135), altitude=c(133,140), distance=c(300,400), heart_rate=c(120,124), speed= c(2.5,2.65), cadence=c(90,88))
activity
activity$time <- data[[1]]$time
activity <- data.frame(time=data[[1]]$time,latitude=data[[1]]$latitude, longitude=data[[1]]$longitude, altitude=data[[1]]$altitude, distance=data[[1]]$distance, heart_rate=data[[1]]$heart_rate, speed=data[[1]]$speed, cadence=data[[1]]$cadence)
head(activity)

# function makes activity with dist in km and speed in km/h
toactivity <- function(df) {
  activity <- data.frame(time=df$time,latitude=df$latitude, longitude=df$longitude, altitude=df$altitude, distance=df$distance/1000, heart_rate=df$heart_rate, speed=df$speed*3.6, cadence=df$cadence)
  return(activity)
}                       
act <- toactivity(data[[1]])
head(act)

# function that only saves duration and hr
toactivitytrimp <- function(df) {
  start <- df$time[1]
  activity <- data.frame(duration=df$time-start, heart_rate=df$heart_rate)
  return(activity)
} 
act2 <- toactivitytrimp(data[[1]])
head(act2)



