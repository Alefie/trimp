setwd("C:/Users/Zander/Uni/R-Projekt")

data <- readRDS("eightruns.rds")
head(data[[1]])

#create classes activity,athlete
setClass("activity", representation(time="POSIXt", latitude="numeric", longitude="numeric", altitude="numeric", distance="numeric", heart_rate="numeric", speed="numeric", cadence="numeric", duration="numeric"))
setClass("athlete", representation(name="character", sex="factor", HRest="numeric", HMax="numeric"))


a <- new("activity", time=data[[1]]$time, latitude=data[[1]]$latitude, longitude=data[[1]]$longitude, altitude=data[[1]]$altitude, distance=data[[1]]$distance, heart_rate=data[[1]]$heart_rate, speed=data[[1]]$speed, cadence=data[[1]]$cadence)
a@time
a@distance[1]

# function initialize activity class with dist in km and speed in km/h
toactivity <- function(df) {
  start <- c(as.POSIXct(df$time[1]), df$time[1:nrow(df)-1])
  dur <- as.numeric(df$time-start)
  activity <- new("activity", time=df$time,latitude=df$latitude, longitude=df$longitude, altitude=df$altitude, distance=df$distance/1000, heart_rate=df$heart_rate, speed=df$speed*3.6, cadence=df$cadence, duration=dur)
  return(activity)
}   
b <- toactivity(data[[1]])
b

#function for data of athlete
toathlete <- function(restHR, maxHR, sex="female", name="Athlete"){
  sex <- factor(sex, levels=c("male","female"))
  if (is.na(sex)){
    print("wrong value: sex has to be male or female")
    return()
  }
  athlete <- new("athlete", name=name, sex=sex, HRest=restHR, HMax= maxHR)
  return(athlete)
}
toathlete(60,120,sex="dog")
