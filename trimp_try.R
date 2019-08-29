setwd("C:/Users/Zander/Uni/R-Projekt")
setwd("C:/Users/Lisa/Desktop/R Projekt Final/trimp-master/trimp-master")

data <- readRDS("eightruns.rds")
head(data[[1]])

setClass("activity",
         representation(
            actnr="numeric",
            time="POSIXt",
            latitude="numeric", 
            longitude="numeric", 
            altitude="numeric",
            distance="numeric",
            heart_rate="numeric",
            speed="numeric",
            cadence="numeric",
            duration="numeric"
          )
)

setClass("athlete",
         representation(
           name="character",
           sex="factor",
           HRest="numeric",
           HMax="numeric"
          )
)

setClass("training",
         representation(
           activity="activity",
           athlete="athlete"
         )
)

# function initialize activity class with dist in km and speed in km/h
activity <- function(df,nr=1) {
  signature("activity")
  start <- c(as.POSIXct(df$time[1]), df$time[1:nrow(df)-1])
  dur <- as.numeric(df$time-start)
  activity <- new("activity", actnr=nr, time=df$time,latitude=df$latitude, longitude=df$longitude, altitude=df$altitude, distance=df$distance/1000, heart_rate=df$heart_rate, speed=df$speed*3.6, cadence=df$cadence, duration=dur)
  return(activity)
}   

b <- activity(data[[1]])
b@duration

#function to get more then one activity data and gives a list of activity objects
actlist <- function(actls){
  ls = list()
  for(i in 1:length(actls)){
    act <- activity(actls[[i]],i)
    ls[i] <- act
  }
  return(ls)
}

a1 <- actlist(data)
a1
a1[[1]]@time
a1[[3]]@actnr

#function for data of athlete
athlete <- function(restHR, maxHR, sex="female", name="Athlete"){
  signature("athlete")
  sex <- factor(sex, levels=c("male","female"))
  if (is.na(sex)){
    print("wrong value: sex has to be male or female")
    return()
  }
  ath <- new("athlete", name=name, sex=sex, HRest=restHR, HMax=maxHR)
  return(ath)
}

#function for athlete with his training data
training <- function(act, ath){
  signature("training") 
  tr <- new("training", activity=act, athlete=ath)
  return(tr)
}

#Berechne TRIMPS####
#TRIMP exp.####
trimp_exp <- function(name, df){
  signature("athlete")
  signature("activity")
  if (name@sex=="male") {
    s<-1.92
  } else {
    s<-1.67
  }
  HRr <- (df@heart_rate - name@HRest)/(name@HMax-name@HRest)
  return(sum((df@duration/60)*(HRr*0.64*exp(s*HRr))))
}

paul <- athlete(92,194,sex="male",name="Paul")
act <- activity(data[[1]])
tr <- training(act,paul)
paul_trimp <- trimp_exp(paul, b)
paul_trimp

#class method training: calculate trimp of one activity
setGeneric(name="trimp_exp_neu",
           def=function(train)
           {
             standardGeneric("trimp_exp_neu")
           }
)


setMethod(f="trimp_exp_neu",
          signature="training",
          definition=function(train)
          {
            if (train@athlete@sex=="male") {
              s<-1.92
            } else {
              s<-1.67
            }
            HRr <- (train@activity@heart_rate - train@athlete@HRest)/(train@athlete@HMax-train@athlete@HRest)
            return(sum((train@activity@duration/60)*(HRr*0.64*exp(s*HRr))))
          }
)

trimp_exp_neu(tr)