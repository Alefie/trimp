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

# function initialize activity class with dist in km and speed in km/h
toactivity <- function(df,nr=1) {
  signature("activity")
  start <- c(as.POSIXct(df$time[1]), df$time[1:nrow(df)-1])
  dur <- as.numeric(df$time-start)
  activity <- new("activity", actnr=nr, time=df$time,latitude=df$latitude, longitude=df$longitude, altitude=df$altitude, distance=df$distance/1000, heart_rate=df$heart_rate, speed=df$speed*3.6, cadence=df$cadence, duration=dur)
  return(activity)
}   
b <- toactivity(data[[1]])
b@duration

actlist <- function(actls){
  ls = list()
  for(i in 1:length(actls)){
    act <- toactivity(actls[[i]],i)
    ls[i] <- act
  }
  return(ls)
}

a1 <- actlist(data)
a1
a1[[1]]@time
a1[[3]]@actnr

#function for data of athlete
toathlete <- function(restHR, maxHR, sex="female", name="Athlete"){
  signature("athlete")
  sex <- factor(sex, levels=c("male","female"))
  if (is.na(sex)){
    print("wrong value: sex has to be male or female")
    return()
  }
  ath <- new("athlete", name=name, sex=sex, HRest=restHR, HMax=maxHR)
  return(ath)
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

paul <- toathlete(92,194,sex="male",name="Paul")
paul_trimp <- trimp_exp(paul, b)
paul_trimp


#combine Klasse als Arguemtn, zieht der sich dann automatisch.####
#setClass("activity", representation(time="POSIXt", latitude="numeric", longitude="numeric", altitude="numeric", distance="numeric", heart_rate="numeric", speed="numeric", cadence="numeric", duration="numeric"))
#setClass("athlete", representation(name="character", sex="character", HRest="numeric", HMax="numeric"))

setClass("training",
         representation(
           activity="activity",
           athlete="athlete"
         )
)

#function for athlete with his training data
totraining <- function(act, ath){
  signature(training) 
  tr <- new("training", activity=act, athlete=ath)
  return(tr)
}

#damit habe ich die Funktion überschrieben, aber nicht überladen
trimp_exp <- function(train){
  signature("training")
  if (train@sex=="male") {
    s<-1.92
  } else {
    s<-1.67
  }
  HRr <- (train@heart_rate - train@HRest)/(train@HMax-train@HRest)
  return(sum((train@duration/60)*(HRr*0.64*exp(s*HRr))))
}

#neuer Versuch:
setGeneric(name="trimp_exp_neu",
           def=function(train, df )
           {
             standardGeneric("trimp_exp_neu")
           }
)


setMethod(f="trimp_exp_neu",
          signature="training",
          definition=function(train, NULL)
          {
            if (train@sex=="male") {
              s<-1.92
            } else {
              s<-1.67
            }
            HRr <- (train@heart_rate - train@HRest)/(train@HMax-train@HRest)
            return(sum((train@duration/60)*(HRr*0.64*exp(s*HRr))))
          }
)

setMethod(f="trimp_exp_neu",
          signature = c("athlete","activity"),
          definition=function(name, df)
          {
            if (name@sex=="male") {
              s<-1.92
            } else {
              s<-1.67
            }
            HRr <- (df@heart_rate - name@HRest)/(name@HMax-name@HRest)
            return(sum((df@duration/60)*(HRr*0.64*exp(s*HRr))))
          }
)
