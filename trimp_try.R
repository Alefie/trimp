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
           HRMax="numeric",
           Zone1="numeric",
           Zone2="numeric",
           Zone3="numeric",
           Zone4="numeric",
           Zone5="numeric"
          )
)

setClass("training",
         representation(
           activity="list",
           athlete="athlete"
         )
)

# function initialize activity class with dist in km and speed in km/h
activity <- function(df,nr=1) {
  signature("activity")
  start <- c(as.POSIXct(df$time[1]), df$time[1:nrow(df)-1])
  dur <- as.numeric(df$time-start)
  if(is.na(df$heart_rate[1])){
    for(i in 2:length(df$heart_rate)){
      if(!is.na(df$heart_rate[i])){
        df$heart_rate[1] <- df$heart_rate[i]
      }
    }
  }
  if(is.na(df$heart_rate[1])){
    cat("error, no heart_rate data")
    return(-1)
  }
  for(j in 2:length(df$heart_rate)){
    if(is.na(df$heart_rate[j])){
      df$heart_rate[j] <- df$heart_rate[j-1]
    }
  }
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
  z1 <- (50*maxHR)/100
  z2 <- (60*maxHR)/100
  z3 <- (70*maxHR)/100
  z4 <- (80*maxHR)/100
  z5 <- (90*maxHR)/100
  ath <- new("athlete", name=name, sex=sex, HRest=restHR, HRMax=maxHR, Zone1=c(z1,z2-0.1), Zone2=c(z2,z3-0.1), Zone3=c(z3,z4-0.1), Zone4=c(z4,z5-0.1), Zone5=c(z5,maxHR))
  return(ath)
}

#function for athlete with his training data
training <- function(actls, ath){
  signature("training") 
  tr <- new("training", activity=actls, athlete=ath)
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
  HRr <- (df@heart_rate - name@HRest)/(name@HRMax-name@HRest)
  return(sum((df@duration/60)*(HRr*0.64*exp(s*HRr))))
}

paul <- athlete(92,194,sex="male",name="Paul")
act <- actlist(data)
act[[1]]@actnr
tr <- training(act,paul)
tr@activity[[1]]@actnr
length(tr@activity)

paul_trimp <- trimp_exp(paul, b)
paul_trimp

#class method training: calculate trimp of all activities
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
            for (count in 1:length(train@activity)){
              cat("activity: ", train@activity[[count]]@actnr, " ")
              HRr <- (train@activity[[count]]@heart_rate - train@athlete@HRest)/(train@athlete@HRMax-train@athlete@HRest)
              trimp <- sum((train@activity[[count]]@duration/60)*(HRr*0.64*exp(s*HRr)))
              cat("Trimp = ", trimp, "\n")
            }
            return(length(train@activity))
          }
)

trimp_exp_neu(tr)

"to do:
  summary for heart_rate, cadence,.. in activity
  summary for distance, duration,.. in all activities
  read xt? file
  Zonal Trimp
  add one or more activities to list
"


