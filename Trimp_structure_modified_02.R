#setwd("C:/Users/Zander/Uni/R-Projekt")
setwd("C:/Users/Lisa/Desktop/R Projekt Final/trimp-master/trimp-master")

data <- readRDS("eightruns.rds")
head(data[[1]])

setClass("activity", representation(act_number="numeric", time="POSIXt", latitude="numeric", longitude="numeric", altitude="numeric", distance="numeric", heart_rate="numeric", speed="numeric", cadence="numeric", duration="numeric"))
setClass("athlete", reppresentation(name="character", sex="factor", HRest="numeric", HMax="numeric"))

library(dplyr) #for bind_rows function
tt<-bind_rows(data, .id = "column_label")

a <- new("activity", time=data[[1]]$time, latitude=data[[1]]$latitude, longitude=data[[1]]$longitude, altitude=data[[1]]$altitude, distance=data[[1]]$distance, heart_rate=data[[1]]$heart_rate, speed=data[[1]]$speed, cadence=data[[1]]$cadence)
a


# function initialize activity class with dist in km and speed in km/h
toactivity <- function(df) {
  signature("activity")
  df <- bind_rows(df, .id = "column_label")
  start <- c(as.POSIXct(df$time[1]), df$time[1:nrow(df)-1])
  dur <- as.numeric(df$time-start)
  activity <- new("activity", act_number=df$column_label, time=df$time,latitude=df$latitude, longitude=df$longitude, altitude=df$altitude, distance=df$distance/1000, heart_rate=df$heart_rate, speed=df$speed*3.6, cadence=df$cadence, duration=dur)
  return(activity)
}   

#hier ist der Fehler
ttt <- toactivity(data)

b <- toactivity(data[[1]])
b@duration

c <- toactivity(data)

dieter <- new("athlete", name="Dieter", sex="male", HRest=60, HMax=190)
dieter@name

#function for data of athlete
toathlete <- function(restHR, maxHR, sex="female", name="Athlete"){
  signature("athlete")
  sex <- factor(sex, levels=c("male","female"))
  if (is.na(sex)){
    print("wrong value: sex has to be male or female")
    return()
  }
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

dieter_trimp<-trimp_exp(dieter, b)
dieter_trimp

paul <- new("athlete", name="Paul", sex="male", HRest=92, HMax=194)
paul_trimp <- trimp_exp(paul, b)
paul_trimp


#combine Klasse als Arguemtn, zieht der sich dann automatisch.####
#setClass("activity", representation(time="POSIXt", latitude="numeric", longitude="numeric", altitude="numeric", distance="numeric", heart_rate="numeric", speed="numeric", cadence="numeric", duration="numeric"))
#setClass("athlete", representation(name="character", sex="character", HRest="numeric", HMax="numeric"))

setClass("training", contains = c("activity", "athlete"))
t <- new("training", time=data[[1]]$time, latitude=data[[1]]$latitude, longitude=data[[1]]$longitude, altitude=data[[1]]$altitude, distance=data[[1]]$distance, heart_rate=data[[1]]$heart_rate, speed=data[[1]]$speed, cadence=data[[1]]$cadence, name="Paul", sex="male", HRest=92, HMax=194) 
t

#function for athlete with his training data
totraining <- function(act, ath){
  signature("training") 
  tr <- new("training", act, ath)
  return(tr)
}

act <- toactivity(df)
ath <- toathlete(name)
training <- data.frame(athlete=ath, activity=act)

peter_train <- new("training", toactivity(data[[1]]), name="Peter", sex="male", HRest=92, HMax=194) 
peter_train

louis_ath <- new("athlete", name="Louis", sex="male", HRest=94, HMax=198)
louis_train <- new("training", toactivity(data[[1]]), louis_ath) 
louis_train


dieter_train <- new("training", b, dieter)
dieter_meth <- totraining(b, dieter)
dieter_meth


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

dieter_trimp_2 <- trimp_exp(dieter_train)
dieter_trimp_2

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

dieter_trimp_2 <- trimp_exp_neu(dieter_train)
dieter_trimp_2

dieter_trimp<-trimp_exp_neu(dieter, b)
dieter_trimp

