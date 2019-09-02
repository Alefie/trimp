#setwd("C:/Users/Zander/Uni/R-Projekt")
setwd("C:/Users/Zander/Uni_Master/Semester 2/Statistisches Programmieren mit R")
setwd("C:/Users/Lisa/Desktop/01_09_R/trimp-master")

#for .rds files
data <- readRDS("eightruns.rds")
#head(data[[1]])

#for .tcx files
#install.packages("zoo") #needed for trackeR
#install.packages("trackeR")
library("zoo") 
library("trackeR")
list.files()
#read raw data
run_1 <- readTCX(file = "b_1.tcx", timezone = "UTC")
str(run_1)
class(run_1) # "data.frame"
#turn into trackeRdata object
units1 <- generate_units()
run1 <- trackeRdata(run_1, units = units1)
class(run1) # "trackeRdata", "list"

#alternatively
run1_a <- read_container("b_1.tcx", timezone = "UTC") 

#some ideas
#source: https://rdrr.io/cran/trackeR/f/inst/doc/trackeR.pdf
plot(run1)
plot_route(run1) #witzenhausen in germany, plot correct
leafletRoute(run1) #interactive map
summary(run1)

#combine two or more runs:
run_2 <- readTCX(file = "b_2.tcx", timezone = "UTC")
units2 <- generate_units()
run2 <- trackeRdata(run_2, units = units2)
r12<- c(run1, run2)
# "trackeRdata" into "data.frame", sessions are counted
r12_frame <- as.data.frame(r12)

summary(r12)
summary(r12, session = 1)

###########
#keine Ahnung wie der erkennt, dass es sich um Running handelt



##################################################################
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
         ),
         prototype(
           actnr = 1
         )
)

setClass("athlete",
         representation(
           name="character",
           sex="factor",
           HRest="numeric",
           HRMax="numeric",
           Zone1="numeric", #50-60%
           Zone2="numeric", #60-70%
           Zone3="numeric", #70-80%
           Zone4="numeric", #80-90%
           Zone5="numeric"  #90-100%
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

# function adds a activity to a trainings class
setGeneric(name="addact",
           def=function(train,act)
              {
                standardGeneric("addact")
              }
)

setMethod(f="addact",
          signature="training",
          definition=function(train,act)
          {
            act@actnr <- length(train@activity) + 1
            train@activity[[act@actnr]] <- act
            return(train)
          }
)

act1 <- activity(data[[1]])
athl <- athlete(60,180)
actl <- actlist(list(data[[2]],data[[3]]))
tr <- training(actl, athl)
trimp_exp(tr)
tr <- addact(tr,act1)

#Berechne TRIMPS####

#class method training: calculate exp_trimp of all activities
setGeneric(name="trimp_exp",
           def=function(train)
           {
             standardGeneric("trimp_exp")
           }
)


setMethod(f="trimp_exp",
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

#Zonal Trimp
#Idea: Zone # replaces the actual HRate
#good for interval training (https://de.coursera.org/lecture/science-of-training-young-athletes-part-2/trimp-zone-method-iFZhO)
setGeneric(name="trimp_zone",
           def=function(train)
           {
             standardGeneric("trimp_zone")
           }
)

setMethod(f="trimp_zone",
          signature="training",
          definition=function(train)
          {
            for (count in 1:length(train@activity)){
              cat("activity: ", train@activity[[count]]@actnr, " ")
              act_hr<- train@activity[[count]]@heart_rate
              zone <- as.numeric(cut(act_hr, 
                                     breaks = c(0,
                                                train@athlete@Zone1[1],
                                                train@athlete@Zone2[1],
                                                train@athlete@Zone3[1],
                                                train@athlete@Zone4[1],
                                                train@athlete@Zone5[1],
                                                train@athlete@HRMax),
                                     labels=c(0,1,2,3,4,5)),
                                 right=FALSE)
              trimp <- sum((train@activity[[count]]@duration/60)*zone)
              cat("Trimp = ", trimp, "\n")
            }
            return(length(train@activity))
          }
)

trimp_zone(tr)




"to do:
summary for heart_rate, cadence,.. in activity
summary for distance, duration,.. in all activities
read tcx file
Zonal Trimp
various plotting
ergibt avg hr scaling sinn?
"
