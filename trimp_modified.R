#setwd("C:/Users/Zander/Uni_Master/Semester 2/Statistisches Programmieren mit R")
setwd("C:/Users/Lisa/Desktop/R Projekt Final")

data <- readRDS("eightruns.rds")

head(data[[1]])

time<- as.POSIXlt("1.1.2019 12:13:00", tryFormats = c("%Y-%m-%d %H:%M:%OS"))
a <- "2016-12-05-16.25.54.875000"
as.POSIXct(a, format="%Y-%m-%d-%H.%M.%S", tz = "UTC") 


#activity <- data.frame(date=c("1.1.2019","1.1.2019"), time=c("12:13","12:14"),latitude=c(133,133), longitude=c(134,135), altitude=c(133,140), distance=c(300,400), heart_rate=c(120,124), speed= c(2.5,2.65), cadence=c(90,88))
#neu:####
activity <- data.frame(time=c("1.1.2019 12:13:00","1.1.2019 12:14:00"),latitude=c(133,133), longitude=c(134,135), altitude=c(133,140), distance=c(300,400), heart_rate=c(120,124), speed= c(2.5,2.65), cadence=c(90,88))####
activity####
#sonst Unterschiedliche Längen####
length(activity)####
length(data[[1]])####

activity$time <- data[[1]]$time
activity$time
class(data[[1]]$time)
head(data[[1]]$time)

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
# Fkt berechnet nicht die Differenzen, immer von Start bis zum nächsten Punkt. 
toactivitytrimp <- function(df) {
  start <- c(as.POSIXct(df$time[1]), data[[1]]$time[1:nrow(data[[1]])-1])####
  activity <- data.frame(duration=df$time-start, heart_rate=df$heart_rate)
  return(activity)
} 
act2 <- toactivitytrimp(data[[1]])
head(act2)
tail(act2)


#function for athlete data
toathlete <- function(restHR, maxHR, sex="male"){
  athlete <- data.frame(restHR=restHR, maxHR=maxHR, sex=factor(sex, levels=c("male","female")))
  return(athlete)
}
ath <- toathlete(80, 200, "female")
ath

#function for athlete with his training data
totraining <- function(act, ath){
  training <- data.frame(activity=act, athlete=ath)
  return(training)
}

#wofür####
train <- totraining(act, ath)
head(train)

act2 <- toactivity(data[[2]])
head(act2)

train <- totraining(rbind(act,act2), ath)
head(train)

#function to count activities
#Wird nicht für jede neue Aktivität ein neuer Listeneintrag angelegt?####
#Demnach würde ja reichen: ####
length(data)####
#Die Funktion berechnet bei mir nur 2 Aktivitäten?####
countact <- function(train){
  n <- 0
  for(i in 1:dim(train)[1]){
    if(train$activity.distance[i] < 0.005){
      n <- n+1
      print("test")
    }
    train$activity.nr[i] <- n
  }
  return(train)
}
train <- countact(train)
train
train$activity.nr[1200]


#Berechne TRIMPS####
#TRIMP exp.####
trimp_exp <- function(BPM, HRrest, HRmax, sex, dur){
  if (sex=="male") {
    s<-1.92
  } else {
    s<-1.67
  }
  HRr <- (BPM-HRrest)/(HRmax-HRrest)
  return(sum(as.numeric(dur/60)*(HRr*0.64*exp(s*HRr))))
}

#HRrest 92
#HRmax 194
#sex "male"
act2 <- toactivitytrimp(data[[1]])
head(act2)

trimp_exp(act2$heart_rate, 92, 194, "male", act2$duration)
