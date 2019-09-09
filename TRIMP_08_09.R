#setwd("C:/Users/Zander/Uni/R-Projekt")
#setwd("C:/Users/Zander/Uni_Master/Semester 2/Statistisches Programmieren mit R")
setwd("C:/Users/Lisa/Desktop/06_09_R/trimp-master")

#.rds files ####
data <- readRDS("eightruns.rds")
class(data)
#head(data[[1]])

#.tcx files ####
library("zoo") #needed for trackeR
library("trackeR")
#read raw data
run_1 <- readTCX(file = "tcx/b_1.tcx", timezone = "UTC")
str(run_1)
class(run_1) # "data.frame"
#turn into trackeRdata object
#units1 <- generate_units()
#run1 <- trackeRdata(run_1, units = units1)
#class(run1) # "trackeRdata", "list"
#alternatively
#run1_a <- read_container("tcx/b_1.tcx", timezone = "UTC") 

#class: activity####
setClass("activity",
         representation(
           actnr="numeric",
           time="POSIXt",
           latitude="numeric", 
           longitude="numeric", 
           altitude="numeric",
           altitude_range="numeric",
           total_climb="numeric",
           total_descent="numeric",
           distance="numeric",
           total_distance="numeric",
           heart_rate="numeric",
           speed="numeric",
           cadence="numeric",
           total_time="numeric",
           duration="numeric"
         ),
         prototype(
           actnr = 1
         )
)


#class: athlete####
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

#class: training####
setClass("training",
         representation(
           activity="list",
           athlete="athlete"
         )
)

#cl-fct: activity####
#function initialize activity class with dist in km and speed in km/h
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
  if(is.null(df$cadence)){
    df$cadence <- df$cadence_running
  }
  cli <- 0
  des <- 0
  for(i in 2:length(df$altitude)){
    ifelse(df$altitude[i]>df$altitude[i-1], cli <- cli+abs(df$altitude[i]-df$altitude[i-1]), des <- des+abs(df$altitude[i]-df$altitude[i-1]))
  }
  ti <- as.numeric(difftime(df$time[length(df$time)], df$time[1], units="secs"))
  activity <- new("activity", 
                  actnr=nr, 
                  time=df$time,
                  latitude=df$latitude, 
                  longitude=df$longitude,
                  altitude=df$altitude,
                  altitude_range=max(df$altitude)-min(df$altitude),
                  total_climb=cli,
                  total_descent=des,
                  distance=df$distance/1000,
                  total_distance=df$distance[length(df$distance)]/1000,
                  heart_rate=df$heart_rate,
                  speed=df$speed*3.6,
                  cadence=df$cadence,
                  total_time=ti,
                  duration=dur
  )
  return(activity)
}  


#b <- activity(data[[1]])
#b@duration

#fct: list() of act.####
#function to get more then one activity data and gives a list of activity objects
actlist <- function(actls){
  ls = list()
  for(i in 1:length(actls)){
    act <- activity(actls[[i]],i)
    ls[i] <- act
  }
  return(ls)
}

#a1 <- actlist(data)
#a1
#a1[[1]]@time
#a1[[3]]@actnr


#cl-fct: athlete####
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
  ath <- new("athlete",
             name=name,
             sex=sex,
             HRest=restHR,
             HRMax=maxHR,
             Zone1=c(z1,z2-0.1),
             Zone2=c(z2,z3-0.1),
             Zone3=c(z3,z4-0.1),
             Zone4=c(z4,z5-0.1),
             Zone5=c(z5,maxHR)
  )
  return(ath)
}


#cl-fct: training####
training <- function(actls, ath){
  signature("training") 
  tr <- new("training",
            activity=actls,
            athlete=ath
  )
  return(tr)
}

#time converter####
#converts time from seconds to hours,minutes and seconds
totime <- function(sec){
  th <- sec %/% 3600
  tm <- (sec - (3600*th)) %/% 60
  ts <- sec %% 60
  ar <- array(c(th,tm,ts))
  return(ar)
}
#totime(4523)


#methods####

#add act to train####
# function adds an activity to a trainings class
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

#act1 <- activity(data[[1]])
#athl <- athlete(60,200)
#actl <- actlist(list(data[[2]],data[[3]]))
#tr <- training(actl, athl)
#tr <- addact(tr,act1)


#summary activity####
setGeneric(name="summary",
           def=function(obj)
           {
             standardGeneric("summary")
           }
)

setMethod(f="summary",
          signature="activity",
          definition=function(obj)
          {
            cat("activity ", obj@actnr, " on ", format(obj@time[1], "%d.%m.%y"), ":\n")
            ti <- totime(obj@total_time)
            cat("distance:", round(obj@total_distance, 2), "km\t\tduration: ", ti[1], ":", ti[2], ":", ti[3], " h\n\n")
            cat("altitude_range: ", obj@altitude_range, "m\ttotal_climb: ", obj@total_climb, "m\ttotal_descent: ", obj@total_descent, "m\n\n")
            table <- matrix(c(round(mean(obj@altitude), 2), round(min(obj@altitude), 2), round(max(obj@altitude), 2),
                              round(mean(obj@heart_rate), 2), round(min(obj@heart_rate), 2), round(max(obj@heart_rate), 2),
                              round(mean(obj@speed), 2), round(min(obj@speed), 2), round(max(obj@speed), 2),
                              round(mean(obj@cadence), 2), round(min(obj@cadence), 2), round(max(obj@cadence),2)
            ), ncol=3, byrow=TRUE)
            colnames(table) <- c("average", "min", "max")
            rownames(table) <- c("altitude", "heart_rate", "speed", "cadence")
            table <- as.table(table)
            table
          }
)
#summary(act)
summary(activity(data[[1]]))

#summary training####
setMethod(f="summary",
          signature="training",
          definition=function(obj)
          {
            actnrs <- length(obj@activity)
            cat("Number of activities:", actnrs, "\n\n")
            tt <- 0
            dist <- 0
            cli <- 0
            for(i in 1:actnrs){
              tt <- tt + obj@activity[[i]]@total_time
              dist <- dist + obj@activity[[i]]@total_distance
              cli <- cli + obj@activity[[i]]@total_climb
            }
            at <- tt / actnrs
            tt <- totime(tt)
            at <- totime(at)
            adist <- dist / actnrs
            cat("average time of activities:\t", at[1], ":", at[2], ":", round(at[3], 0), "h\n")
            cat("total activity time:\t\t", tt[1], ":", tt[2], ":", tt[3], "h\n\n")
            cat("average distance of activities:\t", round(adist, 2), "\tkm\n")
            cat("total distance:\t\t\t", round(dist, 2), "\tkm\n\n")
            cat("total climb:\t\t\t", cli , "m\n\n")
          }
)
#summary(tr)
#class(tr)
#class(activity(data[[1]]))

#TRIMP exp.####
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
            trimpexp <- array()
            for (count in 1:length(train@activity)){
              cat("activity: ", train@activity[[count]]@actnr, " ")
              HRr <- (train@activity[[count]]@heart_rate - train@athlete@HRest)/(train@athlete@HRMax-train@athlete@HRest)
              trimp <- sum((train@activity[[count]]@duration/60)*(HRr*0.64*exp(s*HRr)))
              trimpexp[count] <- trimp
              cat("Trimp = ", trimp, "\n")
            }
            return(trimpexp)
          }
)

#trimp_exp(tr)

#TRIMP zonal####
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
            trimpzo <- array()
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
              trimpzo[count] <- trimp
              cat("Trimp = ", trimp, "\n")
            }
            return(trimpzo)
          }
)

#trimp_zone(tr)

#Plots####
library("leaflet") #required

#plot one route####
setGeneric(name="plot_route",
           def=function(obj, num=1)
           {
             standardGeneric("plot_route")
           }
)

setMethod(f="plot_route",
          signature="training",
          definition=function(obj, num=1)
          {
            ti <- totime(obj@activity[[num]]@total_time)
            m <- leaflet() %>%
              addTiles() %>%  # Add default OpenStreetMap map tiles
              #supports: matrices, data frames, spatial objects
              addMarkers(lng=obj@activity[[num]]@longitude[1], lat=obj@activity[[num]]@latitude[1], popup=paste("start run", obj@activity[[num]]@actnr))  %>%
              addMarkers(lng=obj@activity[[num]]@longitude[length(obj@activity[[num]]@longitude)], lat=obj@activity[[num]]@latitude[length(obj@activity[[num]]@latitude)], popup=paste("end run", obj@activity[[num]]@actnr))  %>%
              addPolylines(lng=obj@activity[[num]]@longitude, lat=obj@activity[[num]]@latitude, 
                           popup = paste0("<b>","Run No: ","</b>", obj@activity[[num]]@actnr, "<br>",
                                          "<b>","Date: ","</b>",   obj@activity[[num]]@time, "<br>",
                                          "<b>","Duration: ","</b>",   ti[1], ":", ti[2], ":", ti[3], " h\n\n", "<br>",
                                          "<b>","Distance: ","</b>",   paste(round(obj@activity[[num]]@total_distance, 2), "km"), "<br>",
                                          "<b>","Avg. Speed: ","</b>",   paste(round(obj@activity[[num]]@speed, 2), "km/h"), "<br>",
                                          "<b>","Altitude: ","</b>",   paste(round(obj@activity[[1]]@altitude_range, 2), "m"), "<br>"
                           )
              )
            print(m)
          }
)

dieter<- athlete(90, 190, sex="male", name="Dieter")
dieter_train <- training(actlist(data), dieter)
plot_route(dieter_train, 2)


paul <- athlete(92,194,sex="male",name="Paul")
actl <- actlist(list(data[[3]],data[[4]]))
paul_train <- training(actl,paul)
plot_route(paul_train,1)

#plot all start points####
setGeneric(name="plot_starts",
           def=function(obj)
           {
             standardGeneric("plot_starts")
           }
)

setMethod(f="plot_starts",
          signature="training",
          definition=function(obj)
          {
            num <- 1:length(obj@activity)
            long <- lapply(num, function(x){obj@activity[[x]]@longitude[1]})
            lati <- lapply(num, function(x){obj@activity[[x]]@latitude[1]})
            ti <- lapply(num, function(x){totime(obj@activity[[x]]@total_time)})
            m <- leaflet() %>%
              addTiles() %>%  # Add default OpenStreetMap map tiles
              #supports: matrices, data frames, spatial objects
              addCircleMarkers(lng=unlist(long), lat=unlist(lati), popup = paste("run no", num), stroke=FALSE
              #popup=paste("start run", obj@activity[[x]]@actnr, "on", ti[[x]][1], ":", ti[[x]][2], ":", ti[[x]][3])
              )
            print(m)
          }
)

plot_starts(dieter_train)


#plot heart_rate, speed, atltitude over distance####
library(ggplot2) #required
library(grid) #??
setGeneric(name="plot_performance",
           def=function(obj, num=1)
           {
             standardGeneric("plot_performance")
           }
)

setMethod(f="plot_performance",
          signature="training",
          definition=function(obj, num=1)
          {
            ti <- cumsum(obj@activity[[num]]@duration)/60
            p1 <- ggplot() +
              ggtitle(paste("Run Number", obj@activity[[num]]@actnr, "on", as.Date(obj@activity[[num]]@time)[1])) +
              xlab("Time [min]") +
              ylab("Heart Rate [bpm]") +
              theme_minimal() + 
              geom_line(aes(x = ti, y = obj@activity[[num]]@heart_rate)) +
              scale_x_continuous(breaks = seq(from = 0, to = floor(max(ti)), by = 5)) +
              stat_smooth(aes(ti, obj@activity[[num]]@heart_rate ), , method="loess", formula = y~x) +
              theme(axis.title.x = element_blank(), axis.text.x = element_blank())
            
            p2 <- ggplot() +
              xlab("Time [min]") +
              ylab("Speed [kmh]") +
              theme_minimal() + 
              geom_line(aes(x = ti, y = obj@activity[[num]]@speed)) +
              scale_x_continuous(breaks = seq(from = 0, to = floor(max(ti)), by = 5)) +
              stat_smooth(aes(ti,obj@activity[[num]]@speed ), , method="loess", formula = y~x) +
              theme(axis.title.x = element_blank(), axis.text.x = element_blank())
            
            p3 <- ggplot() +
              xlab("Time [min]") +
              ylab("Altitude [m]") +
              theme_minimal() + 
              geom_line(aes(x = ti, y = obj@activity[[num]]@altitude)) +
              scale_x_continuous(breaks = seq(from = 0, to = floor(max(ti)), by = 5)) +
              stat_smooth(aes(ti,obj@activity[[num]]@altitude), method="loess", formula = y~x)
            
            
            grid.newpage()
            grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
            
          }
)

plot_performance(dieter_train)

