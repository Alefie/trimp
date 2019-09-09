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
activity <- function(df, nr=1) {
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


#function to get more then one activity data and gives a list of activity objects
actlist <- function(actls){
  ls = list()
  for(i in 1:length(actls)){
    act <- activity(actls[[i]],i)
    ls[i] <- act
  }
  return(ls)
}

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

#function for athlete with his training data
training <- function(actls, ath){
  signature("training")
  tr <- new("training",
            activity=actls,
            athlete=ath
  )
  return(tr)
}

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

#read tcx data and creates a new data.frame
read_tcxToRun <- function(file){
  doc <- xmlParse(file)                                                                   #read xml data
  data <- xmlToDataFrame(nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns"))               #filter every trackingpoint
  for(i in 1:length(nodes)){
    if(is.na(data$HeartRateBpm[i])){
      addChildren(nodes[i][[1]], newXMLNode("HeartRateBpm", newXMLNode("Value", "Na")))   #if no entry for heart rate the value is na
    }
  }
  rows <- lapply(nodes, function(x) data.frame(xmlToList(x)))                             #make a data.frame to get every value in extension
  df <- do.call("rbind", rows)

  #rename the columns
  names(df)[names(df)=="Time"] <- "time"
  names(df)[names(df)=="Position.LatitudeDegrees"] <- "latitude"
  names(df)[names(df)=="Position.LongitudeDegrees"] <- "longitude"
  names(df)[names(df)=="AltitudeMeters"] <- "altitude"
  names(df)[names(df)=="DistanceMeters"] <- "distance"
  names(df)[names(df)=="Value"] <- "heart_rate"
  names(df)[names(df)=="Extensions.TPX.Speed"] <- "speed"
  names(df)[names(df)=="Extensions.TPX.RunCadence"] <- "cadence"

  #convert values from factor to numeric/date
  df$time <- as.POSIXct(paste(substr(as.character(df$time),1,10),substr(as.character(df$time),12,19)), tz="UTC")
  df$latitude <- as.numeric(as.character(df$latitude))
  df$longitude <- as.numeric(as.character(df$longitude))
  df$altitude <- as.numeric(as.character(df$altitude))
  df$distance <- as.numeric(as.character(df$distance))
  df$heart_rate <- as.numeric(as.character(df$heart_rate))
  df$speed <- as.numeric(as.character(df$speed))
  df$cadence <- as.numeric(as.character(df$cadence))

  return(df)
}

#converts time from seconds to hours,minutes and seconds
totime <- function(sec){
  th <- sec %/% 3600
  tm <- (sec - (3600*th)) %/% 60
  ts <- sec %% 60
  ar <- array(c(th,tm,ts))
  return(ar)
}
