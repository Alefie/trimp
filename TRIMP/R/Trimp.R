##########################
## classes ###############
##########################

# class activity ####
# contains all activity data
setClass("activity",
         representation(
           actnr          = "numeric",
           time           = "POSIXt",
           latitude       = "numeric",
           longitude      = "numeric",
           altitude       = "numeric",
           altitude_range = "numeric",
           total_climb    = "numeric",
           total_descent  = "numeric",
           distance       = "numeric",
           total_distance = "numeric",
           heart_rate     = "numeric",
           speed          = "numeric",
           cadence        = "numeric",
           total_time     = "numeric",
           duration       = "numeric"
         ),
         prototype(
           actnr = 1
         )
)

# class activity ####
# contains athlete data with his personal TRIMP zones
setClass("athlete",
         representation(
           name  = "character",
           sex   = "factor",
           HRest = "numeric",
           HRMax = "numeric",
           Zone1 = "numeric",
           Zone2 = "numeric",
           Zone3 = "numeric",
           Zone4 = "numeric",
           Zone5 = "numeric"
         )
)

# class training ####
# combines a list of activities and personal athlete data to one training object
setClass("training",
         representation(
           activity = "list",
           athlete  = "athlete"
         )
)
#################################
## initialization of classes ####
#################################


# activity function ####
# function initialize activity class with distance in km and speed in km/h
activity <- function(df, nr = 1) {
  signature("activity")
  start <- c(as.POSIXct(df$time[1]), df$time[1 : nrow(df) - 1])

  # calculates the duration of each sequence
  dur <- as.numeric(df$time-start)

  # if heart_rate entry is missing: last observation carry forward
  if(is.na(df$heart_rate[1])) {
    for(i in 2 : length(df$heart_rate)) {
      if(!is.na(df$heart_rate[i])) {
        df$heart_rate[1] <- df$heart_rate[i]
      }
    }
  }
  if(is.na(df$heart_rate[1])) {
    cat("error, no heart_rate data")
    return(-1)
  }
  for(j in 2 : length(df$heart_rate)) {
    if(is.na(df$heart_rate[j])) {
      df$heart_rate[j] <- df$heart_rate[j - 1]
    }
  }
  if(is.null(df$cadence)) {
    df$cadence <- df$cadence_running
  }

  # calculates the total climb and descent
  cli <- 0
  des <- 0
  for(i in 2 : length(df$altitude)) {
    ifelse(df$altitude[i] > df$altitude[i - 1],
           cli <- cli + abs(df$altitude[i] - df$altitude[i - 1]),
           des <- des + abs(df$altitude[i] - df$altitude[i - 1]))
  }

  # calculates the total time of a session
  ti <- as.numeric(difftime(df$time[length(df$time)], df$time[1], units="secs"))

  activity <- new("activity",
                  actnr          = nr,
                  time           = df$time,
                  latitude       = df$latitude,
                  longitude      = df$longitude,
                  altitude       = df$altitude,
                  altitude_range = max(df$altitude) - min(df$altitude),
                  total_climb    = cli,
                  total_descent  = des,
                  distance       = df$distance / 1000,
                  total_distance = df$distance[length(df$distance)] / 1000,
                  heart_rate     = df$heart_rate,
                  speed          = df$speed * 3.6,
                  cadence        = df$cadence,
                  total_time     = ti,
                  duration       = dur
  )
  return(activity)
}

# actlist function ####
# function to get more then one activity data and gives a list of activity objects
actlist <- function(actls) {
  ls = list()
  for(i in 1 : length(actls)) {
    act <- activity(actls[[i]] , i)
    ls[i] <- act
  }
  return(ls)
}

# athlete function ####
# function for personal data of the athlete
athlete <- function(restHR, maxHR, sex = "female", name = "Athlete"){
  signature("athlete")
  sex <- factor(sex, levels = c("male","female"))

  # error handling
  if (restHR > maxHR) {
    stop("restHR cannot be greater than maxHR")
  }
  if (restHR < 30) {
    stop("restHR unrealistic, too low")
  }
  if (maxHR > 220) {
    stop("maxHR unrealistic, too high")
  }
  if (is.na(sex)) {
    stop("wrong value: sex has to be male or female")
  }

  # calculates athletes trimp zones
  z1 <- (50 * maxHR) / 100
  z2 <- (60 * maxHR) / 100
  z3 <- (70 * maxHR) / 100
  z4 <- (80 * maxHR) / 100
  z5 <- (90 * maxHR) / 100

  ath <- new("athlete",
             name  = name,
             sex   = sex,
             HRest = restHR,
             HRMax = maxHR,
             Zone1 = c(z1 , z2 - 0.1),
             Zone2 = c(z2 , z3 - 0.1),
             Zone3 = c(z3 , z4 - 0.1),
             Zone4 = c(z4 , z5 - 0.1),
             Zone5 = c(z5 , maxHR)
  )
  return(ath)
}

# training function ####
# combines athlete object with training data
training <- function(actls, ath){
  signature("training")
  tr <- new("training",
            activity = actls,
            athlete  = ath
  )
  return(tr)
}

# addact functions ####
# function adds an activity to a trainings class
setGeneric(name = "addact",
           def  = function(train, act)
           {
             standardGeneric("addact")
           }
)

setMethod(f = "addact",
          signature  = "training",
          definition = function(train, act)
          {
            act@actnr <- length(train@activity) + 1
            train@activity[[act@actnr]] <- act
            return(train)
          }
)

# totime function ####
# converts time from seconds to hours, minutes and seconds
totime <- function(sec){
  th <- sec %/% 3600
  tm <- sprintf("%02g", (sec - (3600 * th)) %/% 60)
  ts <- sprintf("%02g", (round(sec %% 60, 0)))
  ar <- array(c(th, tm, ts))
  return(ar)
}

# summary functions ####
setGeneric(name = "summary",
           def  = function(obj)
           {
             standardGeneric("summary")
           }
)

# summarieses the results of one training session
setMethod(f = "summary",
          signature  = "activity",
          definition = function(obj)
          {
            cat("activity ", obj@actnr, " on ", format(obj@time[1], "%d.%m.%y"), ":\n")
            ti <- totime(obj@total_time)
            cat("distance:",          round(obj@total_distance, 2),
                "km\t\tduration: ",   ti[1], ":", ti[2], ":", ti[3], " h\n\n")
            cat("altitude_range: ",   round(obj@altitude_range, 2),
                "m\ttotal_climb: ",   round(obj@total_climb, 2),
                "m\ttotal_descent: ", round(obj@total_descent, 2), "m\n\n")
            table <- matrix(c(round(mean(obj@altitude), 2),   round(min(obj@altitude), 2),   round(max(obj@altitude), 2),
                              round(mean(obj@heart_rate), 2), round(min(obj@heart_rate), 2), round(max(obj@heart_rate), 2),
                              round(mean(obj@speed), 2),      round(min(obj@speed), 2),      round(max(obj@speed), 2),
                              round(mean(obj@cadence), 2),    round(min(obj@cadence), 2),    round(max(obj@cadence),2)
            ), ncol=3, byrow=TRUE)
            colnames(table) <- c("average", "min", "max")
            rownames(table) <- c("altitude", "heart_rate", "speed", "cadence")
            table <- as.table(table)
            table
          }
)

# summarieses the results of all training sessions
setMethod(f = "summary",
          signature  = "training",
          definition = function(obj)
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
            cat("average time of activities:\t", at[1], ":", at[2], ":", at[3], "h\n")
            cat("total activity time:\t\t",      tt[1], ":", tt[2], ":", tt[3], "h\n\n")
            cat("average distance of activities:\t", round(adist, 2), "\tkm\n")
            cat("total distance:\t\t\t",             round(dist, 2), "\tkm\n\n")
            cat("total climb:\t\t\t",                round(cli, 2) , "m\n\n")
          }
)

# read data ####
# read tcx data and creates a new data.frame
read_tcxToRun <- function(file) {
  doc <- xmlParse(file)                                                                   #read xml data
  data <- xmlToDataFrame(nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns"))               #filter every trackingpoint
  for (i in 1 : length(nodes)) {                                                          #add new nodes if values are missing
    if(is.null(data$HeartRateBpm[i]) || is.na(data$HeartRateBpm[i])) {
      addChildren(nodes[i][[1]], newXMLNode("HeartRateBpm", newXMLNode("Value", "Na")))
    }
    if(is.null(data$Time[i]) || is.na(data$Time[i])) {
      addChildren(nodes[i][[1]], newXMLNode("Time", "Na"))
    }
    if(is.null(data$Position[i]) || is.na(data$Position[i])) {
      addChildren(nodes[i][[1]], newXMLNode("Position", newXMLNode("LatitudeDegrees", "Na"), newXMLNode("LongitudeDegrees", "Na")))
    }
    if(is.null(data$AltitudeMeters[i]) || is.na(data$AltitudeMeters[i])) {
      addChildren(nodes[i][[1]], newXMLNode("AltitudeMeters", "Na"))
    }
    if(is.null(data$DistanceMeters[i]) || is.na(data$DistanceMeters[i])) {
      addChildren(nodes[i][[1]], newXMLNode("DistanceMeters", "Na"))
    }
    if(is.null(data$Speed[i]) || is.na(data$Speed[i])) {
      addChildren(nodes[i][[1]], newXMLNode("Extensions", newXMLNode("TPX", newXMLNode("Speed", "Na"))))
    }
    if(is.null(data$Cadence[i]) || is.na(data$Cadence[i])) {
      addChildren(nodes[i][[1]], newXMLNode("Extensions", newXMLNode("TPX", newXMLNode("Cadence", "Na"))))
    }
  }

  rows <- lapply(nodes, function(x) data.frame(xmlToList(x)))                             #make a data.frame of each tracking point
  df <- do.call("rbind", rows)

  #rename the columns
  names(df)[names(df) == "Time"] <- "time"
  names(df)[names(df) == "Position.LatitudeDegrees"] <- "latitude"
  names(df)[names(df) == "Position.LongitudeDegrees"] <- "longitude"
  names(df)[names(df) == "AltitudeMeters"] <- "altitude"
  names(df)[names(df) == "DistanceMeters"] <- "distance"
  names(df)[names(df) == "Value"] <- "heart_rate"
  names(df)[names(df) == "Extensions.TPX.Speed"] <- "speed"
  names(df)[names(df) == "Speed"] <- "speed"
  names(df)[names(df) == "Extensions.TPX.Cadence"] <- "cadence"
  names(df)[names(df) == "Cadence"] <- "cadence"

  #convert values from factor to numeric/date
  df$time       <- as.POSIXct(paste(substr(as.character(df$time),1,10),substr(as.character(df$time),12,19)), tz="UTC")
  df$latitude   <- as.numeric(as.character(df$latitude))
  df$longitude  <- as.numeric(as.character(df$longitude))
  df$altitude   <- as.numeric(as.character(df$altitude))
  df$distance   <- as.numeric(as.character(df$distance))
  df$heart_rate <- as.numeric(as.character(df$heart_rate))
  df$speed      <- as.numeric(as.character(df$speed))
  df$cadence    <- as.numeric(as.character(df$cadence))

  return(df)

}

# TRIMP ####
# trimp exp function ####
trimp_exp_fct <- function(obj) {
  signature("training")
  if (obj@athlete@sex == "male") {
    s <- 1.92
  } else {
    s <- 1.67
  }
  num <- 1 : length(obj@activity)
  HRr <- lapply(num, function(x) {
    (obj@activity[[x]]@heart_rate - obj@athlete@HRest)/(obj@athlete@HRMax - obj@athlete@HRest)
  })
  trimp_ex <- lapply(num, function(x) {
    sum((obj@activity[[x]]@duration / 60) * (HRr[[x]] * 0.64 * exp(s * HRr[[x]])))
  }) %>% unlist(.)
  return(trimp_ex)
}

# trimp print function #####
# print function for both trimps
trimp_print_fct <- function(obj, trimp) {
  num <- 1:length(obj@activity)
  lapply(num, function(x) {
    ti <- totime(obj@activity[[x]]@total_time)

    # output on console:
    cat("activity: ", obj@activity[[x]]@actnr, " ")
    cat("trimp = ",   trimp[x] %>% round(., 2), "\t" )
    cat("on ",        format(obj@activity[[x]]@time[1], "%d.%m.%y") , "\t")
    cat("distance: ", obj@activity[[x]]@total_distance %>% round(., 2) %>% sprintf("%4.1f", .), "\tkm\t")
    cat("duration: ", ti[1], ":", ti[2], ":", ti[3], "\th\n")}
  )
  cat("")
}

setGeneric(name = "trimp_exp",
           def  = function(obj)
           {
             standardGeneric("trimp_exp")
           }
)

setMethod(f = "trimp_exp",
          signature  = "training",
          definition = function(obj) {
            cat("\texponential trimp \n")
            trimp <- trimp_exp_fct(obj)
            trimp_print_fct(obj, trimp)
          }

)

# trimp zonal function ####
trimp_z_fct <- function(obj) {
  signature("training")
  num <- 1:length(obj@activity)
  act_hr <- lapply(num, function(x) {
    obj@activity[[x]]@heart_rate
  })

  # calculates the zone for each sequence
  zone <- lapply(num, function(x) {
    as.numeric(cut(act_hr[[x]],
                   breaks = c(0,
                              obj@athlete@Zone1[1],
                              obj@athlete@Zone2[1],
                              obj@athlete@Zone3[1],
                              obj@athlete@Zone4[1],
                              obj@athlete@Zone5[1],
                              obj@athlete@HRMax),
                   labels=c(0, 1, 2, 3, 4, 5)),
               right=FALSE)})

  trimp_zo <- lapply(num, function(x) {
    sum((obj@activity[[x]]@duration / 60)*zone[[x]])
  }) %>% unlist(.)
  return(trimp_zo)
}


setGeneric(name = "trimp_zone",
           def  = function(obj)
           {
             standardGeneric("trimp_zone")
           }
)

setMethod(f = "trimp_zone",
          signature = "training",
          definition=function(obj) {
            cat("\tzonal trimp \n")
            trimp <- trimp_z_fct(obj)
            trimp_print_fct(obj, trimp)
          }
)

# plot sessions ####
# plot one route####
setGeneric(name = "plot_route",
           def  = function(obj, num=1)
           {
             standardGeneric("plot_route")
           }
)

setMethod(f = "plot_route",
          signature  = "training",
          definition = function(obj, num = 1)
          {
            ti <- totime(obj@activity[[num]]@total_time)
            m  <- leaflet() %>%
              addTiles() %>%  # adds default OpenStreetMap map tiles

              # add start points
              addMarkers(lng=obj@activity[[num]]@longitude[1], lat=obj@activity[[num]]@latitude[1],
                         popup=paste("start run", obj@activity[[num]]@actnr))  %>%

              #add end points
              addMarkers(lng=obj@activity[[num]]@longitude[length(obj@activity[[num]]@longitude)], lat=obj@activity[[num]]@latitude[length(obj@activity[[num]]@latitude)],
                         popup=paste("end run", obj@activity[[num]]@actnr))  %>%

              # plot route
              addPolylines(lng=obj@activity[[num]]@longitude, lat=obj@activity[[num]]@latitude,
                           popup = paste0("<b>","Run No: "," </b>",     obj@activity[[num]]@actnr, "<br>",
                                          "<b>","Date: "," </b>",       format(obj@activity[[num]]@time[1], "%d.%m.%y %H:%M:%S"), "<br>",
                                          "<b>","Duration: "," </b>",   ti[1], ":", ti[2], ":", ti[3], " h\n\n", "<br>",
                                          "<b>","Distance: ","  </b>",  paste(round(obj@activity[[num]]@total_distance, 2), "km"), "<br>",
                                          "<b>","Avg. Speed: "," </b>", paste(round(obj@activity[[num]]@speed, 2), "km/h"), "<br>",
                                          "<b>","Altitude: "," </b>",   paste(round(obj@activity[[1]]@altitude_range, 2), "m"), "<br>"
                           )
              )
            print(m)
          }
)

# plot all start points ####
setGeneric(name = "plot_starts",
           def  = function(obj)
           {
             standardGeneric("plot_starts")
           }
)

setMethod(f = "plot_starts",
          signature  = "training",
          definition = function(obj)
          {
            num  <- 1:length(obj@activity)
            long <- lapply(num, function(x){obj@activity[[x]]@longitude[1]})
            lati <- lapply(num, function(x){obj@activity[[x]]@latitude[1]})
            ti   <- lapply(num, function(x){totime(obj@activity[[x]]@total_time)})
            m <- leaflet() %>%
              addTiles() %>%  # Add default OpenStreetMap map tiles
              addCircleMarkers(lng=unlist(long), lat=unlist(lati), popup = paste("run no", num), stroke=FALSE
              )
            print(m)
          }
)

# plot performance ####
# plot heart_rate, speed, atltitude over distance
setGeneric(name = "plot_performance",
           def  = function(obj, num = 1)
           {
             standardGeneric("plot_performance")
           }
)

setMethod(f = "plot_performance",
          signature  = "training",
          definition = function(obj, num = 1)
          {

            # 1st: heart rate over distance
            p1 <- ggplot() +
              ggtitle(paste("Run Number", obj@activity[[num]]@actnr, "on", format(obj@activity[[num]]@time[1], "%d.%m.%y"))) +
              ylab("Heart Rate [bpm]") +
              theme_minimal() +
              geom_line(aes(x = obj@activity[[num]]@distance, y = obj@activity[[num]]@heart_rate)) +
              scale_x_continuous(breaks = seq(from = 0, to = floor(max(obj@activity[[num]]@distance)), by = 5)) +
              stat_smooth(aes(obj@activity[[num]]@distance, obj@activity[[num]]@heart_rate ), method="loess", formula = y~x) +
              theme(axis.title.x = element_blank(), axis.text.x = element_blank())

            # 2nd: speed over distance
            p2 <- ggplot() +
              ylab("Speed [kmh]") +
              theme_minimal() +
              geom_line(aes(x = obj@activity[[num]]@distance, y = obj@activity[[num]]@speed)) +
              scale_x_continuous(breaks = seq(from = 0, to = floor(max(obj@activity[[num]]@distance)), by = 5)) +
              stat_smooth(aes(obj@activity[[num]]@distance,obj@activity[[num]]@speed ),method="loess", formula = y~x) +
              theme(axis.title.x = element_blank(), axis.text.x = element_blank())

            # 3rd: altitude over distance
            p3 <- ggplot() +
              xlab("Distance [km]") +
              ylab("Altitude [m]") +
              theme_minimal() +
              geom_line(aes(x = obj@activity[[num]]@distance, y = obj@activity[[num]]@altitude)) +
              scale_x_continuous(breaks = seq(from = 0, to = floor(max(obj@activity[[num]]@distance)), by = 5))

            grid.newpage()
            grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))

          }
)
