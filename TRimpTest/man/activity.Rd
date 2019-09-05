\name{activity}
\alias{activity}
\title{
  creates an object of class activity
}
\description{
  Get a data frame with time, latitude, longitude, altitude, distance, heart_rate, speed and cadence/cadence running which was   built from a rds or from a tcx file. It creates then a new activity object.
}
\usage{
  activity(df, nr=1)
}
\arguments{
  \item{df}{
    dataframe created from rds or tcx file
  }
  \item{nr}{
    if known, the number of the activity
  }
}
\value{
  \item{activity}{
    the new object of class activity
  }
}
