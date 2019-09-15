\docType{methods}
\name{summary}
\alias{summary}
\alias{summary,activity-method}
\alias{summary,training-method}
\title{gives a summary of running data}
\usage{
  summary(obj, nr = NULL)

\S4method{summary}{activity}(obj, nr = NULL)
\S4method{summary}{training}(obj, nr = NULL)
}
\arguments{
  \item{obj}{
    an object of class activity or training
  }
  \item{nr}{
    numbers of the activities, for which a summary has to be printed (only if the summary function get an object of class training)
  }
}
\description{
  This method gives a detailed summary of the trainings activity. It prints information about the distance, duration, heart rate, climb and pace.
}
