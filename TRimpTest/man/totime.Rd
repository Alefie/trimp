\name{totime}
\alias{totime}
\title{
  converts seconds to another time format
}
\description{
  Converts seconds to a time format of hours, minutes and seconds.
}
\usage{
  totime(sec)
}
\arguments{
  \item{sec}{
  seconds as numeric value
  }
}
\value{
  \item{th}{
    time in hours
  }
  \item{tm}{
    time in minutes
  }
  \item{ts}{
    time in seconds
  }
}
\examples{
  totime(4325)
}

