\name{athlete}
\alias{athlete}
\title{
  creates an object of class athlete
}
\description{
  Get the data of aa person with his resting pulse, his maximal heart rate, his name and his sex. It creates than an object of class athlete
}
\usage{
  athlete(restHR, maxHR, sex="female", name="Athlete")
}
\arguments{
  \item{restHR}{
    resting pulse of the athlete
  }
  \item{maxHR}{
    maximum heart rate of the athlete
  }
  \item{sex}{
    sex of the athlete; factor with levels "male" and "female"
  }
  \item{name}{
    name of the athlete
  }
}
\value{
  \item{ath}{
    the new object of class athlete
  }
}
\examples{
  athlete(restHR=50, maxHR=200, sex="male", name="Sven")
}
