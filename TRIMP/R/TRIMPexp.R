#' Trimpexp
#'
#' gives the value of the Trainingsimpulse of the exponantional TRIMP
#' needs time, Hearte Rate maximum, resting Heart Rate, Heart Rate average in training and sex(0=male, 1=female)
#'


TRIMPexp <- function(time, Hmax, Hrest, Havrg, sex) {

  HRR <- (Havrg-Hrest)/(Hmax-Hrest)
  if(sex!=0 && sex!=1) {
    print("sex must be 0 or 1")
    return(-1)
  }
  if(sex==0) {
    y <- HRR*1.92
  } else {
    y <- HRR*1.67
  }
  trimpexp <- time*HRR*0.64*exp(y)
  return(trimpexp)
}
