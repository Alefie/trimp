binlik <- function(probH, h, t) {
  n <- h+t
  lik <- choose(n,h)*probH^h*(1-probH)^(n-h)
  print(lik)
}

## testwerte

binlik(0.5, 40, 60)
binlik(0.5, 50, 50)