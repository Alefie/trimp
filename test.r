binlik <- function(probH, h, t) {
  n <- h+t
  lik <- choose(n,h)*probH^h*(1-probH)^(n-h)
  print(lik)
}
