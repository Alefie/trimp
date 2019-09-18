context("nohrtest")

test_that("test that no Hr gives error", {
  expect_equal(activity(data.frame(time=as.POSIXct("2019-01-01 15:00:00 UTC"), latitude=1, longitude=1, altitude=100, distance=0.1, speed=2, cadence=10, heart_rate=as.numeric(c("na","na","na")))), -1)
})
