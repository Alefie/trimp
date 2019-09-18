context("wrongsextest")

test_that("test that wrong sex not possible in athlete", {
  expect_error(athlete(HRest=50, HRMax=200, name="Tester", sex="Alien"))
  expect_error(athlete(HRest=-20, HRMax=200))
  expect_error(athlete(HRest=50, HRMax=500))
  expect_error(athlete(Hrest=100, HRMax=80))
})
