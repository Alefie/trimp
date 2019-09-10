context("totimetest")

test_that("test that time is correct", {
  expect_equal(totime(3), array(c(0, "00", "03")))
  expect_equal(totime(67), array(c(0, "01", "07")))
  expect_equal(totime(3676), array(c(1, "01", "16")))
})
