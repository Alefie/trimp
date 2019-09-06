context("totimetest")

test_that("test that time is correct", {
  expect_equal(totime(3), array(c(0, 0, 3)))
  expect_equal(totime(67), array(c(0, 1, 7)))
  expect_equal(totime(3676), array(c(1, 1, 16)))
})
