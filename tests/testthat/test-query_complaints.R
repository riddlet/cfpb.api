test_that("query complaints works", {
  testthat::skip_if_offline()
  expect_equal(nrow(query_complaints(size = 1)), 1)

  temp <- query_complaints(
    company = "JPMORGAN CHASE & CO.", size = 1000,
    date_received_min = "2021-01-01",
    date_received_max = "2021-01-05"
  )
  expect_equal(as.Date(min(temp$date_received)), as.Date("2021-01-01"))
  expect_equal(as.Date(max(temp$date_received)), as.Date("2021-01-05"))
})
