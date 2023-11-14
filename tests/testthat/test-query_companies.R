test_that("company query works", {
  testthat::skip_if_offline()
  expect_equal(query_companies(text = "chase"), "JPMORGAN CHASE & CO.")

  expect_error(query_companies(text = c("chase", "america")))

  # is not working here, or on the direct API test (https://cfpb.github.io/api/ccdb/api.html)
  #expect_equal(length(query_companies(text = "bank", size = 5)), 5)
})
