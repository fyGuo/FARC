test_that("fars_summarize_years() output the correct class", {
  expect_s3_class(fars_summarize_years(2013:2015), "data.frame")
})

