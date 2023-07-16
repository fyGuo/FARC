library(devtools)
devtools::load_all()

# test whether the output class of fars_summarize_years is a data.frame
expect_s3_class(fars_summarize_years(2013:2015), "data.frame")
