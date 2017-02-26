library(fars)

test_that("fars", {
	expect_that(fars_summarize_years(c(2014))[[11,2]], equals(2714))
	expect_that(fars_summarize_years(c(2014))[[12,2]], equals(2604))
	expect_that(make_filename("2014"),matches("inst/extdata/accident_2014.csv.bz2"))
})

