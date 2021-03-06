% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{The input is a vector of years and for each year
it reads a file and selects the columns Month and Year.
It modifies the variable dat and .
We will end up with repeated rows (one per observation)
of what happened in a given Mmonth and Year}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{input numeric vector}
}
\value{
tibble with columns for all the year files
}
\description{
The input is a vector of years and for each year
it reads a file and selects the columns Month and Year.
It modifies the variable dat and .
We will end up with repeated rows (one per observation)
of what happened in a given Mmonth and Year
}
\examples{
fars_read_years(c(2014,2015))
}
