#' Reads a data file and creates a dataframe with dplyr.
#' @param filename input character vector
#' @return dplyr::tbl_df by reading the whole file
#' @import readr
#' @import dplyr
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Returns a string with a file name made from a year like
#' "accident_1987.csv.bz2"
#' @param year input numeric
#' @return character vector
#' @examples
#' make_filename(1987)
make_filename <- function(year) {
        year <- as.integer(year)
        fname <- sprintf("accident_%d.csv.bz2", year)
        system.file("extdata", fname, package = "fars")
}

#' The input is a vector of years and for each year
#' it reads a file and selects the columns Month and Year.
#' It modifies the variable dat and .
#' We will end up with repeated rows (one per observation)
#' of what happened in a given Mmonth and Year
#' @param years input numeric vector
#' @return tibble with columns for all the year files
#' @import dplyr
#' @export
#' @examples
#' fars_read_years(c(2014,2015))
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' It starts with having one row per observation
#' and columns for Month and years, what this
#' function does is to count how many observations
#' happen by Month and Year
#' @param years input numeric vector
#' @return tibble
#' @import dplyr
#' @import tidyr
#' @import magrittr
#' @export
#' @examples
#' fars_summarize_years(c(2014,2015))
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' The final function produces a picture plotting
#' the observations (accidents) that happen in a state
#' in a given year.
#' It takes only the observation that belong in the state (using filter)
#'
#' @param state.num input character
#' @param year input numeric
#' @return plot of observations
#' @export
#' @import dplyr
#' @import maps
#' @examples
#' fars_map_state("21",2014)
#' fars_map_state("30",2015)
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
