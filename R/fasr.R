#' Read a fars file
#'
#' @param filename Name of a csv file.
#' @import readr
#' @returns A tibble.
#' @examples
#' library(farc)
#' library(dplyr)
#' system.file("extdata", 'accident_2013.csv.bz2' , package = "farc") %>% fars_read()
#'
#' @details The function may return errors when the filename cannot be found
#' @export
#
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Makde filenames in a format of accident_YY.csv.bz2
#' @param year Year number
#' @returns A character "accident_year.csv.bz2" where year is the input
#' @import dplyr

#' @examples
#' library(dplyr)
#' library(farc)
#' make_filename(2013)
#' @details The function may return errors when year cannot be converted to an integer
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  filename <- paste0("accident_", year, ".csv.bz2")
  system.file("extdata", filename , package = "farc") %>% sprintf()
}

#' Read in data and make sure years in the data are consistent with their fileanmes
#' @param years A string of year numbers
#' @import dplyr
#' @returns A list of dataframes. Each data frame contains two columns, month and year
#' @details The function may return errors when any of the years cannot be found in the filename
#' @examples
#' library(dplyr)
#' library(farc)
#' make_filename(2013:2015)
#'
#' @export

fars_read_years <- function(years = c(2013, 2014, 2015)) {
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

#' Summarize number of months appearing in the fars data by year
#' @param years A string of year numbers
#' @import dplyr
#' @import tidyr
#' @returns A dataframe summarizing number of months appearing in the fars data by year
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(farc)
#' fars_summarize_years(2013:2014)
#' @export

fars_summarize_years <- function(years = c(2013, 2014, 2015)) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot the map of incidences occuring in the state and the year set by the use
#' @param state.num state number
#' @param year Year number
#' @import dplyr
#' @import tidyr
#' @import maps
#' @returns A dataframe summarizing number of months appearing in the fars data by year
#' @details The function may return errors when any of the years cannot be found in the filename.
#' Or the stat.num is invalid.
#' @examples
#' library(dplyr)
#' library(maps)
#' library(farc)
#' fars_map_state(1, 2013)
#'
#' @export

fars_map_state <- function(state.num, year = c(2013, 2014, 2015)) {
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
