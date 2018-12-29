#' File Reading Function
#'
#' This is a function that first checks if a file exists
#' otherwise uses the read_csv function from readr
#' to read the file suppressing the messages during that
#' process. Finally it converts the data read above into
#' a data frame
#'
#' @details For more information, see:
#' \itemize{
#'   \item{\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}}
#'   \item{\url{https://en.wikipedia.org/wiki/Fatality_Analysis_Reporting_System}}
#' }
#'
#' @param filename A name for the file to be read
#' @return The data frame created after reading the file \code{filename}
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' fars_read("accident_2015.csv.bz2")
#' @note To generate file name use: \code{\link{make_filename}}
#' @seealso \link{make_filename}
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' File Name Printing Function for a Particular Year
#'
#' This is a function that prints the respective filename
#' for the \code{year} value provided
#'
#' @param year A numeric or character value for the year
#' @return The file name for the respective year in zipped-csv format
#' @examples
#' make_filename(2013)
#' make_filename(2014)
#' @seealso \link{make_filename}
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata",
             sprintf("accident_%d.csv.bz2", year),
             package = "Coursera3Sub",
             mustWork = TRUE)
}

#' Extract Data for Given Years
#'
#' This is a function that reads all data for the
#' given vector of years and returns a warning if
#' any of the entered years is not present in the
#' data. The function also makes prints separate
#' filenames for every year by calling the function
#' make_filename
#'
#' @param years A vector of numeric or character values of years
#' @return The data frame consisting of all values of months and years, else NULL
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom magrittr "%>%"
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014))
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = "year") %>%
        dplyr::select("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarising Data for Years
#'
#' This is a function that reads all data for the
#' given vector of years and groups the data by
#' month and year - one record for every month and
#' year combination
#'
#' @param years A vector of numeric or character values of years
#' @return The data frame consisting of count of accidents by month and year
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by("year", "MONTH") %>%
    dplyr::summarize(n = "n()") %>%
    tidyr::spread("year", "n")
}

#' Plotting Accidents on Map for Given State
#'
#' This is a function that takes a particular State
#' as an Input along with a particular Year and plots
#' the accidents on the map of that State. It returns
#' and error if the State provided is invalid and also
#' returns a blank plot if there are no accidents to
#' plot
#'
#' @param state.num A numeric/integer State Code
#' \tabular{cc}{
#'   \strong{State Code} \tab \strong{State Name}    \cr
#'   01 \tab  Alabama              \cr
#'   02 \tab  Alaska               \cr
#'   04 \tab  Arizona              \cr
#'   05 \tab  Arkansas             \cr
#'   06 \tab  California           \cr
#'   08 \tab  Colorado             \cr
#'   09 \tab  Connecticut          \cr
#'   10 \tab  Delaware             \cr
#'   11 \tab  District of Columbia \cr
#'   12 \tab  Florida              \cr
#'   13 \tab  Georgia              \cr
#'   15 \tab  Hawaii               \cr
#'   16 \tab  Idaho                \cr
#'   17 \tab  Illinois             \cr
#'   18 \tab  Indiana              \cr
#'   19 \tab  Iowa                 \cr
#'   20 \tab  Kansas               \cr
#'   21 \tab  Kentucky             \cr
#'   22 \tab  Louisiana            \cr
#'   23 \tab  Maine                \cr
#'   24 \tab  Maryland             \cr
#'   25 \tab  Massachusetts        \cr
#'   26 \tab  Michigan             \cr
#'   27 \tab  Minnesota            \cr
#'   28 \tab  Mississippi          \cr
#'   29 \tab  Missouri             \cr
#'   30 \tab  Montana              \cr
#'   31 \tab  Nebraska             \cr
#'   32 \tab  Nevada               \cr
#'   33 \tab  New Hampshire        \cr
#'   34 \tab  New Jersey           \cr
#'   35 \tab  New Mexico           \cr
#'   36 \tab  New York             \cr
#'   37 \tab  North Carolina       \cr
#'   38 \tab  North Dakota         \cr
#'   39 \tab  Ohio                 \cr
#'   40 \tab  Oklahoma             \cr
#'   41 \tab  Oregon               \cr
#'   42 \tab  Pennsylvania         \cr
#'   43 \tab  Puerto Rico          \cr
#'   44 \tab  Rhode Island         \cr
#'   45 \tab  South Carolina       \cr
#'   46 \tab  South Dakota         \cr
#'   47 \tab  Tennessee            \cr
#'   48 \tab  Texas                \cr
#'   49 \tab  Utah                 \cr
#'   50 \tab  Vermont              \cr
#'   51 \tab  Virginia             \cr
#'   52 \tab  Virgin Islands       \cr
#'   53 \tab  Washington           \cr
#'   54 \tab  West Virginia        \cr
#'   55 \tab  Wisconsin            \cr
#'   56 \tab  Wyoming
#' }
#' @param year A numeric or character value for a particular Year
#' @return The map of accidents for the entered State and Year
#' @importFrom dplyr filter_
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' fars_map_state(49,2015)
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, "STATE" == state.num)
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
