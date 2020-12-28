#' Clean messy date variables
#'
#' @param x a vector of dates to be cleaned
#'
#' @param quietly a TRUE/FALSE of whether to show warnings for dropped observations
#' (default is FALSE, so warnings are shown)
#'
#' @importFrom lubridate parse_date_time
#'
#' @details
#' Any observation with only text will be set to missing (NA).
#' Properly spelt text names of months will be translated from French and Portuguese to English.
#' If there is a date and year along with the month name this will be changed to
#' an appropriate date (nb. if there is an ending to date e.g. *1st*, *2nd* etc. this
#' will not work).
#'
#' Dates imported from Excel are identified by containing 5 numbers and starting
#' with the number 4. This will only work for dates after July 2009 and before
#' November 2036.(This could be updated to attempt all those containing any 5
#' numbers).
#'
#' Anything else in a date formate will be kept; regardless of the separator
#' (dots, slashes or dashes).
#'
#' All observations are then passed through [lubridate::parse_date_time()] to
#' produce appropriate dates.
#'
#' @author Alice Carr, Alex Spina
#' @export


clean_dates <- function(x,
                        quietly = FALSE) {



  ## translate month names
  month_translaters <- data.frame(en = month.name,
                                  fr = c(
                                    "janvier",
                                    "fevrier",
                                    "mars",
                                    "avril",
                                    "mai",
                                    "juin",
                                    "juillet",
                                    "aout",
                                    "septembre",
                                    "octobre",
                                    "novembre",
                                    "decembre"),
                                  pt = c(
                                    "janeiro",
                                    "fevereiro",
                                    "março",
                                    "abril",
                                    "maio",
                                    "junho",
                                    "julho",
                                    "agosto",
                                    "setembro",
                                    "outubro",
                                    "novembro",
                                    "dezembro"
                                  ))


  ## store original variable for comparing drops later
  og_var <- x

  ## make in to a character variable
  x <- as.character(x)

  ## remove accented characters
  x <- iconv(x,
             to = 'ASCII//TRANSLIT')

  ## for each of the months
  for (i in 1:nrow(month_translaters)) {
    ## replace the spelling
    x <- gsub(paste0(month_translaters$fr[i],
                                  "|",
                                  month_translaters$pt[i]),
                           month_translaters$en[i],
                           x,
                           ignore.case = TRUE,
                           perl = TRUE)
  }



  ## find excel date rows
  exceldaterows <- nchar(x) == 5 &
    substr(x, 1, 1) == "4" &
    !is.na(x)

  ## get a vector of the values to be changed to dates - as numeric
  exceldatenumerics <- suppressWarnings(
    as.numeric(
      x[exceldaterows]
    )
  )

  ## change excel compatibles to dates
  exceldatedates <- as.Date(
    exceldatenumerics,
    origin = "1899-12-30")

  ## put excel dates back in as character
  x[exceldaterows] <- as.character(exceldatedates)

  ## use lubridate to change all of the diff posibilities to POSIXcT date
  x <- suppressWarnings(
    lubridate::parse_date_time(x,
                               orders = c("ymd","Ymd","dmy","dmY",
                                          "%Y%m%d","%y%m%d","%d%m%y",
                                          "%Y-%m-%d","%y-%m-%d","%d-%m-%y",
                                          "%Y.%m.%d","%y.%m.%d","%d.%m.%y",
                                          "%Y/%m/%d","%y/%m/%d","%d/%m/%y",
                                          "dBY","ymd HMS","Ymd HMS"))
  )

  ## change back to normal date format because POSIXcT annoying downstream
  x <- as.Date(x)

  ## count how many were dropped
  num_dropped <- sum(is.na(x) - is.na(og_var))

  ## give warning on number of obs dropped
  if (num_dropped > 0 & !quietly) {
    warning(
      paste0(num_dropped, " date entries not usable")
    )
  }

  ## return dataset
  x
}
