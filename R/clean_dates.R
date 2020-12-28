#' Clean messy date variables
#'
#' @param inputfile a data frame
#'
#' @param datevars a vector of names for date variables to be cleaned
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


clean_dates <- function(inputfile,
                        datevars,
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





  ## for each date variable
  for (j in DATEVARS) {

    ## store original variable for comparing drops later
    og_var <- inputfile[[j]]

    ## make in to a character variable
    inputfile[[j]] <- as.character(inputfile[[j]])

    ## remove accented characters
    inputfile[[j]] <- iconv(inputfile[[j]],
                            to = 'ASCII//TRANSLIT')


    ## for each of the months
    for (i in 1:nrow(month_translaters)) {
      ## replace the spelling
      inputfile[[j]] <- gsub(paste0(month_translaters$fr[1],
                                    "|",
                                    month_translaters$pt[1]),
                             month_translaters$en[i],
                             inputfile[[j]],
                             ignore.case = TRUE,
                             perl = TRUE)
    }

    ## find excel date rows
    exceldaterows <- nchar(inputfile[[j]]) == 5 &
      substr(inputfile[[j]], 1, 1) == "4" &
      !is.na(inputfile[[j]])

    ## get a vector of the values to be changed to dates - as numeric
    exceldatenumerics <- suppressWarnings(
      as.numeric(
        inputfile[[j]][exceldaterows]
      )
    )

    ## change excel compatibles to dates
    exceldatedates <- as.Date(
      exceldatenumerics,
      origin = "1899-12-30")

    ## put back in as character
    inputfile[exceldaterows, j] <- as.character(exceldatedates)

    ## use lubridate to change all of the diff posibilities to POSIXcT date
    inputfile[[j]] <- suppressWarnings(
      lubridate::parse_date_time(inputfile[[j]],
                                 orders = c("ymd","Ymd","dmy","dmY",
                                            "%Y%m%d","%y%m%d","%d%m%y",
                                            "%Y-%m-%d","%y-%m-%d","%d-%m-%y",
                                            "%Y.%m.%d","%y.%m.%d","%d.%m.%y",
                                            "%Y/%m/%d","%y/%m/%d","%d/%m/%y",
                                            "dBY","ymd HMS","Ymd HMS"))
    )

    ## change back to normal date format because POSIXcT annoying downstream
    inputfile[[j]] <- as.Date(inputfile[[j]])

    ## count how many were dropped
    num_dropped <- sum(is.na(inputfile[[j]]) - is.na(og_var))

    ## give warning on number of obs dropped
    if (num_dropped > 0 & !quietly) {
      warning(
        paste0(num_dropped, " date entries dropped from ", j)
      )
    }
  }


}



