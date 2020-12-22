#' Clean country monitoring and evaluation excel tool data (key performance indicators)
#'
#' @param inputfile a data frame produced by the merge_kpi function
#'
#' @importFrom dplyr mutate case_when across all_of
#'
#' @author Alice Carr, Alex Spina

## TODO: Decide whether this is an internal function or if is used in RMD

clean_kpi <- function(inputfile) {


  ## define date variables
  DATEVARS <- var_dict$var_name[var_dict$var_type == "date_vars"]

  ## define numeric variables
  NUMVARS  <- var_dict$var_name[var_dict$var_type == "num_vars"]

  ## define yes no variables
  YNVARS   <- var_dict$var_name[var_dict$var_type == "yn_vars"]

  ## define yes no probably vars
  YNPVARS  <- var_dict$var_name[var_dict$var_type == "ynp_vars"]

  ## clean the wide version
  if (wide) {

    ## TODO: fix country coding to be based off a common dictionary

    inputfile <- dplyr::mutate(inputfile,
                               country = dplyr::case_when(
                                 country == "Burkina Faso"         ~       "Burkina Faso",
                                 country == "COMORES"              ~       "Comores",
                                 country == "CÔTE D'IVOIRE"        ~       "Cote d'Ivoire",
                                 country == "Equatorial Guinea"    ~       "Equatorial Guinea",
                                 country == "Eswatini"             ~       "Eswatini",
                                 country == "GABON"                ~       "Gabon",
                                 country == "Ghana"                ~       "Ghana",
                                 country == "NIGER"                ~       "Niger",
                                 country == "République du Congo"  ~       "Republic of Congo",
                                 country == "SAO TOME ET PRINCIPE" ~       "Sao Tome & Principe",
                                 country == "SENEGAL"              ~       "Senegal",
                                 country == "Seychelles"           ~       "Seychelles",
                                 country == "Sierra Leone"         ~       "Sierra Leone",
                                 country == "South Sudan"          ~       "South Sudan",
                                 country == "Uganda"               ~       "Uganda")


    )

    ## fix numeric variables
    inputfile <- dplyr::mutate(inputfile,
                               dplyr::across(dplyr::all_of(NUMVARS), as.numeric))

    ## TODO: make date fixing in to a separate function
    ## date fixing plan:
    ## if char month, remove accents translate to english
    ## nb. (if contains date and year will be kept, else NA)
    ## if length of 5 and starts with a 4 -> as.date excel origin
    ## if in normal date format then just leave
    ## use lubridate to catch all date formats

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
      if (num_dropped > 0) {
        warning(
          paste0(num_dropped, " date entries dropped from ", j)
        )
      }
    }

    ## fix yes/no and yes/no/partially variables
    inputfile <- dplyr::mutate(inputfile,
                               dplyr::across(dplyr::all_of(c(YNVARS, YNPVARS)),
                                             ~ dplyr::case_when(
                                               . %in% c("Yes", "yes", "YES",
                                                        "Oui", "oui", "OUI",
                                                        "Y", "y",
                                                        "Totalement")                     ~ "Yes",
                                               . %in% c("No",  "no",  "NO",
                                                        "Non", "non", "NON",
                                                        "N", "n")                         ~ "No",
                                               . %in% c("Partially", "partially",
                                                        "Partiellement", "partiellement") ~ "Partially"
                                             )))

  } else {
    ## clean the long version


    ## clean reporting frequency
    inputfile <- mutate(inputfile,
                        reporting_frequency = case_when(
                          reporting_frequency %in% c("Bi-weekly",
                                                     "Bihebdomadaire",
                                                     "Bi-hebdomadaire"
                                                     )                  ~ "Biweekly",
                          reporting_frequency %in% c("Monthly",
                                                     "Mensuelle")       ~ "Monthly",
                          reporting_frequency %in% c("Weekly",
                                                     "Hebdomadaire")    ~ "Weekly",
                          reporting_frequency %in% ("Semestruelle")     ~ "Biannually"
                        ))

    ## TODO: clean grps (french come out all weird)
    ## TODO: clean countries as above based on dictionary
    ## TODO: clean each of the str rows using YN and YNP vars defined above







  }






}
