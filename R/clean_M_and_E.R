#' Internal function to clean country monitoring and evaluation excel tool data
#' (key performance indicators)
#'
#' @param inputfile a data frame produced by the [covidmonitor::merge_kpi()]
#'
#' @param var_dict data dictionary sourced in [covidmonitor::merge_kpi()]
#'
#' @param clean_dict data cleaning dictionary sourced in [covidmonitor::merge_kpi()]
#'
#' @param wide logical (TRUE/FALSE) of whether the output data frame should be
#' in wide format (default), else will produce long format data - defined in
#' [covidmonitor::merge_kpi()]
#'
#' @importFrom dplyr mutate case_when across all_of
#' @importFrom matchmaker match_vec
#'
#' @author Alice Carr, Alex Spina

clean_kpi <- function(inputfile,
                      var_dict,
                      clean_dict,
                      wide) {


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

    ## dictionary recode variable names (once so dont have to repeat lower down)
    inputfile$country <- matchmaker::match_vec(inputfile$country,
                                         dictionary = clean_dict,
                                         from = "old",
                                         to = "new")


    ## fix numeric variables
    inputfile <- dplyr::mutate(inputfile,
                               dplyr::across(dplyr::all_of(NUMVARS), as.numeric))

    ## fix date variables
    inputfile <- dplyr::mutate(inputfile,
                               dplyr::across(dplyr::all_of(DATEVARS), clean_dates))

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

    ## return the dataset
    inputfile

  } else {
    ## clean the long version

    ## dictionary recode variable names (once so dont have to repeat lower down)
    inputfile$country <- matchmaker::match_vec(inputfile$country,
                                               dictionary = clean_dict,
                                               from = "old",
                                               to = "new")

    ## clean reporting frequency
    inputfile <- dplyr::mutate(inputfile,
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

    ## clean the str rows using YN and YNP vars defined above
    inputfile <- dplyr::mutate(inputfile,
                        str_vars = dplyr::case_when(
                          str_vars %in% c("Yes", "yes", "YES",
                                   "Oui", "oui", "OUI",
                                   "Y", "y",
                                   "Totalement")                     ~ "Yes",
                          str_vars %in% c("No",  "no",  "NO",
                                   "Non", "non", "NON",
                                   "N", "n")                         ~ "No",
                          str_vars %in% c("Partially", "partially",
                                   "Partiellement", "partiellement") ~ "Partially"
                        ))

    ## clean dat_var with function
    inputfile$dat_vars <- clean_dates(inputfile$dat_vars)

    ## return the dataset
    inputfile

  }


}
