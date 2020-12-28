#' Clean country monitoring and evaluation excel tool data (key performance indicators)
#'
#' @param inputfile a data frame produced by the [covidmonitor::merge_kpi()]
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

## TODO: add in line using new date cleaning function

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
