#' Merge weekly report monitoring and evaluation data.
#' Currently based on Guinea template; to be further in case of non-templated
#' documents
#'
#' @param inputdirectory path to folder containing datasets
#'
#' @param outputdirectory path to folder where merged file is to be saved
#'
#' @param outputname character string to name merged file
#'
#' @param template logical (TRUE/FALSE) of whether the input data frame fits
#' the standard template
#'
#' @param wide logical (TRUE/FALSE) of whether the output data frame should be
#' in wide format (default), else will produce long format data (currently only
#' supports wide format)
#'
#' @importFrom openxlsx loadWorkbook read.xlsx
#' @importFrom tidyr fill
#'
#' @author Alice Carr
#' @export



merge_mne <- function(inputdirectory,
                     outputdirectory = tempdir(),
                     outputname = "filename",
                     template = TRUE,
                     wide = TRUE) {

  # for debug
  # files <- "Weekly Indicator Tool_FR GUINEE  S37.xlsx"

  # Read in file list. Creat output directory.
  files <- base::list.files(path = inputdirectory, full.names = TRUE)

  # create folder for output
  base::dir.create(outputdirectory, showWarnings = FALSE)

  # upload will be the output file
  upload <- base::as.data.frame(base::matrix(nrow = 0, ncol = base::length(files)))

  base::colnames(upload) <- base::rep("Record", base::length(files))
  # handling dates
  dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                      "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                      "Ydm HM","Ydm HMS","ydm HM","ydm HMS")

  # col_types = c("skip","skip","skip","skip","text","text","text","text","text","text","skip")

  # for each file listed
  for (f in 1:base::length(files)) {

    # load excel file
    wb <- openxlsx::loadWorkbook(files[f])
    # read in sheet of interest
    table_sheet1 <- openxlsx::read.xlsx(wb, sheet = 2, skipEmptyRows = TRUE,
                                        colNames = FALSE, rows = 1:122,
                                        skipEmptyCols = TRUE, detectDates = TRUE)

    # NB. if template is same, this could go after sheet is loaded in if we dont already know what template countries have used

    ## for those fitting the standard template
    if (template){

    ## reshaping the dataframe

    # fill-in missing rows from above
    table_sheet1 <- tidyr::fill(table_sheet1, X1)
    # drop rows with "Reponse"
    table_sheet1 <- table_sheet1[!grepl("Réponse", table_sheet1$X6), ]
    # add two empty rows at bottom
    table_sheet1[nrow(table_sheet1) + 2, ] <- NA
    # shuffle last line around so vars and responses next to eachother
    table_sheet1$X1[nrow(table_sheet1) - 1] <- table_sheet1$X3[nrow(table_sheet1) - 2]
    table_sheet1$X2[nrow(table_sheet1) - 1] <- table_sheet1$X4[nrow(table_sheet1) - 2]
    table_sheet1$X1[nrow(table_sheet1)]     <- table_sheet1$X5[nrow(table_sheet1) - 2]
    table_sheet1$X2[nrow(table_sheet1)]     <- table_sheet1$X6[nrow(table_sheet1) - 2]

    # over-write empties with values in column 6
    table_sheet1$X2 <- ifelse(is.na(table_sheet1$X2),
                              table_sheet1$X6,
                              table_sheet1$X2)

    # drop empties and previous response column
    table_sheet1 <- table_sheet1[,-c(3:6)]
    # drop variables occurring twice
    table_sheet1 <- table_sheet1[!duplicated(table_sheet1$X1), ]

    ## Dealing with the observation columns (free text comments)

    # only keep non empty comment rows
    observations <- na.omit(as.data.frame(table_sheet1$X8))

    # Read in empty template of observations
    # make sure file path correct
    checkobservations <- utils::read.csv(
      system.file("extdata", "Observations_empty_check.csv",
                  package = "covidmonitor"))

    # bind datasets together (NOTE: this will probably throw an error if diff nrow)
    observations <- cbind(observations, checkobservations)

    ## NOTE: THIS IS CURRENTLY NOT WORKING PROPERLY
    # create new var to check if comments identical, else NA
    observations$trueobservation <- ifelse(
      observations$`table_sheet1$X8` == observations$table_sheet1.X8,
      NA,
      observations$`table_sheet1$X8`)

    # create a vector of all observation names
    obslist <- data.frame(paste("Observation", seq(1, 43), sep = "_"))
    # bind to observation column checker from above
    observations <- cbind(observations, obslist)
    # only keep indicator name and if comments have been added vars
    observations <- subset(observations, select = c(3, 4))
    # rename to fit table_sheet1
    colnames(observations) <- c("X2","X1")

    # only keep indicators and obs
    table_sheet1 <- subset(table_sheet1, select = c(1, 2))
    # chuck in comments below as separate variables
    table_sheet1 <- rbind(table_sheet1, observations)

    }

    ## for non standard template files

    # else{
    #
    # }

    #make upload dataframe
    upload["Country",f]                                   <- table_sheet1$X2[  1]
    upload["Date",f]                                      <- table_sheet1$X2[  2]
    upload["Epi_week",f]                                  <- table_sheet1$X2[  3]
    upload["Budget_COVID",f]                              <- table_sheet1$X2[  4]
    upload["Budget_total",f]                              <- table_sheet1$X2[  5]
    upload["Indicator_1",f]                               <- table_sheet1$X2[  6]
    upload["Intervetnion_funding",f]                      <- table_sheet1$X2[  7]
    upload["Intervetnion_funding_total",f]                <- table_sheet1$X2[  8]
    upload["Indicator_2",f]                               <- table_sheet1$X2[  9]
    upload["ESGI_workforce",f]                            <- table_sheet1$X2[ 10]
    upload["ESGI_status",f]                               <- table_sheet1$X2[ 11]
    upload["Indicator_3",f]                               <- table_sheet1$X2[ 12]
    upload["ESGI_national_staff",f]                       <- table_sheet1$X2[ 13]
    upload["Indicator_4",f]                               <- table_sheet1$X2[ 14]
    upload["ESGI_country_staff",f]                        <- table_sheet1$X2[ 15]
    upload["Indicator_5",f]                               <- table_sheet1$X2[ 16]
    upload["ESGI_support",f]                              <- table_sheet1$X2[ 17]
    upload["ESGI_virtual_training",f]                     <- table_sheet1$X2[ 18]
    upload["Weekly_meeting_reps_authorities",f]           <- table_sheet1$X2[ 19]
    upload["Meeting_reps_head",f]                         <- table_sheet1$X2[ 20]
    upload["Travelers_tested_total",f]                    <- table_sheet1$X2[ 21]
    upload["Travelers_positive",f]                        <- table_sheet1$X2[ 22]
    upload["Indicator_6",f]                               <- table_sheet1$X2[ 23]
    upload["Travelers_quarentined",f]                     <- table_sheet1$X2[ 24]
    upload["Travelers_tracked",f]                         <- table_sheet1$X2[ 25]
    upload["Travelers_positive_surveillance",f]           <- table_sheet1$X2[ 26]
    upload["entry_points_COVID",f]                        <- table_sheet1$X2[ 27]
    upload["entry_points_total",f]                        <- table_sheet1$X2[ 28]
    upload["Indicator_7",f]                               <- table_sheet1$X2[ 29]
    upload["Indicator_8",f]                               <- table_sheet1$X2[ 30]
    upload["Borders_reopened",f]                          <- table_sheet1$X2[ 31]
    upload["Borders_reopened_land_date",f]                <- table_sheet1$X2[ 32]
    upload["Borders_reopened_air_date",f]                 <- table_sheet1$X2[ 33]
    upload["Borders_reopened_maritime_date",f]            <- table_sheet1$X2[ 34]
    upload["Cum_num_confirmed_cases",f]                   <- table_sheet1$X2[ 35]
    upload["New_cases_confirmed_total",f]                 <- table_sheet1$X2[ 36]
    upload["New_cases_confirmed_known_contacts",f]        <- table_sheet1$X2[ 37]
    upload["Indicator_9",f]                               <- table_sheet1$X2[ 38]
    upload["New_cases_reported_total",f]                  <- table_sheet1$X2[ 39]
    upload["Reported_cases_investigated_24h",f]           <- table_sheet1$X2[ 40]
    upload["Indicator_10",f]                              <- table_sheet1$X2[ 41]
    upload["Cum_num_contacts",f]                          <- table_sheet1$X2[ 42]
    upload["Contacts_followed_7days",f]                   <- table_sheet1$X2[ 43]
    upload["Contacts_followed_total",f]                   <- table_sheet1$X2[ 44]
    upload["Contacts_reviewed_24h",f]                     <- table_sheet1$X2[ 45]
    upload["Indicator_11",f]                              <- table_sheet1$X2[ 46]
    upload["Samples_COVID_48h",f]                         <- table_sheet1$X2[ 47]
    upload["Lab_results_communicated",f]                  <- table_sheet1$X2[ 48]
    upload["Indicator_12",f]                              <- table_sheet1$X2[ 49]
    upload["Cum_num_tests_prev_week",f]                   <- table_sheet1$X2[ 50]
    upload["Cum_num_tests_curr_week",f]                   <- table_sheet1$X2[ 51]
    upload["Indicator_13",f]                              <- table_sheet1$X2[ 52]
    upload["Screening_labs",f]                            <- table_sheet1$X2[ 53]
    upload["New_tests_curr_week_total",f]                 <- table_sheet1$X2[ 54]
    upload["New_tests_curr_week_decentralized",f]         <- table_sheet1$X2[ 55]
    upload["Indicator_14",f]                              <- table_sheet1$X2[ 56]
    upload["COVID_treatment_centres",f]                   <- table_sheet1$X2[ 57]
    upload["Total_beds_reserved_suspected",f]             <- table_sheet1$X2[ 58]
    upload["Total_beds_occupied_suspected",f]             <- table_sheet1$X2[ 59]
    upload["Indicator_15",f]                              <- table_sheet1$X2[ 60]
    upload["COVID_treatment_centres_subnational",f]       <- table_sheet1$X2[ 61]
    upload["Indicator_16",f]                              <- table_sheet1$X2[ 62]
    upload["Total_beds_reserved_confirmed",f]             <- table_sheet1$X2[ 63]
    upload["Total_beds_occupied_confirmed",f]             <- table_sheet1$X2[ 64]
    upload["Indicator_17",f]                              <- table_sheet1$X2[ 65]
    upload["Total_beds_reserved_severe",f]                <- table_sheet1$X2[ 66]
    upload["Total_beds_occupied_severe",f]                <- table_sheet1$X2[ 67]
    upload["Indicator_18",f]                              <- table_sheet1$X2[ 68]
    upload["Cum_num_deaths",f]                            <- table_sheet1$X2[ 69]
    upload["Indicator_19",f]                              <- table_sheet1$X2[ 70]
    upload["Cum_num_confirmed_cases_healthcare_worker",f] <- table_sheet1$X2[ 71]
    upload["Indicator_20",f]                              <- table_sheet1$X2[ 72]
    upload["Confirmed_cases_healthcare_7day",f]           <- table_sheet1$X2[ 73]
    upload["Indicator_21",f]                              <- table_sheet1$X2[ 74]
    upload["Total_districts",f]                           <- table_sheet1$X2[ 75]
    upload["Total_districts_1case",f]                     <- table_sheet1$X2[ 76]
    upload["Total_districts_1case_7day",f]                <- table_sheet1$X2[ 77]
    upload["Indicator_22",f]                              <- table_sheet1$X2[ 78]
    upload["Deaths_confirmed_cases_7day",f]               <- table_sheet1$X2[ 79]
    upload["Indicator_23",f]                              <- table_sheet1$X2[ 80]
    upload["Confirmed_cases_isolation_within1day_7day",f] <- table_sheet1$X2[ 81]
    upload["Indicator_24",f]                              <- table_sheet1$X2[ 82]
    upload["Healthcare_worker_total",f]                   <- table_sheet1$X2[ 83]
    upload["Healthcare_worker_COVID_trained",f]           <- table_sheet1$X2[ 84]
    upload["Indicator_25",f]                              <- table_sheet1$X2[ 85]
    upload["Indicator_26",f]                              <- table_sheet1$X2[ 86]
    upload["RCCE_outreach_percent",f]                     <- table_sheet1$X2[ 87]
    upload["Public_transport_percent_means",f]            <- table_sheet1$X2[ 88]
    upload["Masks_total_percent",f]                       <- table_sheet1$X2[ 89]
    upload["Masks_correct_percent",f]                     <- table_sheet1$X2[ 90]
    upload["Hand_hygiene_percent",f]                      <- table_sheet1$X2[ 91]
    upload["Primare_care_consultations_month",f]          <- table_sheet1$X2[ 92]
    upload["Primare_care_consultations_month_2019",f]     <- table_sheet1$X2[ 93]
    upload["Indicator_27",f]                              <- table_sheet1$X2[ 94]
    upload["Infant_survival_d_t_p_month",f]               <- table_sheet1$X2[ 95]
    upload["Infant_survival_d_t_p_month_2019",f]          <- table_sheet1$X2[ 96]
    upload["Indicator_28",f]                              <- table_sheet1$X2[ 97]
    upload["Outpatients_month",f]                         <- table_sheet1$X2[ 98]
    upload["Outpatients_month_2019",f]                    <- table_sheet1$X2[ 99]
    upload["Indicator_29",f]                              <- table_sheet1$X2[100]
    upload["HIV_treated_month",f]                         <- table_sheet1$X2[101]
    upload["HIV_treated_month_2019",f]                    <- table_sheet1$X2[102]
    upload["Indicator_30",f]                              <- table_sheet1$X2[103]
    upload["Formal_procurment_national",f]                <- table_sheet1$X2[104]
    upload["Essential_purhases_monitoring",f]             <- table_sheet1$X2[105]
    upload["Indicator_31",f]                              <- table_sheet1$X2[106]
    upload["Submitted_contaminaion_route",f]              <- table_sheet1$X2[107]
    upload["Updated_submitted_tracing",f]                 <- table_sheet1$X2[108]
    upload["Presentation_COVID",f]                        <- table_sheet1$X2[109]
    upload["Observation_1",f]                             <- table_sheet1$X2[110]
    upload["Observation_2",f]                             <- table_sheet1$X2[111]
    upload["Observation_3",f]                             <- table_sheet1$X2[112]
    upload["Observation_4",f]                             <- table_sheet1$X2[113]
    upload["Observation_5",f]                             <- table_sheet1$X2[114]
    upload["Observation_6",f]                             <- table_sheet1$X2[115]
    upload["Observation_7",f]                             <- table_sheet1$X2[116]
    upload["Observation_8",f]                             <- table_sheet1$X2[117]
    upload["Observation_9",f]                             <- table_sheet1$X2[118]
    upload["Observation_10",f]                            <- table_sheet1$X2[119]
    upload["Observation_11",f]                            <- table_sheet1$X2[120]
    upload["Observation_12",f]                            <- table_sheet1$X2[121]
    upload["Observation_13",f]                            <- table_sheet1$X2[122]
    upload["Observation_14",f]                            <- table_sheet1$X2[123]
    upload["Observation_15",f]                            <- table_sheet1$X2[124]
    upload["Observation_16",f]                            <- table_sheet1$X2[125]
    upload["Observation_17",f]                            <- table_sheet1$X2[126]
    upload["Observation_18",f]                            <- table_sheet1$X2[127]
    upload["Observation_19",f]                            <- table_sheet1$X2[128]
    upload["Observation_20",f]                            <- table_sheet1$X2[129]
    upload["Observation_21",f]                            <- table_sheet1$X2[130]
    upload["Observation_22",f]                            <- table_sheet1$X2[131]
    upload["Observation_23",f]                            <- table_sheet1$X2[132]
    upload["Observation_24",f]                            <- table_sheet1$X2[133]
    upload["Observation_25",f]                            <- table_sheet1$X2[134]
    upload["Observation_26",f]                            <- table_sheet1$X2[135]
    upload["Observation_27",f]                            <- table_sheet1$X2[136]
    upload["Observation_28",f]                            <- table_sheet1$X2[137]
    upload["Observation_29",f]                            <- table_sheet1$X2[138]
    upload["Observation_30",f]                            <- table_sheet1$X2[139]
    upload["Observation_31",f]                            <- table_sheet1$X2[140]
    upload["Observation_32",f]                            <- table_sheet1$X2[141]
    upload["Observation_33",f]                            <- table_sheet1$X2[142]
    upload["Observation_34",f]                            <- table_sheet1$X2[143]
    upload["Observation_35",f]                            <- table_sheet1$X2[144]
    upload["Observation_36",f]                            <- table_sheet1$X2[145]
    upload["Observation_37",f]                            <- table_sheet1$X2[146]
    upload["Observation_38",f]                            <- table_sheet1$X2[147]
    upload["Observation_39",f]                            <- table_sheet1$X2[148]
    upload["Observation_40",f]                            <- table_sheet1$X2[149]
    upload["Observation_41",f]                            <- table_sheet1$X2[150]
    upload["Observation_42",f]                            <- table_sheet1$X2[151]
    upload["Observation_43",f]                            <- table_sheet1$X2[152]

    #can make colour coded variables for the indicators as specified
    #ie
    #upload["Indicator_6_evaluation",f] <- ifelse(table_sheet1$X2[6]<5 ,"well",
                                                  # ifelse(table_sheet1$X2[6]>10,"poor",
                                                  #        ifelse(table_sheet1$X2[6]>=5 & table_sheet1$X2[6]<=10, "acceptable",
                                                  #               NA)))


  }

  ## output file

  # put rownames in a column
  upload <- base::cbind("Variable / Field Name" = rownames(upload),upload)
  # flip to wide
  upload <- base::as.data.frame(base::t(upload))
  # drop duplicate (because of rownames)
  upload <- upload[-1, ]
  # define path to output to
  filename <- base::paste(outputdirectory,"/",outputname,".csv", sep = "")
  # write file
  utils::write.csv(upload, file = filename, row.names = FALSE)
}
