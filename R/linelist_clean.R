#' Clean merged COVID-19 linelists from WHO AFRO
#'
#' @param inputfile path to file created by [covidmonitor::merge_linelist()] function
#'
#' @param outputdirectory path to folder where merged file is to be saved
#'
#' @param outputname character string to name merged file
#'
#' @param cleaningdict path to dictionary file containing cleaning rule definitions.
#' Default is a predefined within the package. For details of how to use your own
#' file see [README](https://github.com/R4IDSR/covidmonitor)
#'
#' @importFrom rio import export
#' @importFrom dplyr mutate across contains if_else filter select
#' @author Alice Carr, Alex Spina
#' @export

## TODO: delete this
# library(dplyr) # still have this here because across not supported in standard dplyr ?

# the inputs that i have been using
inputfile <- "inst/Merged_linelist_2021-01-17.xlsx"
# outputdirectory <- "inst/"
# outputname <- "Merged_linelist_"
cleaningdict <- "inst/extdata/cleaning_dictionary.xlsx"

clean_linelist <- function(inputfile,
                           outputdirectory = tempdir(),
                           outputname = "Cleaned_linelist_",
                           cleaningdict = system.file("extdata", "cleaning_dictionary.xlsx", package = "covidmonitor")
                           ) {

  #import cleaning dictionary
  clean_dict <- rio::import(
    cleaningdict,
    which = "all_clean",
    readxl = FALSE)

  ## select the columns for recoding yes and no
  no <- na.omit(dplyr::select(clean_dict, no))
  yes <- na.omit(dplyr::select(clean_dict, yes))

  #capital city
  capital_dict <- rio::import(
    cleaningdict,
    which = "capital",
    readxl = FALSE)

  #somelinelists are only of confimred cases so are missing the outcome variable but this should be made confirmed and lab result positive for these countries
  confirmed_dict <- rio::import(
    cleaningdict,
    which = "confirmed",
    readxl = FALSE)

  # import has for some reason lost the values in some columns (ageonsetdays), due to the read_excel that is used.
  # as the output from the merge file is a .xlsx we can specify readxl =FALSE so the read.xlsx function will be used on the import instead
  # the read.xlsx functions requires the dependancy openxlsx
  big_data <- rio::import(
    inputfile,
    readxl = FALSE)

  # initialised new df for cleaning
  big_data_clean <- big_data

  # read in dates
  big_data_clean <- dplyr::mutate(big_data_clean, across(contains("date"), as.Date, origin = "1899-12-30"))

  ## report date ##
  #use dplyr::if_else for date handleing
  #replace report date with lab result date if missing
  big_data_clean$report_date <- dplyr::if_else(is.na(big_data_clean$report_date), big_data_clean$lab_resdate, big_data_clean$report_date)
  #big_data_clean$report_date <- dplyr::if_else(big_data_clean$report_date < as.Date("2020-01-01") | big_data_clean$report_date > as.Date(Sys.Date()), as.Date(NA), big_data_clean$report_date)


  ## patinfo_ageonset##
  big_data_clean$patinfo_ageonset <- ifelse(grepl("month|M", big_data_clean$patinfo_ageonsetunit, ignore.case = T), as.numeric(big_data_clean$patinfo_ageonset) / 12, big_data_clean$patinfo_ageonset)
  big_data_clean$patinfo_ageonset <- ifelse(grepl("day", big_data_clean$patinfo_ageonsetunit, ignore.case = T), as.numeric(big_data_clean$patinfo_ageonset) / 365, big_data_clean$patinfo_ageonset)

  # remove written units
  # Months unit column created from linelist_merge fn
  big_data_clean$patinfo_ageonsetunit <- ifelse(grepl("month|M|year|day|^0$", big_data_clean$patinfo_ageonsetunit, ignore.case = T), NA, big_data_clean$patinfo_ageonsetunit)
  # column was suppose to be for ages under 12 months so convert these to years and leave the rest
  big_data_clean$patinfo_ageonsetunit <- as.numeric(big_data_clean$patinfo_ageonsetunit) / 12
  # Days unit column created from linelist_merge fn
  big_data_clean$patinfo_ageonsetunitdays <- ifelse(grepl("month|M|year|day|^0$", big_data_clean$patinfo_ageonsetunitdays, ignore.case = T), NA, big_data_clean$patinfo_ageonsetunitdays)
  # column was suppose to be for ages under 12 months so convert these to years and leave the rest
  big_data_clean$patinfo_ageonsetunitdays <- as.numeric(big_data_clean$patinfo_ageonsetunitdays) / 365

  # replace ages that are in the unit column if missing in the normal age column or 0 in the normal age column but has a valueinputed in the unit column
  big_data_clean$patinfo_ageonset <- ifelse(is.na(big_data_clean$patinfo_ageonset) | big_data_clean$patinfo_ageonset == 0 & !is.na(big_data_clean$patinfo_ageonsetunit), big_data_clean$patinfo_ageonsetunit, big_data_clean$patinfo_ageonset)
  big_data_clean$patinfo_ageonset <- ifelse(is.na(big_data_clean$patinfo_ageonset) | big_data_clean$patinfo_ageonset == 0 & !is.na(big_data_clean$patinfo_ageonsetunitdays), big_data_clean$patinfo_ageonsetunitdays, big_data_clean$patinfo_ageonset)

  # Return NA for ages that are clearly incorrect
  big_data_clean$patinfo_ageonset <- ifelse(big_data_clean$patinfo_ageonset > 120 | big_data_clean$patinfo_ageonset < 0 & !is.na(big_data_clean$patinfo_ageonset), NA, big_data_clean$patinfo_ageonset)

  # drop unit columns now irrelavent
  big_data_clean$patinfo_ageonsetunit <- NULL
  big_data_clean$patinfo_ageonsetunitdays <- NULL


  ## patinfo_sex##
  #regex clean
  big_data_clean <-dplyr::mutate(big_data_clean, across(c(patinfo_sex), gsub, pattern = "[0-9?]", replacement = NA, ignore.case = T, perl = T))
  #keep first letter (in french/eng/pt it will always be M or F)
  big_data_clean$patinfo_sex <- substr(big_data_clean$patinfo_sex, 1, 1)
  # capitalise
  big_data_clean$patinfo_sex <- toupper(big_data_clean$patinfo_sex)
  # if entered incorrect make equal to NA
  big_data_clean$patinfo_sex <- ifelse(!grepl("M|F", big_data_clean$patinfo_sex, ignore.case = T), NA, big_data_clean$patinfo_sex)


  ## pat_symptomatic##
  # ensure id variable is present as splitting up dataframe
  big_data_clean$id <- rownames(big_data_clean)
  # next steps requrie dataframe format
  big_data_clean <- data.frame(big_data_clean)
  # ensure original pat_symptomatic variable contains no numbers of special characters and only yes or no
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(pat_symptomatic), gsub, pattern = "[0-9?]", replacement = NA, ignore.case = T, perl = T))
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(pat_symptomatic), gsub, pattern = paste0("(?i)^", no$no, "$", collapse = "|"), replacement = "no", ignore.case = T, perl = T))
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(pat_symptomatic), gsub, pattern = paste0("(?i)^", yes$yes, "$", collapse = "|"), replacement = "yes", ignore.case = T, perl = T))

  big_data_clean$pat_symptomatic <- ifelse(!grepl("(?i)^no$|(?i)^yes$", big_data_clean$pat_symptomatic, ignore.case = T), NA, big_data_clean$pat_symptomatic)


  # Create a symptomatic column those missing variable using other symptoms variables

  #separate dataframe
  symptoms <- dplyr::select(big_data_clean, c(id, contains("sympt"), -contains(c("pat_symptomatic"))))
  unknown_sympt <- na.omit(dplyr::select(clean_dict,unknown_sympt))
  #symptom variables
  symptvars <- names(symptoms)

  # symptoms[,-1] means apply to everything except column 1 which is the id variable
  # remove any ? or numerics
  symptoms[, -1] <- data.frame(lapply(symptoms[, -1], function(x) {
    gsub("[0-9?]", NA, x, ignore.case = T, perl = T)
  }), stringsAsFactors = F)
  # change all variations of no to standardised
  symptoms[, -1] <- data.frame(lapply(symptoms[, -1], function(x) {
    gsub(paste0("(?i)^", no$no, "$", collapse = "|"), "no", x, ignore.case = T, perl = T)
  }), stringsAsFactors = F)
  # Change all variations of unknown symptoms to a blank (not na as this may lose information on the other symptoms column)
  symptoms[, -1] <- data.frame(lapply(symptoms[, -1], function(x) {
    gsub(paste0("(?i)", unknown_sympt$unknown_sympt, collapse = "|"), "", x, perl = T)
  }), stringsAsFactors = F)

  #regex clean
  # replace 1 or more spaces with one space
  symptoms[, -1] <- data.frame(lapply(symptoms[, -1], function(x) {
    gsub("\\s+", " ", x, ignore.case = T, perl = T)
  }), stringsAsFactors = F)
  # blank cells to NA
  symptoms[symptoms == " "] <- NA
  symptoms[symptoms == ""] <- NA


  # determine if patient had symptoms
  # sum row if contains no or is missing (if all rows are no of missing =7)
  symptoms$symptoms_none <- rowSums(symptoms[, -1] == "no" | is.na(symptoms[, -1]))
  # sum row if it is missing (if all ros missing =7)
  symptoms$symptoms_na <- rowSums(is.na(symptoms[, -1]))
  # initialised symptomatic variable
  symptoms$pat_symptomatic <- NA

  # if row sums in symptoms_none=7 then all rows had no or missing in symptoms
  symptoms$pat_symptomatic <- ifelse(symptoms$symptoms_none == length(symptvars)-1, "no", symptoms$pat_symptomatic)
  # if row sums in symptoms_none<7 then at least one row had yes or other symptoms
  symptoms$pat_symptomatic <- ifelse(symptoms$symptoms_none < length(symptvars)-1, "yes", symptoms$pat_symptomatic)
  # replace rows that had all missing in all symptoms
  symptoms$pat_symptomatic <- ifelse(symptoms$symptoms_na == length(symptvars)-1, NA, symptoms$pat_symptomatic)

  # add  back columns of interest in  merging
  symptoms_clean <- dplyr::select(symptoms, c(id,pat_symptomatic))

  # merge with big data to infill symptomatic yes or no, only if missing this variable
  big_data_clean <- merge(big_data_clean, symptoms_clean, by = "id")
  big_data_clean$pat_symptomatic.x <- ifelse(is.na(big_data_clean$pat_symptomatic.x), big_data_clean$pat_symptomatic.y, big_data_clean$pat_symptomatic.x) #.x was the original variable


  # final clean up of pat_symptomatic
  big_data_clean$pat_symptomatic <- big_data_clean$pat_symptomatic.x
  #big_data_clean$pat_asymptomatic <- ifelse(big_data_clean$pat_asymptomatic == 1, 0, big_data_clean$pat_asymptomatic)
  big_data_clean$pat_symptomatic <- ifelse(is.na(big_data_clean$pat_symptomatic) & !is.na(big_data_clean$pat_asymptomatic) & big_data_clean$pat_asymptomatic == 1, "no", big_data_clean$pat_symptomatic)

  big_data_clean$pat_symptomatic.x <- NULL
  big_data_clean$pat_symptomatic.y <- NULL
  big_data_clean$pat_asymptomatic <- NULL

  # debug to check
  #sympt_test<- big_data_clean %>% select(id,contains("sympt"))

  # remove now unneccessary specific symptoms variables but leave symptomatic variable (included removing the asymptomatic variable)
  big_data_clean <- big_data_clean[, -which(names(big_data_clean) %in% symptvars)]


  ## patinfo_occus ##
  # list of words/ strings associated with healthcare/ non occupations that should be cleaned up
  occupation <- na.omit(dplyr::select(clean_dict,patinfo_occus))
  not_occupation <- na.omit(dplyr::select(clean_dict,not_occupation))

  #regex cleaning
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(patinfo_occus), gsub, pattern = "['?]|[[:punct:]]", replacement = "", ignore.case = T, perl = T))
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(patinfo_occus), gsub, pattern = "[0-9]", replacement = "", ignore.case = T, perl = T))
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(patinfo_occus), gsub, pattern = "^[a-zA-Z]$", replacement = "", ignore.case = T, perl = T))
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(patinfo_occus), gsub, pattern = "\r\n", replacement = "", fixed = T))
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(patinfo_occus), gsub, pattern = "\\s+", replacement = " ", ignore.case = T))
  big_data_clean <- dplyr::mutate(big_data_clean, across(c(patinfo_occus), gsub, pattern = "\\s+$", replacement = "", ignore.case = T))

  # cleaning of "non occupations"
  big_data_clean$patinfo_occus <- ifelse(big_data_clean$patinfo_occus == "", NA, big_data_clean$patinfo_occus)
  #case insentive exact string match remove these strings from occupation column
  big_data_clean$patinfo_occus <- ifelse(grepl(paste0("(?i)^", not_occupation$not_occupation, "$", collapse = "|"), big_data_clean$patinfo_occus, ignore.case = T), NA, big_data_clean$patinfo_occus)

  # Create a column that creates TRUE for health care worker after a match or partial match with list og HCW associated strings
  big_data_clean$hcw <- grepl(paste(occupation$patinfo_occus, collapse = "|"), big_data_clean$patinfo_occus, ignore.case = T)
  # replce hcw with NA if occupation was missing
  big_data_clean$hcw <- ifelse(is.na(big_data_clean$patinfo_occus), NA, big_data_clean$hcw)  # use this column for hcw identification in analysis



  ## patcourse_status ##
  # list of words/ strings associated with:
  #dead
  dead <- na.omit(dplyr::select(clean_dict,dead))
  #alive
  alive <- na.omit(dplyr::select(clean_dict,alive))
  #recovered
  recovered <- na.omit(dplyr::select(clean_dict,recovered))

  # if missing patcourse_status make equal to patcurrent status
  big_data_clean$patcourse_status <- ifelse(is.na(big_data_clean$patcourse_status), big_data_clean$patcurrent_status, big_data_clean$patcourse_status)

  # regex cleaning
  big_data_clean<- dplyr::mutate(big_data_clean, across(c(patcourse_status), gsub, pattern = "[0-9?]", replacement = NA, ignore.case = T, perl = T))

  big_data_clean$patcourse_status_dead <-ifelse(grepl(paste0("(?i)", dead$dead, collapse = "|"), big_data_clean$patcourse_status, ignore.case = T), "dead", NA)
  big_data_clean$patcourse_status_alive <-ifelse(grepl(paste0("(?i)", alive$alive, collapse = "|"), big_data_clean$patcourse_status, ignore.case = T), "alive", NA)
  big_data_clean$patcourse_status_recovered <-ifelse(grepl(paste0("(?i)", recovered$recovered, collapse = "|"), big_data_clean$patcourse_status, ignore.case = T), "recovered", NA)

  #fill in status column with above clean
  big_data_clean$patcourse_status <- ifelse(!is.na(big_data_clean$patcourse_status) & !is.na(big_data_clean$patcourse_status_dead), big_data_clean$patcourse_status_dead, big_data_clean$patcourse_status)
  big_data_clean$patcourse_status <- ifelse(!is.na(big_data_clean$patcourse_status) & !is.na(big_data_clean$patcourse_status_alive), big_data_clean$patcourse_status_alive, big_data_clean$patcourse_status)

  big_data_clean$patcourse_status <- ifelse(!grepl("alive|dead", big_data_clean$patcourse_status, ignore.case = T), NA, big_data_clean$patcourse_status)
  big_data_clean$patcourse_status_recovered <- ifelse(is.na(big_data_clean$patcourse_status_recovered), big_data_clean$patcourse_status, big_data_clean$patcourse_status_recovered)

  # drop columns created, uncommment if needed for debugging
  big_data_clean$patcourse_status_dead <- NULL
  big_data_clean$patcourse_status_alive <- NULL


  # some cases with a date of death but not "dead" in their status column?
  # Date of death handling.
  # some countries used a date of outcome column - nonspecific to dead or discharged, this needs cleaning if patient is not dead then empy the date of death column and put date in the discharge column
  big_data_clean$patcourse_datedischarge <- ifelse(big_data_clean$patcourse_status != "dead", big_data_clean$patcourse_datedeath, big_data_clean$patcourse_datedischarge)
  big_data_clean$patcourse_datedeath <- ifelse(big_data_clean$patcourse_status != "dead",as.Date(NA), big_data_clean$patcourse_datedeath)
  big_data_clean$patcourse_datedischarge <- ifelse(big_data_clean$patcourse_status == "dead", as.Date(NA), big_data_clean$patcourse_datedischarge)
  # date format changes due to the additions of NAs at this point in these two columns so change to readble date classes
  big_data_clean<-dplyr::mutate(big_data_clean, across(contains(c("datedeath","datedischarge")), as.Date,origin="1970-01-01"))


  ## report classif ##
  # list of words/ strings associated with:
  #probable case
  probable <- na.omit(dplyr::select(clean_dict,probable))
  #suspected case
  suspected <- na.omit(dplyr::select(clean_dict,suspected))
  #confirmed case
  confirmed <- na.omit(dplyr::select(clean_dict,confirmed))
  #not a case
  notacase <- na.omit(dplyr::select(clean_dict,notacase))

  # regex cleaning
  big_data_clean<- dplyr::mutate(big_data_clean, across(c(report_classif), gsub, pattern = "[0-9?]", replacement = NA, ignore.case = T, perl = T))
  # probable
  big_data_clean$report_classif_probable <-ifelse(grepl(paste0("(?i)", probable$probable, collapse = "|"), big_data_clean$report_classif, ignore.case = T), "probable", NA)
  # suspected
  big_data_clean$report_classif_suspected <-ifelse(grepl(paste0("(?i)", suspected$suspected, collapse = "|"), big_data_clean$report_classif, ignore.case = T), "suspected", NA)
  # confirmed
  big_data_clean$report_classif_confirmed <-ifelse(grepl(paste0("(?i)", confirmed$confirmed, collapse = "|"), c(big_data_clean$report_classif), ignore.case = T), "confirmed", NA)
  big_data_clean$report_classif_confirmed <-ifelse(grepl(paste0("(?i)", confirmed$confirmed, collapse = "|"), c(big_data_clean$lab_result), ignore.case = T), "confirmed", big_data_clean$report_classif_confirmed)
  # not a case
  big_data_clean$report_classif_nac <-ifelse(grepl(paste0("(?i)", notacase$notacase, collapse = "|"), big_data_clean$report_classif, ignore.case = T), "not a case", NA)

  #in fill variable with above clean
  big_data_clean$report_classif <- ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_probable), big_data_clean$report_classif_probable, big_data_clean$report_classif)
  big_data_clean$report_classif <- ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_suspected), big_data_clean$report_classif_suspected, big_data_clean$report_classif)
  big_data_clean$report_classif <- ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_confirmed), big_data_clean$report_classif_confirmed, big_data_clean$report_classif)
  big_data_clean$report_classif <- ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_nac), big_data_clean$report_classif_nac, big_data_clean$report_classif)

  # report_test<- big_data_clean %>% select(patinfo_id,contains("report"))

  # drop columns created, uncommment if needed for debugging
  big_data_clean$report_classif_probable <- NULL
  big_data_clean$report_classif_suspected <- NULL
  big_data_clean$report_classif_confirmed <- NULL
  big_data_clean$report_classif_nac <- NULL

  big_data_clean$report_classif <- ifelse(!grepl("suspected|probabale|confirmed|not a case", big_data_clean$report_classif, ignore.case = T), NA, big_data_clean$report_classif)


  # labresult
  positive <- na.omit(dplyr::select(clean_dict,positive))
  negative <- na.omit(dplyr::select(clean_dict,negative))
  inconclusive <- na.omit(dplyr::select(clean_dict,inconclusive))

  # regex cleaning
  big_data_clean<-dplyr::mutate(big_data_clean, across(c("lab_result"), iconv, from = "UTF-8", to = "ASCII//TRANSLIT"))
  big_data_clean<- dplyr::mutate(big_data_clean, across(c(lab_result), gsub, pattern = "[0-9?]", replacement = NA, ignore.case = T, perl = T))

  # positive
  big_data_clean$lab_result_pos <-ifelse(grepl(paste0("(?i)", positive$positive, collapse = "|"), big_data_clean$lab_result, ignore.case = T), "positive", NA)
  # negative
  big_data_clean$lab_result_neg <-ifelse(grepl(paste0("(?i)", negative$negative, collapse = "|"), big_data_clean$lab_result, ignore.case = T), "negative", NA)
  # inconclusive
  big_data_clean$lab_result_incon <-ifelse(grepl(paste0("(?i)", inconclusive$inconclusive, collapse = "|"), big_data_clean$lab_result, ignore.case = T), "inconclusive", NA)

  #in fill variable with above clean
  big_data_clean$lab_result <- ifelse(!is.na(big_data_clean$lab_result) & !is.na(big_data_clean$lab_result_pos), big_data_clean$lab_result_pos, big_data_clean$lab_result)
  big_data_clean$lab_result <- ifelse(!is.na(big_data_clean$lab_result) & !is.na(big_data_clean$lab_result_neg), big_data_clean$lab_result_neg, big_data_clean$lab_result)
  big_data_clean$lab_result <- ifelse(!is.na(big_data_clean$lab_result) & !is.na(big_data_clean$lab_result_incon), big_data_clean$lab_result_incon, big_data_clean$lab_result)

  # drop columns created, uncommment if needed for debugging
  big_data_clean$lab_result_pos <- NULL
  big_data_clean$lab_result_neg <- NULL
  big_data_clean$lab_result_incon <- NULL


  # using lab result to confirm case
  # create report_classif_final variable for extra cleaning
  #new variable created by looking for confirmed instances in the lab result variable - cleaning is above
  big_data_clean$report_classif_final <- big_data_clean$report_classif
  big_data_clean$report_classif_final <- ifelse(big_data_clean$lab_result == "positive" & !is.na(big_data_clean$lab_result), "confirmed", big_data_clean$report_classif)


  # load in dictionary for linelists that are only the positves / confirmed cases as pre specified
  linelist_pos <- confirmed_dict
  linelist_pos <- filter(linelist_pos, !is.na(linelist_pos$labresult))

  big_data_clean$report_classif_final <- ifelse(is.na(big_data_clean$report_classif_final), linelist_pos$classification[match(big_data_clean$country_iso, linelist_pos$country_iso)], big_data_clean$report_classif_final)
  big_data_clean$lab_result <- ifelse(is.na(big_data_clean$lab_result), linelist_pos$labresult[match(big_data_clean$country_iso, linelist_pos$country_iso)], big_data_clean$lab_result)


  ## comorbidies ##
  big_data_comorbs <- dplyr::select(big_data_clean, contains("comcond"))
  big_data_comorbs$id <- rownames(big_data_comorbs)
  # partial string matches using above spreadsheet dictionary for each ncd, checking in both comcond columns
  #curretnly this takes a while to run, other options dont seem to work but this part could be developed for a quicker run

  big_data_comorbs$diabetes <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$diabetes), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$asthma <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$asthma), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$hypertension <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$hypertension), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$obesity <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$obesity), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$cardiovascular_disease <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$cardiovascular.disease), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$pregnancy <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$Pregnancy), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$renal_disease <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$Renal.disease), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$drepanocytosis <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$Drepanocytosis), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$chronic_pulmonary <- apply(big_data_comorbs, 1, function(x) any(grepl(paste0(na.omit(clean_dict$Chronic.pulmonary.disease), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$cancer <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$Cancer), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$tuberculosis <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$Tuberculosis), collapse = "|"), x, ignore.case = T)))
  big_data_comorbs$other_comorb <- apply(big_data_comorbs, 1, function(x) any(grepl(paste(na.omit(clean_dict$Other), collapse = "|"), x, ignore.case = T)))

  # changes all TRUE to 1 and FALSE to NA ftom grepl
  big_data_comorbs <- data.frame(lapply(big_data_comorbs, function(x) {
    gsub("TRUE", 1, x, ignore.case = T, perl = T)
  }), stringsAsFactors = F)
  big_data_comorbs <- data.frame(lapply(big_data_comorbs, function(x) {
    gsub("FALSE", NA, x, ignore.case = T, perl = T)
  }), stringsAsFactors = F)
  # changes variations of yes and no to standard
  big_data_comorbs <- data.frame(lapply(big_data_comorbs, function(x) {
    gsub(paste0("(?i)^", no$no, "$", collapse = "|"), "no", x, ignore.case = T, perl = T)
  }), stringsAsFactors = F)
  big_data_comorbs <- data.frame(lapply(big_data_comorbs, function(x) {
    gsub(paste0("(?i)^", yes$yes, "$", collapse = "|"), "yes", x, ignore.case = T, perl = T)
  }), stringsAsFactors = F)


  # removed anything entered in here that is not yes or no or not given
  big_data_comorbs$comcond_preexist1 <- ifelse(grepl("yes|no", big_data_comorbs$comcond_preexist1, ignore.case = T), big_data_comorbs$comcond_preexist1, NA)
  big_data_comorbs$comcond_preexist1 <- ifelse(grepl("yes", big_data_comorbs$comcond_preexist1, ignore.case = T), "yes", big_data_comorbs$comcond_preexist1)
  big_data_comorbs$comcond_preexist1 <- ifelse(grepl("(?i)^no$", big_data_comorbs$comcond_preexist1, perl = T), "no", big_data_comorbs$comcond_preexist1)
  big_data_comorbs$comcond_preexist1 <- ifelse(is.na(big_data_comorbs$comcond_preexist1) & grepl("yes|(?i)^no$", big_data_comorbs$comcond_preexist, ignore.case = T, perl = T), big_data_comorbs$comcond_preexist, big_data_comorbs$comcond_preexist1)


  # If RowSums=0 then there are no ncd specified
  big_data_comorbs <- dplyr::mutate(big_data_comorbs, across(c(-comcond_preexist1, -comcond_preexist), as.numeric))
  big_data_comorbs$comcond_preexsist_yesno <- rowSums(dplyr::select(
    big_data_comorbs, diabetes, asthma, hypertension, cancer,
    renal_disease, cardiovascular_disease, obesity, tuberculosis, drepanocytosis, chronic_pulmonary, other_comorb
  ), na.rm = T)

  big_data_comorbs$not_specified_comorb <- ifelse(big_data_comorbs$comcond_preexist1 == "yes" & is.na(big_data_comorbs$comcond_preexist) & big_data_comorbs$comcond_preexsist_yesno == 0, 1, NA)
  big_data_comorbs$not_specified_comorb <- ifelse(big_data_comorbs$comcond_preexist1 == "yes" & is.na(big_data_comorbs$not_specified_comorb) & grepl(paste(na.omit(clean_dict$`Not Specified`), collapse = "|"), big_data_comorbs$comcond_preexist, ignore.case = T), 1, big_data_comorbs$not_specified_comorb)

  big_data_comorbs$comcond_preexsist_yesno <- NULL # removing this placeholder variable to redo below including not specified

  big_data_comorbs$comcond_preexsist_count <- rowSums(dplyr::select(big_data_comorbs, c(
    diabetes, asthma, hypertension, cancer,
    renal_disease, cardiovascular_disease, obesity, tuberculosis, drepanocytosis, chronic_pulmonary, not_specified_comorb, other_comorb
  )), na.rm = T)

  # variable correction for yes/no ncd based on previous dictionary
  # if no ncd were picked up then the yes/no variable is changed to no
  # if ncd was picked up then yes/no variable is changed to yes
  # if missing yes/no variable it is changed to not given
  #  comcond_preexsist_final is created as a comparison variable for above criteria
  big_data_comorbs$comcond_preexist1_final <- ifelse(big_data_comorbs$comcond_preexsist_count == 0 & is.na(big_data_comorbs$comcond_preexist) & is.na(big_data_comorbs$comcond_preexist1), "not given", NA)

  big_data_comorbs$comcond_preexist1_final <- ifelse(big_data_comorbs$comcond_preexsist_count > 0 & is.na(big_data_comorbs$comcond_preexist1_final), "yes", big_data_comorbs$comcond_preexist1_final)
  big_data_comorbs$comcond_preexist1_final <- ifelse(big_data_comorbs$comcond_preexsist_count == 0 & is.na(big_data_comorbs$comcond_preexist1_final) & big_data_comorbs$comcond_preexist1 == "no", "no", big_data_comorbs$comcond_preexist1_final)
  # if no comorbs identified from string matching and if they had in column yes change this to a no because it is not a comorb
  big_data_comorbs$comcond_preexist1_final <- ifelse(big_data_comorbs$comcond_preexsist_count == 0 & is.na(big_data_comorbs$comcond_preexist1_final) & big_data_comorbs$comcond_preexist1 == "yes", "no", big_data_comorbs$comcond_preexist1_final)
  # for those with missing yes/no byt entered in specify variable but it is not a comorbidity as identified from the previous string mtching
  big_data_comorbs$comcond_preexist1_final <- ifelse(big_data_comorbs$comcond_preexsist_count == 0 & is.na(big_data_comorbs$comcond_preexist1_final) & is.na(big_data_comorbs$comcond_preexist1) & !is.na(big_data_comorbs$comcond_preexist), "no", big_data_comorbs$comcond_preexist1_final)


  ###
  # generate row id to ensure correct back merge
  big_data_clean$id <- rownames(big_data_clean)
  # removed old comcond variables
  big_data_clean <- dplyr::select(big_data_clean, -c(contains("comcond")))
  # change comborbs if to character for merge
  big_data_comorbs$id <- as.character(big_data_comorbs$id)
  big_data_clean <- merge(big_data_clean, big_data_comorbs, by = "id")



  ### capital city
  country <- capital_dict
  # varible of country full name from aboove dictionary
  big_data_clean$country_full <- country$country_full[match(big_data_clean$country_iso, country$country_iso)]
  # add column that is the capital city
  big_data_clean$capital <- country$capital[match(big_data_clean$country_iso, country$country_iso)]
  # partial match readmin1 variable to identify if patient is in capital city
  big_data_clean$capital_final <- mapply(function(x, y) grepl(x, y, ignore.case = T), big_data_clean$capital, big_data_clean$patinfo_resadmin1)
  big_data_clean$capital <- NULL # drop variable that was used for matching



  # re-run this line to replace any report dates that were dropped because entered incorrectly we can use the labdate as a surrogate at the end of this clean,
  big_data_clean$report_date <- dplyr::if_else(is.na(big_data_clean$report_date), big_data_clean$lab_resdate, big_data_clean$report_date)
  #big_data_clean$report_date <- dplyr::if_else(big_data_clean$report_date < as.Date("2020-01-01") | big_data_clean$report_date > as.Date(Sys.Date()), as.Date(NA), big_data_clean$report_date)

  # define path to output to
  filename <- paste0(outputdirectory,"/", outputname, Sys.Date(), ".xlsx")

  # write file
  rio::export(big_data_clean, file = filename)

  # return merged dataset
  big_data_clean

  confirmedcases<-select(big_data_clean,c(patinfo_id,
                                          report_date,
                                          country_full,
                                          country_iso,
                                          patinfo_ageonset,
                                          patinfo_sex,
                                          patinfo_resadmin1,
                                          patinfo_resadmin2,
                                          report_classif,
                                          lab_result,
                                          patcourse_status,
                                          patcourse_datedeath,
                                          patcourse_datedischarge,
                                          pat_symptomatic,
                                          hcw,
                                          patcourse_status_recovered,
                                          report_classif_final,
                                          comcond_preexist1,
                                          comcond_preexist1_final))
  ## in readme it is described that some countires ie. Chad and Senegal (TCD) has date issue due to the excel file used.
  ## If the fix isnt done prior to merging uncommenting these lines would do a rough fix
  ## see readme for details

  # confirmedcases<-dplyr::mutate(confirmedcases, across(c("report_date", "patcourse_datedeath","patcourse_datedischarge"), ~ ifelse(country_iso=="TCD" |country_iso=="SEN",  as.Date(.,origin= "1970-01-01") + lubridate::years(4), as.Date(.,origin= "1970-01-01"))))
  # confirmedcases<-dplyr::mutate(confirmedcases, across(c("report_date","patcourse_datedeath","patcourse_datedischarge"), as.Date, origin= "1970-01-01"))
  # confirmedcases <- dplyr::mutate(confirmedcases, across(report_date, ~if_else(. < as.Date("2020-01-01") | . > as.Date(Sys.Date()), as.Date(NA), .)))


  #filter for only confimered cases in this file
  confirmedcases<-filter(confirmedcases,report_classif_final=="confirmed"|report_classif_final=="suspected")

  #filter for not missing report date
  #confirmedcases<-filter(confirmedcases,!is.na(report_date))

  #change variables/variable names to be in line with previous ConfirmedCases files for template scripts that have been previously developed based on ConfirmedCases files
  confirmedcases<-rename(confirmedcases,c("id"=patinfo_id,
                                          "reporting_date"=report_date,
                                          "country"=country_full,
                                          "country_iso3"=country_iso,
                                          "age"=patinfo_ageonset,
                                          "sex"= patinfo_sex,
                                          "province"=patinfo_resadmin1,
                                          "district"=patinfo_resadmin2,
                                          "epi_classification"=report_classif,
                                          "outcome"=patcourse_status,
                                          "date_of_death" = patcourse_datedeath,
                                          "date_of_discharge"=patcourse_datedischarge,
                                          "symptomatic"=pat_symptomatic,
                                          "healthcare_worker"=hcw,
                                          "finaloutcome"=patcourse_status_recovered,
                                          "finalepiclassification"=report_classif_final,
                                          "preexsiting_comorbidity"=comcond_preexist1,
                                          "preexsiting_comorbidity_final"=comcond_preexist1_final
                                          ))

  confirmedcases<-mutate(confirmedcases,across(healthcare_worker,
                         ~case_when(.==TRUE~"yes",
                                    .==FALSE~"no")))

  confirmedcases<-mutate(confirmedcases,across(finalepiclassification,
                         ~case_when(.=="suspected"~"probable",
                                    .!="suspected" ~ . )))

  # write ConfirmedCases file
  rio::export(confirmedcases, file = paste0("ConfirmedCases_",Sys.Date(),".csv"))

}
