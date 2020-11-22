#' Merge linelists from AFRO
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
#' in wide format (default), else will produce long format data
#'
#' @importFrom rio import export
#' @importFrom tidyr fill pivot_wider
#' @importFrom matchmaker match_vec
#' @importFrom dplyr bind_rows
#' @importFrom janitor
#' @author Alice Carr, Alex Spina
#' @export

merge_linelist<- function(inputdirectory,
                          outputdirectory = tempdir(),
                          outputname = "filename",
                          wide = TRUE) {
  inputdirectory="inst/extdata/tamayi linelists/"
  files <- base::list.files(path = inputdirectory, full.names = TRUE)

  # create folder for output
  base::dir.create(outputdirectory, showWarnings = FALSE)

require(dplyr)

  # read in dictionary for renaming variables contry specific sheet
  var_dict <- rio::import(
    here::here("inst","extdata", "linelist_dictionary.xlsx"),
    which = 1
  ) %>% as_tibble() %>%
    linelist::clean_variable_names()

  # read in dictionary for checking comments empty or not
  var_standards <- rio::import(
    here::here("inst","extdata", "linelist_dictionary.xlsx"),
    which = 2) %>% as_tibble() %>%
      linelist::clean_variable_names()


  #read in sheet checking if template is true
  template_check <- rio::import(
    here::here("inst","extdata", "linelist_dictionary.xlsx"),
    which = 3) %>% as_tibble() %>%
      linelist::clean_variable_names()



  # create an empty list to fill in with datasets
  output <- list()
  ulst<-list()

  # for each file listed
  for (f in 1:10){
  #for (f in 1:base::length(files)) {
    #isocode of country file
    iso<-substr(tools::file_path_sans_ext(basename(files[f])), 0, 3)
    print(iso)
    #for this iso code set parameters dictating file load in and clean process
    sheetname<-template_check$sheetname[template_check$country==iso]
    skip<-template_check$skip[template_check$country==iso]
    template<-template_check$template[template_check$country==iso]

    # if file requires no cleaning as matched WHO standard template do this loop
    if(template==TRUE){
     # load in file
     og_sheet <- rio::import(files[f], which=sheetname, skip=skip)
     # filter variable cleaning dictionary specific to country file loaded
     var_dict_country<- var_dict %>% filter(country==iso)
     # list variables to drop that are non-template
     drop_nontemplate_vars<- as.list(var_dict_country$old_var_r[var_dict_country$notes=="non_template" & !is.na(var_dict_country$notes)])
     if(length(drop_nontemplate_vars)!=0){
     # drop variables from list which are non-tmeplate
     og_sheet<-og_sheet[ , -which(names(og_sheet) %in% drop_nontemplate_vars)]
     }

     require(anytime)
     dates<-og_sheet %>% select(contains("Date", ignore.case = T)) %>% mutate_if(is.character, as.numeric) %>% mutate_if(is.numeric,as.Date, origin="1899-12-30")
     date_vars<-names(dates)
     og_sheet<-og_sheet[ , -which(names(og_sheet) %in% date_vars)]

     og_sheet<- cbind(og_sheet,dates)

     # make column for ISO code
    og_sheet$country_iso<-iso

    output_sheet<-og_sheet

    }else if(template==FALSE){
      #load in file
      og_sheet <- rio::import(files[f], which=sheetname, skip=skip)
      # filter variable cleaning dictionary specific to country file loaded
      var_dict_country<- var_dict %>% filter(country==iso)

      # list variables to drop that are non-template
      drop_nontemplate_vars<- as.list(var_dict_country$old_var_r[var_dict_country$notes=="non_template" & !is.na(var_dict_country$notes)])
      # drop variables from list which are non-tmeplate
      og_sheet<-og_sheet[ , -which(names(og_sheet) %in% drop_nontemplate_vars)]

      # recode variables

      recode_vars<-as.list(var_dict_country$old_var_r[grepl("Recode",var_dict_country$notes) & !is.na(var_dict_country$notes)])
      if(length(recode_vars)!=0){
      variable_to<- sub("Recode to ", "",as.list(var_dict_country$notes[grepl("Recode",var_dict_country$notes) & !is.na(var_dict_country$notes)]))
      what_to_match<-unique(variable_to)

      recode_sheet<- og_sheet[ , which(names(og_sheet) %in% recode_vars)]
      names(recode_sheet)<- gsub(x = names(recode_sheet), pattern = "COVID |Co19 ", replacement = "", ignore.case = T)

      recode_sheet<- data.frame(lapply(recode_sheet, function(x) {gsub("NAO|NON|NO", NA, x, ignore.case = T) }), stringsAsFactors=F)
      recode_sheet <- data.frame(lapply(recode_sheet, function(x) {gsub("SIM|OUI|YES", 1, x, ignore.case = T) }), stringsAsFactors=F)
      recode_sheet <-  data.frame(sapply(names(recode_sheet), function(x)ifelse(recode_sheet[,x] == 1, x, recode_sheet[,x])), stringsAsFactors=F)

      names(recode_sheet)<-paste(names(recode_sheet),variable_to, sep= ".")

      require(data.table)
      recode_sheet<-data.table(recode_sheet)
      recode_sheet[, paste(what_to_match,"", sep = "") :=
                 lapply(what_to_match, function(wtm) Reduce(function(x,y) paste(x, y, sep = ","),
                                                          .SD[, grep(wtm, names(recode_sheet)), with = F]))]

      recode_sheet <- data.frame(lapply(recode_sheet, function(x) {gsub(".", " ", x, fixed = T) }), stringsAsFactors=F)

      recode_sheet<-data.frame(lapply(recode_sheet, function(x) {gsub("NA,", "", x) }), stringsAsFactors=F)
      recode_sheet<-data.frame(lapply(recode_sheet, function(x) {gsub(",NA", "", x) }), stringsAsFactors=F)
      recode_sheet<- recode_sheet %>% naniar::replace_with_na_all(condition = ~.x == "NA")
      recode_sheet<- recode_sheet[ , which(names(recode_sheet) %in% what_to_match)]

      output_sheet<-og_sheet[ , -which(names(og_sheet) %in% recode_vars)]
      names(output_sheet) <- gsub(x= names(output_sheet),pattern ="\r\n",replacement = "", fixed = T)
      names(output_sheet) <- with(var_dict_country,new_var[match(names(output_sheet),old_var_r)])
      output_sheet<-cbind(output_sheet,recode_sheet)
      }else{
        output_sheet<- og_sheet
        names(output_sheet) <- gsub(x= names(output_sheet),pattern ="\r\n",replacement = "", fixed = T)
        names(output_sheet) <- with(var_dict_country,new_var[match(names(output_sheet),old_var_r)])
      }
      #handle all date variables
      require(anytime)
      dates<-output_sheet %>% select(contains("Date", ignore.case = T)) %>% mutate_if(is.character, as.numeric) %>% mutate_if(is.numeric,as.Date, origin="1899-12-30")
      date_vars<-names(dates)
      output_sheet<-output_sheet[ , -which(names(output_sheet) %in% date_vars)]

      output_sheet<- cbind(output_sheet,dates)

      #remove rows where all columns except ID is missing
      na_rows = output_sheet %>%
        select(-patinfo_ID) %>%
        is.na() %>%
        rowSums() > 0

      output_sheet<-  output_sheet %>%
        filter(!na_rows)

      # make column for ISO code
      output_sheet$country_iso<-iso


    }else if(template=="SKIP"){
      print(base::paste(files[f],"country file no longer in use"))
      next
    }

    #checking that variables of interest are not missing if so maipulate outputsheet so it isnt
    output_sheet$report_date<-output_sheet$report_date
    if(all(is.na(output_sheet$report_date))){
      print("report_date")
    }
    output_sheet$patinfo_ageonset<-as.numeric(iconv(output_sheet$patinfo_ageonset, 'utf-8', 'ascii', sub=''))
    if(all(is.na(output_sheet$patinfo_ageonset))){
      print("patinfo_ageonset")
    }
    output_sheet$patinfo_sex<- substr(output_sheet$patinfo_sex,1,1)
    if(all(is.na(output_sheet$patinfo_sex))){
      print("patinfo_sex")
    }

    output_sheet$patinfo_resadmin1<-output_sheet$patinfo_resadmin1
      if(all(is.na(output_sheet$patinfo_resadmin1))){
        print("patinfo_resadmin1")
      }

    output_sheet$patinfo_resadmin2<-output_sheet$patinfo_resadmin2
    if(all(is.na(output_sheet$patinfo_resadmin2))){
      print("patinfo_resadmin2")
    }


    output_sheet$report_classif<-output_sheet$report_classif
    if(all(is.na(output_sheet$report_classif))){
      print("report_classif")
    }

    output_sheet$sympt_presence<-output_sheet$sympt_presence
      if(all(is.na(output_sheet$sympt_presence))){
        print("sympt_presence")
      }

    output_sheet$comcond_preexist1<-output_sheet$comcond_preexist1
    if(all(is.na(output_sheet$comcond_preexist1))){
      print("comcond_preexist1")
    }

    output_sheet$comcond_preexist<-output_sheet$comcond_preexist
    if(all(is.na(output_sheet$comcond_preexist))){
      print("comcond_preexist")
    }
    #these needs recoding to just healthcareworker or not
    output_sheet$patinfo_occus<-output_sheet$patinfo_occus
    if(all(is.na(output_sheet$patinfo_occus))){
      print("patinfo_occus")
    }
    output_sheet$expo_travel<-output_sheet$expo_travel
    if(all(is.na(output_sheet$expo_travel))){
      print("expo_travel")
    }
    output_sheet$expo_travel_country<- output_sheet$expo_travel_country
    if(all(is.na(output_sheet$expo_travel_country))){
      print("expo_travel_country")
    }
    output_sheet$expo_contact_case<-output_sheet$expo_contact_case
    if(all(is.na(output_sheet$expo_contact_case))){
      print("expo_contact_case")
    }
    output_sheet$lab_datetaken<-output_sheet$lab_datetaken
    if(all(is.na(output_sheet$lab_datetaken))){
      print("lab_datetaken")
    }
    output_sheet$lab_result<-output_sheet$lab_result
    if(all(is.na(output_sheet$lab_result))){
      print("lab_result")
    }
    output_sheet$lab_resdate<-output_sheet$lab_resdate
    if(all(is.na(output_sheet$lab_resdate))){
      print("lab_resdate")
    }
    output_sheet$patcourse_status<-output_sheet$patcourse_status
    if(all(is.na(output_sheet$patcourse_status))){
      print("patcourse_status")
    }
    #ensure this is for death, some files ie. KEM had date of outcome column whihc was used in this place
    #some cases who were not dead would have this entred into their date of death
    #make date of death missing if patcourse_status !=dead
    patcourse_datedeath<-
    if(all(is.na(output_sheet$patinfo_resadmin2))){
      print("patinfo_resadmin2")
    }
    country_iso
    if(all(is.na(output_sheet$patinfo_resadmin2))){
      print("patinfo_resadmin2")
    }


    #keep variables of interet
    output_sheet<- output_sheet %>% dplyr::select(report_date, patinfo_ageonset, patinfo_sex, patinfo_resadmin1, patinfo_resadmin2, report_classif,sympt_presence,  comcond_preexist1, comcond_preexist, patinfo_occus, expo_travel, expo_travel_country, expo_contact_case, lab_result, lab_datetaken, lab_resdate, patcourse_status, patcourse_datedeath, country_iso)

    #add cleaned output sheet to a list
    output[[f]] <-output_sheet
    #the unique values in each column
    ulst[[f]] <- lapply(output_sheet, unique)
  }
  big_data <- do.call(bind_rows, output)
  #find all possible values entries fro each of teh above variables for recoding


#
}

