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
    linelist::clean_variable_names() %>% mutate_at("old_var_r", gsub,pattern ="\\s+$", replacement ="" ) %>% mutate_at("old_var_r", gsub,pattern ="\\s+", replacement =" ")


  #names(og_sheet) <- gsub(x= names(og_sheet),pattern ="\r\n",replacement = "", fixed = T)
  #names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+", replacement =" ")

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
  for (f in 19:35) {
  #for (f in 1:base::length(files)) {
    #isocode of country file
    iso<-substr(tools::file_path_sans_ext(basename(files[f])), 0, 3)

      print(iso)
    #for this iso code set parameters dictating file load in and clean process
    sheetname<-template_check$sheetname[template_check$country==iso]
    skip<-template_check$skip[template_check$country==iso]
    template<-template_check$template[template_check$country==iso]


    # load in file
    if(grepl(files[f],pattern="\\.xlsm$", ignore.case=TRUE)){
      og_sheet <- readxl::read_excel(files[f], sheet=sheetname, skip=skip)
    }else{
      og_sheet <- rio::import(files[f], which=sheetname, skip=skip)
    }

    names(og_sheet) <- gsub(x= names(og_sheet),pattern ="\r\n",replacement = "", fixed = T)
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+", replacement =" ")
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+$", replacement ="")

    # problem countries with regex patterns
    if(iso=="CIV"){
      og_sheet<-og_sheet %>% select(-c(1))
    }

    # filter variable cleaning dictionary specific to country file loaded
    var_dict_country<- var_dict %>% filter(country==iso)

    # list variables to drop that are non-template
    drop_nontemplate_vars<- as.list(var_dict_country$old_var_r[var_dict_country$notes=="non_template" & !is.na(var_dict_country$notes)])

    # drop variables from list which are non-tmeplate
    if(length(drop_nontemplate_vars)!=0){
      # drop variables from sheet which are non-tmeplate
      og_sheet<-og_sheet[ , -which(names(og_sheet) %in% drop_nontemplate_vars)]
    }

    # if file requires no cleaning as matched WHO standard template do this loop
    if(template==TRUE){
     #names(og_sheet) <- gsub(x= names(og_sheet),pattern ="\r\n",replacement = "", fixed = T)
     #names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+", replacement =" ")

     require(anytime)
     #handle dates that are numeric. All character dates -> NA.
     # Convert to numeric and change date format using origin of excel
     # Convert to character for final merge
     datesnumeric<-og_sheet %>% select(contains("Date", ignore.case = T)) %>% mutate_if(is.character, as.numeric) %>% mutate_if(is.numeric,as.Date, origin="1899-12-30") %>% mutate_all(as.character)
     datesnumeric$id<-rownames(datesnumeric)

     #handle dates that are characters. Select virst 10 characters in value. This will select all dates. Convert character formats to same ones usng parse date time.
     #Convert to numeric and change date format using origin of excel
     # Convert to character for final merge
     datescharacter<-og_sheet %>% select(contains("Date", ignore.case = T)) %>% mutate_at(vars(contains("Date", ignore.case = T)), ~ substr(., 1, 10)) %>% mutate_if(is.character, lubridate::parse_date_time, orders=c("ymd","dmy")) %>% mutate_if(is.character, as.numeric) %>%
       mutate_if(is.numeric,as.Date, origin="1899-12-30") %>% mutate_all(as.character)
     datescharacter$id<-rownames(datescharacter)

     #Natural join of data frames, fills in missing values in one df with values from other if not misssing
     dates<- rquery::natural_join(datesnumeric, datescharacter,
                          by = "id",
                          jointype = "FULL")

    # final date handeling change all columns to date class type (double)
    dates_final<- dates %>% mutate_at(vars(contains("Date", ignore.case = T)),anydate)

    # merge in these new handled dates to orginal sheet
     date_vars<-names(dates_final)
     og_sheet<-og_sheet[ , -which(names(og_sheet) %in% date_vars)]
     og_sheet$id<-rownames(og_sheet)

     output_sheet<- merge(og_sheet,dates_final, by="id") %>% select(-c(id))

     # outout sheet for next stage of function
    }else if(template==FALSE){

      # recode variables

      recode_vars<-as.list(var_dict_country$old_var_r[grepl("Recode",var_dict_country$notes) & !is.na(var_dict_country$notes)])

      if(length(recode_vars)!=0){
        var_dict_country<- var_dict_country[var_dict_country$old_var_r %in% names(og_sheet),]
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
      names(output_sheet) <-gsub(x= names(output_sheet),pattern ="\\s+", replacement =" ")
      names(output_sheet) <- with(var_dict_country,new_var[match(names(output_sheet),old_var_r)])
      output_sheet<-cbind(output_sheet,recode_sheet)
      }else{
        output_sheet<- og_sheet
        names(output_sheet) <- gsub(x= names(output_sheet),pattern ="\r\n",replacement = "", fixed = T)
        names(output_sheet) <-gsub(x= names(output_sheet),pattern ="\\s+", replacement =" ")
         names(output_sheet) <- with(var_dict_country,new_var[match(names(output_sheet),old_var_r)])
      }
      #handle all date variables
      require(anytime)
      datesnumeric<-output_sheet %>% select(contains("Date", ignore.case = T)) %>% mutate_if(is.character, as.numeric) %>% mutate_if(is.numeric,as.Date, origin="1899-12-30") %>% mutate_all(as.character)
      datesnumeric$id<-rownames(datesnumeric)

      datescharacter<-output_sheet %>% select(contains("Date", ignore.case = T)) %>% mutate_at(vars(contains("Date", ignore.case = T)), ~ substr(., 1, 10)) %>% mutate_if(is.character, lubridate::parse_date_time, orders=c("ymd","dmy")) %>% mutate_if(is.character, as.numeric) %>%
        mutate_if(is.numeric,as.Date, origin="1899-12-30") %>% mutate_all(as.character)
      datescharacter$id<-rownames(datescharacter)

      dates<- rquery::natural_join(datesnumeric, datescharacter,
                                   by = "id",
                                   jointype = "FULL")


      dates_final<- dates %>% mutate_at(vars(contains("Date", ignore.case = T)),anydate)

      date_vars<-names(dates_final)
      output_sheet<-output_sheet[ , -which(names(output_sheet) %in% date_vars)]
      output_sheet$id<-rownames(output_sheet)

      output_sheet<- merge(output_sheet,dates_final, by="id") %>% select(-c(id))

    }else if(template=="SKIP"){
      print(base::paste(files[f],"country file no longer in use"))
      next
    }

    #remove rows where all columns except ID is missing or id is missing (not a real row if so)
    output_sheet$na_rows <- rowSums(!is.na(output_sheet))
    output_sheet<-  output_sheet %>% filter(!is.na(patinfo_ID))  %>%  filter(na_rows > 1) %>% select(-c(na_rows))

    # make column for ISO code
    output_sheet$country_iso<-iso

    #make variable nsames lower case for output sheet
    names(output_sheet)<-tolower(names(output_sheet))


  ######
    #checking that variables of interest exists
    #if not create missing column and if possible manipulate output_sheet using surrogate variables to in fill this column

    vars<- colnames(output_sheet)
    output_sheet$id<-rownames(output_sheet)
    # some countries missing this can use lab result date to infill in next clean
    if(!("report_date" %in% vars)){
      output_sheet$report_date<-NA
      print("report_date")
    }

    if(!("patinfo_ageonset" %in% vars)){
      print("patinfo_ageonset")
      output_sheet$patinfo_ageonset<-NA
    }else{
      output_sheet$patinfo_ageonset<-as.numeric(iconv(output_sheet$patinfo_ageonset, 'utf-8', 'ascii', sub=''))
    }

    if(!("patinfo_sex" %in% vars)){
      print("patinfo_sex")
      output_sheet$patinfo_sex<-NA
    }else{
      output_sheet$patinfo_sex<- substr(output_sheet$patinfo_sex,1,1)
    }


      if(!("patinfo_resadmin1" %in% vars)){
        print("patinfo_resadmin1")
        output_sheet$patinfo_resadmin1<-NA
      }


    if(!("patinfo_resadmin2" %in% vars)){
      print("patinfo_resadmin2")
      output_sheet$patinfo_resadmin2<-NA
    }

    # if missing this variable use lab result for COVID to code up
    if(!("report_classif" %in% vars)){
      print("report_classif")
      output_sheet$report_classif<-NA
    }


#creat syptomatic column if missing this column
      if(!("pat_symptomatic" %in% vars)){
        print("pat_symptomatic")
        output_sheet$pat_symptomatic<-NA
      }
# fill in syp tomatic column if value is missing using other patsympt_ columns
# if has yes in any of the patsympt clolums then make patsymtp = 1. If entries are missing in these columns
# if all values are no  pat_symptomatic is set to 0
# the pat_symptomatic is set to missing
        symptoms<- output_sheet %>% select(id, starts_with("patsympt"))
        # if were not missing these columns then complete this loop
        if(dim(symptoms)[2]>0){
        symptoms<- symptoms %>% mutate_at(vars(contains("other")), ~replace(., !is.na(.), "yes"))
        symptoms<-data.frame(lapply(symptoms, function(x) {gsub("know", NA, x, ignore.case = T) }), stringsAsFactors=F)
        symptoms$pat_symptomatic.fill <- (!!rowSums(sapply(symptoms, grepl, pattern = "yes|oui|sim", ignore.case=T)))*1
        cols <- grep('patsympt', names(symptoms))
        symptoms$pat_symptomatic.fill[rowSums(!is.na(symptoms[cols])) <1] <- NA
        symptoms<-symptoms %>% select(c(id, pat_symptomatic.fill))
        output_sheet<-cbind(output_sheet,symptoms)
        output_sheet$pat_symptomatic<-ifelse(is.na(output_sheet$pat_symptomatic), output_sheet$pat_symptomatic.fill,output_sheet$pat_symptomatic )
}


    if(!("comcond_preexist" %in% vars)){
      print("comcond_preexist")
      output_sheet$comcond_preexist<-NA
    }

    #recode if missing this variable code up from the spciafied column
    if(!("comcond_preexist1" %in% vars)){
      print("comcond_preexist1")
      output_sheet$comcond_preexist1<-NA
    }
      output_sheet$comcond_preexist1<-ifelse(!is.na(output_sheet$comcond_preexist) & !grepl("NAO|NON|NO",output_sheet$comcond_preexist, ignore.case = T),"yes",output_sheet$comcond_preexist1)

    #these needs recoding to just healthcareworker or not can be done by next dictionary

    if(!("patinfo_occus" %in% vars)){
      print("patinfo_occus")
      output_sheet$patinfo_occus<-NA
    }

    if(!("expo_travel" %in% vars)){
      print("expo_travel")
      output_sheet$expo_travel<-NA
    }


    if(!("expo_travel_country" %in% vars)){
      print("expo_travel_country")
      output_sheet$expo_travel_country<-NA
    }

    if(!("expo_contact_case" %in% vars)){
      print("expo_contact_case")
      output_sheet$expo_contact_case<-NA
    }

    if(!("lab_datetaken" %in% vars)){
      print("lab_datetaken")
      output_sheet$lab_datetaken<-NA
    }

    # Some contires do not have lab result or erport classification
      # handle this issue in next dictionary cleaning step
    if(all(is.na(output_sheet$lab_result))){
      print("lab_result")
      output_sheet$lab_result<-NA
    }

    if(!("lab_resdate" %in% vars)){
      print("lab_resdate")
      output_sheet$lab_resdate<-NA
    }

    #Recode this if missing use patcurrent status
    if(!("patcourse_status" %in% vars)){
      print("patcourse_status")
      output_sheet$patcourse_status<-NA
    }
      if("patcurrent_status" %in% vars){
      output_sheet$patcourse_status<-ifelse(is.na(output_sheet$patcourse_status),output_sheet$patcurrent_status,output_sheet$patcourse_status)
    }

    #ensure this is for death, some files ie. KEN had date of outcome column whihc was used in this place
    #some cases who were not dead would have this entred into their date of death
    #make date of death missing if patcourse_status !=dead

    if(!("patcourse_datedeath" %in% vars)){
      print("patcourse_datedeath")
      output_sheet$patcourse_datedeath<-NA
    }

if("patcourse_status" %in% vars){
  #for countries where date death and date dischrage were combined we have recoded to date of death
  # fixing way this was coded here to set date of death to missing if patient is not dead
      output_sheet$patcourse_datedeath<- ifelse(!grepl("Obito|mort|dead|death|deceased",output_sheet$patcourse_status, ignore.case = T),NA,output_sheet$patcourse_datedeath)
      output_sheet$patcourse_datedeath<- as.Date(output_sheet$patcourse_datedeath, origin = "1970-01-01")

}else if("patcurrent_status" %in% vars) {
  output_sheet$patcourse_datedeath<- ifelse(!grepl("Obito|mort|dead|death|deceased",output_sheet$patcourse_status, ignore.case = T),NA,output_sheet$patcourse_datedeath)
  output_sheet$patcourse_datedeath<- as.Date(output_sheet$patcourse_datedeath, origin = "1970-01-01")
}




    #keep variables of interest
    output_sheet<- output_sheet %>% dplyr::select(report_date, patinfo_ageonset,
                                                  patinfo_sex, patinfo_resadmin1,
                                                  patinfo_resadmin2, report_classif,
                                                  pat_symptomatic,
                                                  comcond_preexist1, comcond_preexist,
                                                  patinfo_occus, expo_travel, expo_travel_country,
                                                  expo_contact_case, lab_result,
                                                  lab_datetaken, lab_resdate,
                                                  patcourse_status,
                                                  patcourse_datedeath, country_iso)



    #add cleaned output sheet to a list
    output[[f]] <-output_sheet

# part of the debug progess can delete this chuck when completed
    #the unique values in each column to build dictionary to do second round of cleaning
     ulst_sheet<- lapply(output_sheet[,sapply(output_sheet, is.character)], unique)
     n.obs <- sapply(ulst_sheet, length)
     seq.max <- seq_len(max(n.obs))
     mat <- as.data.frame(sapply(ulst_sheet, "[", i = seq.max))
     ulst[[f]]<-mat

  }



  cleaning_dict<-Reduce(function(...) merge(..., all=T), ulst)
  u<-lapply(cleaning_dict[,sapply(cleaning_dict, is.factor)], unique)
  n.obs <- sapply(u, length)
  seq.max <- seq_len(max(n.obs))
  u_cleaning_dict <- as.data.frame(sapply(u, "[", i = seq.max))




  big_data<-Reduce(function(...) merge(..., all=T), output)

  #find all possible values entries fro each of teh above variables for recoding


#
}

