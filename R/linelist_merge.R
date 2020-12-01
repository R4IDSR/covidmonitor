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
    linelist::clean_variable_names() %>% mutate_at("old_var_r", gsub,pattern ="\\s+$", replacement ="" ) %>%
    mutate_at("old_var_r", gsub,pattern ="\\s+", replacement =" ") %>% # cleaning old variable names from unwanted regex patterns
    mutate_at("old_var_r", gsub,pattern ="\\s+", replacement =".") # cleaning old variable names replacing all sapces with . (as this is dont on the variable names upon loading files)

  # read in dictionary for variable names in full format
  var_standards <- rio::import(
    here::here("inst","extdata", "linelist_dictionary.xlsx"),
    which = 2) %>% as_tibble() %>%
      linelist::clean_variable_names()


  #read in template and sheet checking dictionary
  template_check <- rio::import(
    here::here("inst","extdata", "linelist_dictionary.xlsx"),
    which = 3) %>% as_tibble() %>%
      linelist::clean_variable_names()


  # read in dictionary for recoding all patsympt_ variables (symptoms variables)
  #symptoms_dict<-rio::import(here::here("inst/","symptoms_dictionary.xlsx"), which=1) %>% as_tibble()


  # create an empty list to fill in with datasets
  output <- list()
  ulst<-list()

  #initialise list of ISO codes from the countries we are interested in merging - UPADTE AS REQUIRED
  iso_tomerge<- c("KEN", "UGA", "RWA", "BWA", "NAM", "ZWE", "SYC", "MUS", "STP", "MOZ", "GAB", "COD", "GIN", "BFA", "CIV", "SEN", "NER", "SWZ")

  # for each file listed in input directory:
  for (f in 1:base::length(files)) {
    #ISO codes of country file
    iso<-substr(tools::file_path_sans_ext(basename(files[f])), 0, 3)

    #if ISO codes is not in initilsed list of interest skip this file, this could be define already in template dictionary ?
    if(!(iso %in% iso_tomerge)){
      print(paste(iso,"skip"))
      next
      }

    print(iso)

    #for this iso code set parameters dictating file load in and clean process using the template_check dictionary
    sheetname<-template_check$sheetname[template_check$country==iso]
    skip<-template_check$skip[template_check$country==iso]
    template<-template_check$template[template_check$country==iso]


    # load in file
    #if any files are an xlsm load in using open xlsx package, this also corrects date issues in certain files that are identified below
    ############i may actually revert to using openxlsx for loading in all files ?#################

    if(grepl(files[f],pattern="\\.xlsm$|BWA|SWZ", ignore.case=TRUE)){
      og_sheet<- openxlsx::read.xlsx(files[f],sheet=sheetname, startRow=skip+1,detectDates=T, skipEmptyRows = F,
                                     skipEmptyCols = F)
    }else{
      og_sheet <- rio::import(files[f], which=sheetname, skip=skip)
    }

    # clean variable names to removed unwated regex patterns, replace all spaces with a ., inorder to match var_dict dictionary
    names(og_sheet) <- gsub(x= names(og_sheet),pattern ="\r\n",replacement = "", fixed = T)
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+", replacement =" ")
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+$", replacement ="")
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+", replacement =".")


    # problem countries with regex patterns
    ############come back to understanding why this column was a problem for CIV  ############
    if(iso=="CIV"){
      og_sheet<-og_sheet %>% select(-c(1))
    }

    # filter variable cleaning dictionary specific to country file loaded
    var_dict_country<- var_dict %>% filter(country==iso)
    var_dict_country<- var_dict_country[var_dict_country$old_var_r %in% names(og_sheet),]

    # list variables to drop that are non-template
    drop_nontemplate_vars<- as.list(var_dict_country$old_var_r[var_dict_country$notes=="non_template" & !is.na(var_dict_country$notes)])

    # drop variables from dataframe which are non-tmeplate
    if(length(drop_nontemplate_vars)!=0){
      og_sheet<-og_sheet[ , -which(names(og_sheet) %in% drop_nontemplate_vars)]
    }

    # if file requires no cleaning as matched WHO standard template do this loop
    if(template==TRUE){
      output_sheet<-og_sheet

     #If file needs recoding of variables
      }else if(template==FALSE){
      # recode variables

      recode_vars<-as.list(var_dict_country$old_var_r[grepl("Recode",var_dict_country$notes) & !is.na(var_dict_country$notes)])

      if(length(recode_vars)!=0){
      var_dict_country<- var_dict_country[var_dict_country$old_var_r %in% names(og_sheet),]
      #from variable dictionary find which variables require recode
      #these variables are usually patinfo_occus (proffession), comcond_exist (pre exsiting medical) and patsympt_other (other symptoms whihc have often been split inot separate variables)
      variable_to<- sub("Recode to ", "",as.list(var_dict_country$notes[grepl("Recode",var_dict_country$notes) & !is.na(var_dict_country$notes)]))
      what_to_match<-unique(variable_to)

      recode_sheet<- og_sheet[ , which(names(og_sheet) %in% recode_vars)]
      #remove instances of covid from variable names or comcond
      names(recode_sheet)<- gsub(x = names(recode_sheet), pattern = "COVID.|Co19.|comcond_", replacement = "", ignore.case = T)

      #replace all instances of no with missing
      recode_sheet<- data.frame(lapply(recode_sheet, function(x) {gsub("NAO|NON|NO|none|nil|^[nN]$|Know", NA, x, ignore.case = T, perl = T) }), stringsAsFactors=F)
      #replace all instances of yes with 1
      recode_sheet <- data.frame(lapply(recode_sheet, function(x) {gsub("SIM|OUI|YES|1|(?i)^si$|^[yY]$", 1, x, ignore.case = T, perl = T) }), stringsAsFactors=F)
      #where all instances were 1 (yes) recde to the name of the variable for merge
      recode_sheet <-  data.frame(sapply(names(recode_sheet), function(x)ifelse(recode_sheet[,x] == 1, x, recode_sheet[,x])), stringsAsFactors=F)
      #renames variables with key of what to code to
      names(recode_sheet)<-paste(names(recode_sheet),variable_to, sep= ".")

      require(data.table)
      recode_sheet<-data.table(recode_sheet)
      #merge all variables inot one column with a common key
      recode_sheet[, paste(what_to_match,"", sep = "") :=
                 lapply(what_to_match, function(wtm) Reduce(function(x,y) paste(x, y, sep = ","),
                                                          .SD[, grep(wtm, names(recode_sheet)), with = F]))]

      #remove the . which was a place holder for a space
      recode_sheet <- data.frame(lapply(recode_sheet, function(x) {gsub(".", " ", x, fixed = T) }), stringsAsFactors=F)
      #replace the NA values which have been coded as NA after reduce
      recode_sheet<-data.frame(lapply(recode_sheet, function(x) {gsub("NA", "", x, perl = T) }), stringsAsFactors=F)
      #replace everything that has a more than one comma or singula commas with no characters
      recode_sheet<-data.frame(lapply(recode_sheet, function(x) {gsub("^,*|(?<=,),|,*$", "", x, perl=T) }), stringsAsFactors=F)
      #replace blanks with NA
      recode_sheet[recode_sheet==""]<-NA
      #select newly recoded columns
      recode_sheet<- recode_sheet[ , which(names(recode_sheet) %in% what_to_match)]

      #remove the columns that required recoding from original sheet
      output_sheet<-og_sheet[ , -which(names(og_sheet) %in% recode_vars)]

      #names(output_sheet) <- gsub(x= names(output_sheet),pattern ="\r\n",replacement = "", fixed = T)    #### maybe unneccesary so comment out
      #names(output_sheet) <-gsub(x= names(output_sheet),pattern ="\\s+", replacement =" ")         #### maybe unneccesary so comment out

      #match old variable names with dictionary for new variable names
      names(output_sheet) <- with(var_dict_country,new_var[match(names(output_sheet),old_var_r)])
      #cbind with newly recoded variables
      output_sheet<-cbind(output_sheet,recode_sheet)

      # if there are no variables that needed recoding just match old variable names with dictionary for new variable names
      }else{
        output_sheet<- og_sheet
        #names(output_sheet) <- gsub(x= names(output_sheet),pattern ="\r\n",replacement = "", fixed = T)    #### maybe unneccesary so comment out
        #names(output_sheet) <-gsub(x= names(output_sheet),pattern ="\\s+", replacement =" ")         #### maybe unneccesary so comment out
         names(output_sheet) <- with(var_dict_country,new_var[match(names(output_sheet),old_var_r)])
      }


      }
      #handle all date variables
      require(anytime)
    #handle dates that are numeric. All character dates -> NA.
    # Convert to numeric and change date format using origin of excel
    # Convert to character for final merge
      datesnumeric<-output_sheet %>% select(contains("Date", ignore.case = T)) %>% mutate_all(.funs=gsub,pattern=".",replacement = "/", ignore.case = T, perl = T) %>%
        mutate_if(is.character, as.numeric) %>%
        mutate_if(is.numeric,as.Date, origin="1899-12-30") %>%
        mutate_all(as.character)

      datesnumeric$id<-rownames(datesnumeric) #ensure correct matching upon merge

      #handle dates that are characters. Select first 10 characters in value. This will select all full dates. Convert character formats to same ones usng parse date time.
      # Convert to numeric and change date format using origin of excel
      # Convert to character for final merge
      datescharacter<-output_sheet %>% select(contains("Date", ignore.case = T)) %>%
        mutate_at(vars(contains("Date", ignore.case = T)), ~ substr(., 1, 10)) %>%
        mutate_if(is.character, lubridate::parse_date_time, orders=c("ymd","dmy")) %>%
        mutate_if(is.character, as.numeric) %>%
        mutate_if(is.numeric,as.Date, origin="1899-12-30") %>% mutate_all(as.character)

      datescharacter$id<-rownames(datescharacter) #ensure correct matching upon merge

       #Natural join of data frames, fills in missing values in one df with values from other if not misssing

      dates<- rquery::natural_join(datesnumeric, datescharacter,
                                   by = "id",
                                   jointype = "FULL")

      # final date handling: change all columns to date class type (double)
      dates_final<- dates %>% mutate_at(vars(contains("Date", ignore.case = T)),anydate)

      # merge in these new handled dates to orginal sheet, removing old coded dates
      date_vars<-names(dates_final)
      output_sheet<-output_sheet[ , -which(names(output_sheet) %in% date_vars)]
      output_sheet$id<-rownames(output_sheet)


      output_sheet<- merge(output_sheet,dates_final, by="id") %>% select(-c(id))

      #remove rows where all columns except ID is missing or if case id is missing
      output_sheet$na_rows <- rowSums(!is.na(output_sheet))
      output_sheet<-  output_sheet %>% filter(!is.na(patinfo_ID))  %>%  filter(na_rows > 1) %>% select(-c(na_rows))

      # make column for ISO code
      output_sheet$country_iso<-iso
      #make variable names lower case for output sheet
      names(output_sheet)<-tolower(names(output_sheet))

  ######
    #checking that variables of interest exists
    #if not create missing column and if possible manipulate output_sheet using surrogate variables to in fill this column

    vars<- colnames(output_sheet)
    output_sheet$id<-rownames(output_sheet)

     # some countries missing this can use lab result date to infill in next clean
    if(!("report_date" %in% vars)){
      output_sheet$report_date<-NA
      output_sheet$report_date<-as.Date(output_sheet$report_date,origin="1899-12-30")
      print("report_date")
    }

    if(!("patinfo_ageonset" %in% vars)){
      print("patinfo_ageonset")
      output_sheet$patinfo_ageonset<-NA

    }else{
      output_sheet$patinfo_ageonset<-as.numeric(iconv(output_sheet$patinfo_ageonset, 'utf-8', 'ascii', sub='')) # some ages had issues on and wouldnt merge
    }

    if(!("patinfo_sex" %in% vars)){
      print("patinfo_sex")
      output_sheet$patinfo_sex<-NA
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


      if(!("pat_symptomatic" %in% vars)){
        print("pat_symptomatic")
        output_sheet$pat_symptomatic<-NA
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
      output_sheet$lab_datetaken<-as.Date(output_sheet$lab_datetaken,origin="1899-12-30")
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
      output_sheet$lab_resdate<-as.Date(output_sheet$lab_resdate,origin="1899-12-30")

    }

    #Recode this if missing use patcurrent status
    if(!("patcourse_status" %in% vars)){
      print("patcourse_status")
      output_sheet$patcourse_status<-NA
    }


    #ensure this is for death, some files ie. KEN had date of outcome column whihc was used in this place
    #some cases who were not dead would have this entred into their date of death
    #make date of death missing if patcourse_status !=dead

    if(!("patcourse_datedeath" %in% vars)){
      print("patcourse_datedeath")
      output_sheet$patcourse_datedeath<-NA
      output_sheet$patcourse_datedeath<-as.Date(output_sheet$patcourse_datedeath,origin="1899-12-30")
    }

    if(!("patcurrent_status" %in% vars)) {
      print("patcurrent_status")
      output_sheet$patcurrent_status<-NA
}

if(length(grep("patsympt",vars))==0){
        print("no patsmpt")
        output_sheet$patsympt_other<-NA
      }


    if(!("patcourse_datedischarge" %in% vars)){
      print("patcourse_datedischarge")
      output_sheet$patcourse_datedischarge<-NA
      output_sheet$patcourse_datedischarge<-as.Date(output_sheet$patcourse_datedischarge,origin="1899-12-30")
    }


    if(!("patinfo_ageonsetunit" %in% vars)){
      print("patinfo_ageonsetunit")
      output_sheet$patinfo_ageonsetunit<-NA
    }
    #keep variables of interest
    output_sheet<- output_sheet %>% dplyr::select(report_date, patinfo_ageonset, patinfo_ageonsetunit,
                                                  patinfo_sex, patinfo_resadmin1,
                                                  patinfo_resadmin2, report_classif,
                                                  pat_symptomatic,
                                                  comcond_preexist1, comcond_preexist,
                                                  patinfo_occus, expo_travel, expo_travel_country,
                                                  expo_contact_case, lab_result,
                                                  lab_datetaken, lab_resdate,
                                                  patcourse_status,
                                                  patcourse_datedeath, patcourse_datedischarge, country_iso, contains("patsympt"), patcurrent_status)



    #add cleaned output sheet to a list
    output[[f]] <-output_sheet

    # part of the debug progess can delete this chunck when completed
    #the unique values in each column to build dictionary to do second round of cleaning
    ulst_sheet<- lapply(output_sheet[,sapply(output_sheet, is.character)], unique)
    n.obs <- sapply(ulst_sheet, length)
    seq.max <- seq_len(max(n.obs))
    if(iso=="TZA"){
      mat<-as.data.frame(ulst_sheet)
    }else {
      mat <- as.data.frame(sapply(ulst_sheet, "[", i = seq.max))
    }
    ulst[[f]]<-mat

  }

###############Can be commented out once code is up and running#######################
  # for each element of the ulst list piviot to long and make iso code variable
  # specific dictionary creation. Unique values form the columns of character class for each iso code with an output sheet

#merge all cleaned sheets into one

  output_fin<-output[!sapply(output,is.null)]
  big_data<-Reduce(function(...) merge(..., all=T), output_fin)
  big_data<-big_data[order(big_data$country_iso),]

  # write csv to store where up to curretnly with cleaning this file
  write.csv(big_data, "inst/merged_cases.csv")
  openxlsx::write.xlsx(big_data, "inst/merged_cases_ofinterest.xlsx")


  ############## cleaning step

  #remove all instances that should be coded as missing
  big_data_clean<-data.frame(lapply(big_data, function(x) {gsub("(?i)^NA$|(?i)^N/A$|(?i)^N/A,|(?i)^N\\A$|(?i)^Unknown$|(?i)know|(?i)^Unkown$|(?i)^N.A$|(?i)^NE SAIT PAS$|(?i)^Inconnu$|^ $",NA, x, ignore.case= T, perl = T) }), stringsAsFactors=F)


  ##report date ##
  #test for problem dates handel asll dates this same way
  #problem_dates<-subset(big_data_clean, report_date < as.Date("2020-01-01") | report_date > as.Date(Sys.Date()))

  ##patinfo_ageonset##
  big_data_clean$patinfo_ageonset<- ifelse(grepl("month|M",big_data_clean$patinfo_ageonsetunit, ignore.case = T), as.numeric(big_data_clean$patinfo_ageonset)/12, big_data_clean$patinfo_ageonset)
  big_data_clean$patinfo_ageonset<- ifelse(grepl("day",big_data_clean$patinfo_ageonsetunit, ignore.case = T), as.numeric(big_data_clean$patinfo_ageonset)/365, big_data_clean$patinfo_ageonset)

# remove writen units
  big_data_clean$patinfo_ageonsetunit<- ifelse(grepl("month|M|year|day|0",big_data_clean$patinfo_ageonsetunit, ignore.case = T),NA,big_data_clean$patinfo_ageonsetunit)
  # cokumn was suppose to be for ages under 12 months so convert these to years and leave the rest
  big_data_clean$patinfo_ageonsetunit<- ifelse(as.numeric(big_data_clean$patinfo_ageonsetunit)<12, as.numeric(big_data_clean$patinfo_ageonsetunit)/12,big_data_clean$patinfo_ageonsetunit)

  # replace ages that are in the unit column if missing in the normal age column or 0 in the normal age column but has a valueinputed in the unit column
  big_data_clean$patinfo_ageonset<-ifelse(is.na(big_data_clean$patinfo_ageonset)|big_data_clean$patinfo_ageonset==0 & !is.na(big_data_clean$patinfo_ageonset), big_data_clean$patinfo_ageonsetunit, big_data_clean$patinfo_ageonset)

  #drop ages that are clearly incorrect
  big_data_clean$patinfo_ageonset<- ifelse(big_data_clean$patinfo_ageonset>120 | big_data_clean$patinfo_ageonset<0 & !is.na(big_data_clean$patinfo_ageonset),NA,big_data_clean$patinfo_ageonset)

  #drop unit clumnas now irrelavent
  big_data_clean$patinfo_ageonsetunit<-NULL

  ##patinfo_sex##
  #keep first letter of sex (always M or F)
  big_data_clean$patinfo_sex<- substr(big_data_clean$patinfo_sex,1,1)
  #capitalise
  big_data_clean$patinfo_sex<-ifelse(grepl("m", big_data_clean$patinfo_sex, ignore.case = T),"M",big_data_clean$patinfo_sex)
  big_data_clean$patinfo_sex<-ifelse(grepl("f", big_data_clean$patinfo_sex, ignore.case = T),"F",big_data_clean$patinfo_sex)
  #if entered incorrect make equal to NA
  big_data_clean$patinfo_sex<-ifelse(!grepl("M|F", big_data_clean$patinfo_sex, ignore.case = T),NA,big_data_clean$patinfo_sex)


  ##pat_symptomatic##
  #ensure id variable is present as splitting up dataframe
  big_data_clean$id<-rownames(big_data_clean)
  #nex steps requrie dataframe format (not data table)

  big_data_clean<-data.frame(big_data_clean)
  #ensure orihinal pat_symptomatic contains no numbers of special characters and only yes or no
  big_data_clean<-big_data_clean %>% mutate_at("pat_symptomatic",.funs=gsub,pattern="[0-9?]",replacement = NA, ignore.case = T, perl = T) %>%
    mutate_at("pat_symptomatic",.funs=gsub,pattern="(?i)^no$|(?i)^non$|(?i)^n$",replacement = "no", ignore.case = T, perl = T) %>%
    mutate_at("pat_symptomatic",.funs=gsub,pattern="(?i)^yes$|(?i)^oui$|(?i)^sim$|(?i)^y$",replacement = "yes", ignore.case = T, perl = T)
  big_data_clean$pat_symptomatic<-ifelse(!grepl("^no$|^yes$",big_data_clean$pat_symptomatic, ignore.case = T),NA,big_data_clean$pat_symptomatic)

  #Creat a symptomatic column those missing variable using other symptoms variables
  symptoms<- big_data_clean %>% select(id,contains("sympt")) %>% select(-contains("pat_symptomatic"))
  symptvars<-names(symptoms)
  #symptoms[,-1] means apply to everything except column 1 which is the id variable
  #remove any ? or numerics
  symptoms[,-1]<- data.frame(lapply(symptoms[,-1], function(x) {gsub("[0-9?]",NA, x, ignore.case = T, perl = T) }), stringsAsFactors=F)
  #change all variations of no to standardised
  symptoms[,-1]<- data.frame(lapply(symptoms[,-1], function(x) {gsub("^NAO$|^NON$|^NO$|^none$|^nil$|^null$|.*Know.*|^N/A$|^NA$|0|^not$|^n$|nn","no", x, ignore.case = T, perl = T) }), stringsAsFactors=F)
  #replace 1 or more spaces with one space
  symptoms[,-1]<- data.frame(lapply(symptoms[,-1], function(x) {gsub("\\s+", " ",x, ignore.case = T, perl = T) }), stringsAsFactors=F)
  #blank cells to NA
  symptoms[symptoms==" "]<-NA
  symptoms[symptoms==""]<-NA

  #determine if patient had symptoms
  #sum row if contains no or is missing (if all rows are no of missing =7)
  symptoms$symptoms_none<-rowSums(symptoms[,-1]=="no" | is.na(symptoms[,-1]))
  #sum rwo if it is missing (if all ros missing =7)
  symptoms$symptoms_na<-rowSums(is.na(symptoms[,-1]))
  #initialised symptomatic variable
  symptoms$pat_symptomatic<-NA

  #if row sums in symptoms_none=7 then all rows had no or missing in symptoms
  symptoms$pat_symptomatic<-ifelse(symptoms$symptoms_none==7,"no",symptoms$pat_symptomatic)
  #if row sums in symptoms_none<7 then at least one row had yes or other symptoms
  symptoms$pat_symptomatic<-ifelse(symptoms$symptoms_none<7,"yes",symptoms$pat_symptomatic)
  #replace rows that had all missing in all symptoms
  symptoms$pat_symptomatic<-ifelse(symptoms$symptoms_na==7,NA,symptoms$pat_symptomatic)

  #add id back to columns for merging
  symptoms_clean<- symptoms %>% select(id,pat_symptomatic)

  #merge with big data to infill symptomatic yes or no, only if missing this variable
  big_data_clean<-merge(big_data_clean,symptoms_clean, by="id")
  big_data_clean$pat_symptomatic.x<-ifelse(is.na(big_data_clean$pat_symptomatic.x), big_data_clean$pat_symptomatic.y,big_data_clean$pat_symptomatic.x)

  big_data_clean_test<- big_data_clean %>% select(id,contains("sympt"))
  #final clean up of pat_symptomatic
  big_data_clean$pat_symptomatic<-big_data_clean$pat_symptomatic.x
  big_data_clean$pat_symptomatic.x<-NULL
  big_data_clean$pat_symptomatic.y<-NULL

  #remove now unneccessary symptoms variables
  big_data_clean<-big_data_clean[ , -which(names(big_data_clean) %in% symptvars)]


  #patinfo_occus
  #list of words/ strings associated with healthcare
  occupation<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=1) %>% select(patinfo_occus)
  #remove accents from occupation column
  big_data_clean<-data.table(big_data_clean)
  big_data_clean<-big_data_clean[, patinfo_occus := stringi::stri_trans_general(str = patinfo_occus,
                                                                    id = "Latin-ASCII")]
  #Create a column that creates TRUE for health care worker after a match or partial match with list above
  big_data_clean$hcw <- grepl(paste(occupation$patinfo_occus, collapse="|"),big_data_clean$patinfo_occus, ignore.case = T)
  #replce if occupation was missing
  big_data_clean$hcw<- ifelse(is.na(big_data_clean$patinfo_occus),NA,big_data_clean$hcw) #this column is if healthcare workerso use in analysis as this


  #patcourse_status
  #list of words/ strings associated with dead or alive
  dead<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(dead)
  alive<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(alive)
  #remove accents from status columns
  big_data_clean<-data.table(big_data_clean)
  big_data_clean<-big_data_clean[, patcourse_status := stringi::stri_trans_general(str = patcourse_status, id = "Latin-ASCII")]
  big_data_clean<-big_data_clean[, patcurrent_status := stringi::stri_trans_general(str = patcurrent_status, id = "Latin-ASCII")]
  #remove any numbers in there or special characters
  big_data_clean<-big_data_clean %>% mutate_at("patcourse_status",.funs=gsub,pattern="[0-9?]",replacement = NA, ignore.case = T, perl = T)  %>%
    mutate_at("patcurrent_status",.funs=gsub,pattern="[0-9?]",replacement = NA, ignore.case = T, perl = T)
  big_data_clean$patcourse_status_dead<- grepl(paste(dead$dead, collapse="|"),big_data_clean$patcourse_status, ignore.case = T)
  big_data_clean$patcourse_status_dead<-ifelse(big_data_clean$patcourse_status_dead==FALSE,NA,big_data_clean$patcourse_status_dead)
  big_data_clean$patcourse_status_dead<-ifelse(big_data_clean$patcourse_status_dead==TRUE,"dead",big_data_clean$patcourse_status_dead)
  big_data_clean$patcourse_status_alive<- grepl(paste(alive$alive, collapse="|"),big_data_clean$patcourse_status, ignore.case = T)
  big_data_clean$patcourse_status_alive<-ifelse(big_data_clean$patcourse_status_alive==FALSE,NA,big_data_clean$patcourse_status_alive)
  big_data_clean$patcourse_status_alive<-ifelse(big_data_clean$patcourse_status_alive==TRUE,"alive",big_data_clean$patcourse_status_alive)

  big_data_clean$patcourse_status<- ifelse(!is.na(big_data_clean$patcourse_status) & !is.na(big_data_clean$patcourse_status_dead), big_data_clean$patcourse_status_dead,big_data_clean$patcourse_status)
  big_data_clean$patcourse_status<-ifelse(!is.na(big_data_clean$patcourse_status) & !is.na(big_data_clean$patcourse_status_alive), big_data_clean$patcourse_status_alive, big_data_clean$patcourse_status)
  big_data_clean$patcourse_status<-ifelse(big_data_clean$patcourse_status=="LTFU" | grepl("lost",big_data_clean$patcourse_status, ignore.case = T),"Lost to follow up",big_data_clean$patcourse_status)




  if("patcurrent_status" %in% vars){
    output_sheet$patcourse_status<-ifelse(is.na(output_sheet$patcourse_status),output_sheet$patcurrent_status,output_sheet$patcourse_status)
  }

  if("patcourse_status" %in% vars){
    #for countries where date death and date dischrage were combined we have recoded to date of death
    # fixing way this was coded here to set date of death to missing if patient is not dead
    output_sheet$patcourse_datedeath<- ifelse(!grepl("Obito|mort|dead|death|deceased",output_sheet$patcourse_status, ignore.case = T),NA,output_sheet$patcourse_datedeath)
    output_sheet$patcourse_datedeath<- as.Date(output_sheet$patcourse_datedeath, origin = "1970-01-01")

  }

  output_sheet$comcond_preexist1<-ifelse(!is.na(output_sheet$comcond_preexist) & !grepl("NAO|NON|NO",output_sheet$comcond_preexist, ignore.case = T),"yes",output_sheet$comcond_preexist1)

}else{
  output_sheet$patinfo_sex<- substr(output_sheet$patinfo_sex,1,1)
}
#
}

#Symptoms variables V lookup
VLookup <- function(this, data, key, value) {
  m <- match(this, data[[key]])
  data[[value]][m]
}




ulst.longer<-bind_rows(ulst, .id = 'country_iso') %>% #type.convert(as.is = TRUE) %>%
  tidyr::pivot_longer(col= -country_iso,names_to = "Variable",
                      values_to = "old_spelling", values_drop_na = TRUE)

#openxlsx::write.xlsx(ulst.longer, "inst/cleaning_dictionary_general.xlsx")

ulst.longer.isocode<-bind_rows(ulst, .id = 'id') %>% select(id, country_iso) %>% na.omit()
names(ulst.longer.isocode)<-c("country_iso","iso_code")

cleaning_dict_final<-merge(ulst.longer, ulst.longer.isocode, by="country_iso")
names(cleaning_dict_final)[1]<- "dfnumber"
openxlsx::write.xlsx(cleaning_dict_final, "inst/cleaning_dictionary.xlsx")



cleaning_rules<-openxlsx::read.xlsx("/Users/aljc201/Documents/PhD/WHO/linelists/franck linelists/Merge_Linelist/data/cleaned/cleaning_rules_1.xlsx")

unique(cleaning_rules$variable)
goodspelling<-read.csv("inst/ConfirmedCases.csv")
goodspelling<-goodspelling %>% select(-contains("date", ignore.case = T))
gs<-lapply(goodspelling[,sapply(goodspelling, is.factor)], unique)
n.obs <- sapply(gs, length)
seq.max <- seq_len(max(n.obs))
gs_dict <- as.data.frame(sapply(gs, "[", i = seq.max))


########################################################################################################################


