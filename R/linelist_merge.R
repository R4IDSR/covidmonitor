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
#' @importFrom rio import export
#' @importFrom tidyr fill pivot_wider
#' @importFrom matchmaker match_vec
#' @importFrom dplyr bind_rows
#' @importFrom janitor
#' @author Alice Carr, Alex Spina
#' @export

#initialise list of ISO codes from the countries we are interested in merging
#this is based off Francks files
isotomerge<- c("BFA","CIV","COD","COG","DZA","GIN","KEN","LBR","MOZ","MUS","NAM","NER","RWA","SEN","SLE","STP","SWZ","SYC","TCD","UGA","ZWE")
inputdirectory="inst/extdata/frank_linelists/"
outputdirectory = tempdir()
outputname = "Merged_linelist_"

merge_linelist<- function(inputdirectory,
                          outputdirectory = tempdir(),
                          outputname = "Merged_linelist_", isotomerge) {

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
    mutate_at("old_var_r", gsub,pattern ="\\s+|[[:punct:]]", replacement ="_") %>% mutate_at(c("old_var_r","new_var"),tolower) %>%
    mutate_at(c("old_var_r","new_var"),stringi::stri_trans_general,"Latin-ASCII")# cleaning old variable names replacing all spaces and punctuation with . (as this is dont on the variable names upon loading files)


  #read in template and sheet checking dictionary
  template_check <- rio::import(
    here::here("inst","extdata", "linelist_dictionary.xlsx"),
    which = 3) %>% as_tibble() %>%
      linelist::clean_variable_names()

  # create an empty list to fill in with datasets
  output <- list()


  # for each file listed in input directory:
  for (f in 1:base::length(files)) {
    #ISO codes of country file
    iso<-substr(tools::file_path_sans_ext(basename(files[f])), 0, 3)


    #if ISO codes is not in initilsed list of interest skip this file, this could be defined already in template dictionary ?
    if(!(iso %in% isotomerge)){
      print(paste(iso,"skip"))
      next
      }

    print(iso)

    #for this iso code set parameters dictating file load in and clean process using the template_check dictionary
    sheetname<-template_check$sheetname[template_check$country==iso]
    skip<-template_check$skip[template_check$country==iso]
    template<-template_check$template[template_check$country==iso]


    # load in file
    #files that are xlsb (CIV) cannot be open (easily on mac) for ease resave the file in an xlsx format, this can be edited to use the RODBC package which i think works on windows
    if (grepl(files[f],pattern="\\.xlsb$", ignore.case=TRUE)){
      print(paste(iso, "file is in xlsb format, please resave file in xlsx format before proceeding"))
    }else{
      # og_sheet<- openxlsx::read.xlsx(files[f],sheet=sheetname, startRow=skip+1,detectDates=T, skipEmptyRows = F,
      #                                skipEmptyCols = F)
      og_sheet <- rio::import(files[f], which=sheetname, skip=skip)
    }

    # clean variable names to removed unwated regex patterns, replace all spaces with a ., inorder to match var_dict dictionary
    names(og_sheet) <- gsub(x= names(og_sheet),pattern ="\r\n",replacement = "", fixed = T)
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+", replacement =" ")
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+$", replacement ="")
    names(og_sheet) <-gsub(x= names(og_sheet),pattern ="\\s+|[[:punct:]]", replacement ="_")
    names(og_sheet) <-tolower(names(og_sheet))
    names(og_sheet)<- stringi::stri_trans_general(names(og_sheet) , "Latin-ASCII")

    #remove all accents from values in dataframe
    #replace all values in file that should be coded as missing
    #using gsub and regex pattern matching here as some patterns require to be fixed and others not
    og_sheet<-og_sheet %>% mutate_all(stringi::stri_trans_general,"Latin-ASCII") %>%
      mutate_all(gsub,pattern="['?]",replacement = "",ignore.case = T, perl = T) %>%
      mutate_all(gsub, pattern="(?i)^NA$|(?i)^N/A$|(?i)^N/A,|
                 (?i)^N\\A$|(?i)^Unknown$|(?i)know|(?i)^Unkown$|(?i)^N.A$|(?i)^NE SAIT PAS$|
                 (?i)^Inconnu$|^ $|(?i)^Nao aplicavel$|(?i)^Sem informacao$", replacement=NA, ignore.case= T, perl = T)

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

     #If file needs recoding of variables/merging of variables
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

      #remove following instances from variable names taht need recoding for the next step to leave what will become the recoded value
      names(recode_sheet)<- gsub(x = names(recode_sheet), pattern = "COVID.|Co19.|comcond.|patsympt.|patinfo.", replacement = "", ignore.case = T)

      #replace all instances of no with missing
      recode_sheet<- data.frame(lapply(recode_sheet, function(x) {gsub("NAO|NON|NO|none|nil|^[nN]$|Know", NA, x, ignore.case = T, perl = T) }), stringsAsFactors=F)
      #replace all instances of yes with 1
      recode_sheet <- data.frame(lapply(recode_sheet, function(x) {gsub("SIM|OUI|YES|1|(?i)^si$|^[yY]$", 1, x, ignore.case = T, perl = T) }), stringsAsFactors=F)
      #where all instances were 1 (yes) recode to the name of the variable for merge
      recode_sheet <-  data.frame(sapply(names(recode_sheet), function(x)ifelse(recode_sheet[,x] == 1, x, recode_sheet[,x])), stringsAsFactors=F)
      #renames variables with key of what to code to
      names(recode_sheet)<-paste(names(recode_sheet),variable_to, sep= ".")

      require(data.table)
      recode_sheet<-data.table(recode_sheet)
      #merge all variables inot one column with a common key
      recode_sheet[, paste(what_to_match,"", sep = "") :=
                 lapply(what_to_match, function(wtm) Reduce(function(x,y) paste(x,y, sep = ","),
                                                          .SD[, grep(wtm, names(recode_sheet)), with = F]))]

      #remove the . which was a place holder for a space
      recode_sheet <- data.frame(lapply(recode_sheet, function(x) {gsub(".", " ", x, fixed = T) }), stringsAsFactors=F)
      #replace the NA values which have been coded as text NA after reduce
      recode_sheet<-data.frame(lapply(recode_sheet, function(x) {gsub("NA", "", x, perl = T) }), stringsAsFactors=F)
      #replace everything that has a more than one comma or singula commas with no characters
      recode_sheet<-data.frame(lapply(recode_sheet, function(x) {gsub("^,*|(?<=,),|,*$", "", x, perl=T) }), stringsAsFactors=F)
      #replace blanks with NA
      recode_sheet[recode_sheet==""]<-NA
      #select newly recoded columns
      recode_sheet$id<-rownames(recode_sheet) #create an id variable for when merging back to og sheet occurs
      recode_sheet<- recode_sheet[ ,which(names(recode_sheet) %in% c(what_to_match,"id"))]

      #remove the columns that required recoding from original sheet
      output_sheet<-og_sheet[ , -which(names(og_sheet) %in% recode_vars)]


      #match old variable names with dictionary for new variable names
      names(output_sheet) <- with(var_dict_country,new_var[match(names(output_sheet),old_var_r)])
      #cbind with newly recoded variables
      output_sheet$id<-rownames(output_sheet)#create an id variable for when merging back to og sheet occurs
      output_sheet<-merge(output_sheet,recode_sheet,by="id") %>% select(-c(id))

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
    # Convert to character date for final merge
    output_sheet$id<-rownames(output_sheet)

    datesnumeric<-output_sheet %>% select(contains("Date", ignore.case = T),id) %>% mutate_all(.funs=gsub,pattern=".",replacement = "/", fixed=T, perl = T) %>%
      mutate_at(vars(contains("Date", ignore.case = T)), as.numeric) %>%
      mutate_at(vars(contains("Date", ignore.case = T)),as.Date, origin="1899-12-30") %>%
      mutate_at(vars(contains("Date", ignore.case = T)), as.character)


    #datesnumeric$id<-rownames(datesnumeric) #ensure correct matching upon merge

      #handle dates that are characters. Select first 10 characters in cell This will select all full dates and not any times appended.
      # Convert character formats using parse date time.
      # Convert to numeric and change date format using origin of excel
      # Convert to character date for final merge
      datescharacter<-output_sheet %>% select(contains("Date", ignore.case = T), id) %>%
        mutate_at(vars(contains("Date", ignore.case = T)), lubridate::parse_date_time, orders=c("ymd","Ymd","dmy","dmY","%Y-%m-%d","%y-%m-%d","%d-%m-%y","dBY","ymd HMS","Ymd HMS")) %>%
        mutate_at(vars(contains("Date", ignore.case = T)),as.Date, origin="1899-12-30") %>% mutate_at(vars(contains("Date", ignore.case = T)),as.character)

      #datescharacter$id<-rownames(datescharacter) #ensure correct matching upon merge

       #Natural join of data frames, fills in missing values in one df with values from other if not misssing

      dates<- rquery::natural_join(datesnumeric, datescharacter,
                                   by = "id",
                                   jointype = "FULL")

      # final date handling: change all columns to date class type (double)
      dates_final<- dates %>% mutate_at(vars(contains("Date", ignore.case = T)),lubridate::parse_date_time, orders=c("ymd"))
      # merge in these new handled dates to orginal sheet, removing old coded dates
      date_vars<-dates_final %>% select(contains("Date", ignore.case = T)) %>% names(.)
      output_sheet<-output_sheet[ , -which(names(output_sheet) %in% date_vars)]
      #output_sheet$id<-rownames(output_sheet)
      output_sheet<- merge(output_sheet,dates_final, by="id") %>% select(-c(id))



      #remove rows where all columns except ID is missing or if case id is missing
      output_sheet$na_rows <- rowSums(!is.na(output_sheet))
      output_sheet<-  output_sheet %>% filter(!is.na(patinfo_id))  %>%  filter(na_rows > 1) %>% select(-c(na_rows))


      # make column for ISO code
      output_sheet$country_iso<-iso
      #make variable names lower case for output sheet
      names(output_sheet)<-tolower(names(output_sheet))

  ######
    #checking that variables of interest exists
    #if not create missing column in cleaning we can use suurogate varaiables to infill

    vars<- colnames(output_sheet)
    output_sheet$id<-rownames(output_sheet)

     # some countries missing this can use lab result date to infill in next clean?
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

    if(!("pat_asymptomatic" %in% vars)){
      print("pat_asymptomatic")
      output_sheet$pat_asymptomatic<-NA
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

    if(!("patinfo_ageonsetunitdays" %in% vars)){
      print("patinfo_ageonsetunit")
      output_sheet$patinfo_ageonsetunitdays<-NA
    }



    #keep variables of interest
    output_sheet<- output_sheet %>% dplyr::select(report_date, patinfo_ageonset, patinfo_ageonsetunit,patinfo_ageonsetunitdays,
                                                  patinfo_sex, patinfo_resadmin1,
                                                  patinfo_resadmin2, report_classif,
                                                  pat_symptomatic, pat_asymptomatic,
                                                  comcond_preexist1, comcond_preexist,
                                                  patinfo_occus, expo_travel, expo_travel_country,
                                                  expo_contact_case, lab_result,
                                                  lab_datetaken, lab_resdate,
                                                  patcourse_status,
                                                  patcourse_datedeath, patcourse_datedischarge, country_iso, contains("patsympt"), patcurrent_status)



    #add cleaned output sheet to a list
    output[[f]] <-output_sheet
  }
    #merge all cleaned sheets into one
    output_fin<-output[!sapply(output,is.null)]
    big_data<-Reduce(function(...) merge(..., all=T), output_fin)
    big_data<-big_data[order(big_data$country_iso),]

    # write xlsx file to take on to ext step of cleaning
    openxlsx::write.xlsx(big_data, here::here("inst",paste(outputname, Sys.Date())))

  }



