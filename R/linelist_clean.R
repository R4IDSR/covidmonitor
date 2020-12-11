library(dplyr)
library(data.table)

big_data<-rio::import(here::here("inst", "Merged_linelist_2020-12-11.xlsx"), readxl = FALSE)
#import has for some reason lost the values in some columns (ageonsetdays), due to the read_excel that is used.
#as the output from the merge file is a .xlsx we can specify readxl =FALSE so the read.xlsx function will be used on the import instead
#the read.xlsx functions requires the dependancy openxlsx


big_data_clean<-big_data

#remove all instances that should be coded as missing
# big_data_clean<-data.frame(lapply(big_data, function(x) {gsub("(?i)^NA$|(?i)^N/A$|(?i)^N/A,|(?i)^N\\A$|(?i)^Unknown$|(?i)know|(?i)
# #                                                               ^Unkown$|(?i)^N.A$|(?i)^NE SAIT PAS$|(?i)^Inconnu$|^ $|(?i)^Nao aplicavel$|(?i)^Sem informacao$",NA, x, ignore.case= T, perl = T) }), stringsAsFactors=F)


##report date ##
#test for problem dates handle all dates that are same way
#problem_dates<-subset(big_data_clean, report_date < as.Date("2020-01-01") | report_date > as.Date(Sys.Date()))

##patinfo_ageonset##
big_data_clean$patinfo_ageonset<- ifelse(grepl("month|M",big_data_clean$patinfo_ageonsetunit, ignore.case = T), as.numeric(big_data_clean$patinfo_ageonset)/12, big_data_clean$patinfo_ageonset)
big_data_clean$patinfo_ageonset<- ifelse(grepl("day",big_data_clean$patinfo_ageonsetunit, ignore.case = T), as.numeric(big_data_clean$patinfo_ageonset)/365, big_data_clean$patinfo_ageonset)

# remove writen units
big_data_clean$patinfo_ageonsetunit<- ifelse(grepl("month|M|year|day|^0$",big_data_clean$patinfo_ageonsetunit, ignore.case = T),NA,big_data_clean$patinfo_ageonsetunit)
# column was suppose to be for ages under 12 months so convert these to years and leave the rest
big_data_clean$patinfo_ageonsetunit<- as.numeric(big_data_clean$patinfo_ageonsetunit)/12

big_data_clean$patinfo_ageonsetunitdays<- ifelse(grepl("month|M|year|day|^0$",big_data_clean$patinfo_ageonsetunitdays, ignore.case = T),NA,big_data_clean$patinfo_ageonsetunitdays)
# column was suppose to be for ages under 12 months so convert these to years and leave the rest
big_data_clean$patinfo_ageonsetunitdays<- as.numeric(big_data_clean$patinfo_ageonsetunitdays)/365


# replace ages that are in the unit column if missing in the normal age column or 0 in the normal age column but has a valueinputed in the unit column
big_data_clean$patinfo_ageonset<-ifelse(is.na(big_data_clean$patinfo_ageonset)|big_data_clean$patinfo_ageonset==0 & !is.na(big_data_clean$patinfo_ageonsetunit), big_data_clean$patinfo_ageonsetunit, big_data_clean$patinfo_ageonset)
big_data_clean$patinfo_ageonset<-ifelse(is.na(big_data_clean$patinfo_ageonset)|big_data_clean$patinfo_ageonset==0 & !is.na(big_data_clean$patinfo_ageonsetunitdays), big_data_clean$patinfo_ageonsetunitdays, big_data_clean$patinfo_ageonset)

#drop ages that are clearly incorrect
big_data_clean$patinfo_ageonset<- ifelse(big_data_clean$patinfo_ageonset>120 | big_data_clean$patinfo_ageonset<0 & !is.na(big_data_clean$patinfo_ageonset),NA,big_data_clean$patinfo_ageonset)

#drop unit columns now irrelavent
big_data_clean$patinfo_ageonsetunit<-NULL
big_data_clean$patinfo_ageonsetunitdays<-NULL


##patinfo_sex##
#keep first letter of sex (always M or F)
big_data_clean<-big_data_clean %>% mutate_at("patinfo_sex",.funs=gsub,pattern="[0-9?]",replacement = NA, ignore.case = T, perl = T)
big_data_clean$patinfo_sex<- substr(big_data_clean$patinfo_sex,1,1)
#capitalise
big_data_clean$patinfo_sex<- toupper(big_data_clean$patinfo_sex)
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
symptoms<- big_data_clean %>% select(id,contains("sympt")) %>% select(-contains(c("pat_symptomatic", "pat_asymptomatic")))
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


#final clean up of pat_symptomatic
big_data_clean$pat_symptomatic<-big_data_clean$pat_symptomatic.x
big_data_clean$pat_asymptomatic<-ifelse(big_data_clean$pat_asymptomatic==1,0,big_data_clean$pat_asymptomatic)
big_data_clean$pat_symptomatic<-ifelse(is.na(big_data_clean$pat_symptomatic) & !is.na(big_data_clean$pat_asymptomatic) & big_data_clean$pat_asymptomatic==0,"no",big_data_clean$pat_symptomatic)

big_data_clean$pat_symptomatic.x<-NULL
big_data_clean$pat_symptomatic.y<-NULL
big_data_clean$pat_asymptomatic<-NULL

#debug to check
#sympt_test<- big_data_clean %>% select(id,contains("sympt"))

#remove now unneccessary symptoms variables but leave symptomatic variable
big_data_clean<-big_data_clean[ , -which(names(big_data_clean) %in% symptvars)]


#patinfo_occus
#list of words/ strings associated with healthcare
occupation<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=1) %>% select(patinfo_occus)
#remove accents from occupation column
big_data_clean<-data.table(big_data_clean)
#Create a column that creates TRUE for health care worker after a match or partial match with list above
big_data_clean$hcw <- grepl(paste(occupation$patinfo_occus, collapse="|"),big_data_clean$patinfo_occus, ignore.case = T)
#replce if occupation was missing
big_data_clean$hcw<- ifelse(is.na(big_data_clean$patinfo_occus),NA,big_data_clean$hcw) #this column is if healthcare workerso use in analysis as this
oc_test<- big_data_clean %>% select(patinfo_occus,hcw)

#patcourse_status
#list of words/ strings associated with dead or alive
dead<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(dead) %>% na.omit()
alive<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(alive) %>% na.omit()

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



#drop columns created, uncommment if needed for debugging
big_data_clean$patcourse_status_dead<-NULL
big_data_clean$patcourse_status_alive<-NULL
big_data_clean$patcourse_status<-ifelse(!grepl("alive|dead|lost|pending",big_data_clean$patcourse_status, ignore.case = T),NA,big_data_clean$patcourse_status)


#Date of death handling.
#some countries used a date of outcome column, this needs cleaning if patient is not dead then empy the date of death column and put dtae in the discharge column
big_data_clean$patcourse_datedischarge<-ifelse(big_data_clean$patcourse_status!="dead",big_data_clean$patcourse_datedeath,big_data_clean$patcourse_datedischarge)
big_data_clean$patcourse_datedeath<-ifelse(big_data_clean$patcourse_status!="dead",NA,big_data_clean$patcourse_datedeath)
big_data_clean$patcourse_datedischarge<-ifelse(big_data_clean$patcourse_status=="dead",NA,big_data_clean$patcourse_datedischarge)


#report classif
probabale<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(probable) %>% na.omit()
suspected<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(suspected) %>% na.omit()
confirmed<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(confirmed) %>% na.omit()
notacase<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(notacase) %>% na.omit()

#remove any numbers in there or special characters
big_data_clean<-big_data_clean %>% mutate_at("report_classif",.funs=gsub,pattern="[0-9?]",replacement = NA, ignore.case = T, perl = T)

#probable
big_data_clean$report_classif_probable<- grepl(paste(probabale$probable, collapse="|"),big_data_clean$report_classif, ignore.case = T)
big_data_clean$report_classif_probable<-ifelse(big_data_clean$report_classif_probable==FALSE,NA,big_data_clean$report_classif_probable)
big_data_clean$report_classif_probable<-ifelse(big_data_clean$report_classif_probable==TRUE,"probabale",big_data_clean$report_classif_probable)
#suspected
big_data_clean$report_classif_suspected<- grepl(paste(suspected$suspected, collapse="|"),big_data_clean$report_classif, ignore.case = T)
big_data_clean$report_classif_suspected<-ifelse(big_data_clean$report_classif_suspected==FALSE,NA,big_data_clean$report_classif_suspected)
big_data_clean$report_classif_suspected<-ifelse(big_data_clean$report_classif_suspected==TRUE,"suspected",big_data_clean$report_classif_suspected)
#confirmed
big_data_clean$report_classif_confirmed<- grepl(paste(confirmed$confirmed, collapse="|"),big_data_clean$report_classif, ignore.case = T)
big_data_clean$report_classif_confirmed<-ifelse(big_data_clean$report_classif_confirmed==FALSE,NA,big_data_clean$report_classif_confirmed)
big_data_clean$report_classif_confirmed<-ifelse(big_data_clean$report_classif_confirmed==TRUE,"confirmed",big_data_clean$report_classif_confirmed)
#not a case
big_data_clean$report_classif_nac<- grepl(paste(notacase$notacase, collapse="|"),big_data_clean$report_classif, ignore.case = T)
big_data_clean$report_classif_nac<-ifelse(big_data_clean$report_classif_nac==FALSE,NA,big_data_clean$report_classif_nac)
big_data_clean$report_classif_nac<-ifelse(big_data_clean$report_classif_nac==TRUE,"not a case",big_data_clean$report_classif_nac)

big_data_clean$report_classif<- ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_probable), big_data_clean$report_classif_probable,big_data_clean$report_classif)
big_data_clean$report_classif<-ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_suspected), big_data_clean$report_classif_suspected, big_data_clean$report_classif)
big_data_clean$report_classif<-ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_confirmed), big_data_clean$report_classif_confirmed, big_data_clean$report_classif)
big_data_clean$report_classif<-ifelse(!is.na(big_data_clean$report_classif) & !is.na(big_data_clean$report_classif_nac), big_data_clean$report_classif_nac, big_data_clean$report_classif)

#drop columns created, uncommment if needed for debugging
big_data_clean$report_classif_probable<- NULL
big_data_clean$report_classif_suspected<- NULL
big_data_clean$report_classif_confirmed<- NULL
big_data_clean$report_classif_nac<- NULL

report_test<- big_data_clean %>% select(patinfo_id,contains("report"))

big_data_clean$report_classif<-ifelse(!grepl("suspected|probabale|confirmed|not a case",big_data_clean$report_classif, ignore.case = T),NA,big_data_clean$report_classif)

#use patcurrent status in the report classif if missing classification ??


###########################################
#export to send to Franck
openxlsx::write.xlsx(big_data_clean,"inst/PARTIALLYCLEANED_Merged_linelist_2020-12-11.xlsx")
########################################

#labresult
positive<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(positive) %>% na.omit()
negative<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(negative) %>% na.omit()
inconclusive<-rio::import(here::here("inst/","Cleaning_dict_alice.xlsx"), which=2) %>% select(inconclusive) %>% na.omit()
#remove accents from status columns
big_data_clean<-data.table(big_data_clean)
big_data_clean<-big_data_clean[, lab_result := stringi::stri_trans_general(str = lab_result, id = "Latin-ASCII")]
#remove any numbers in there or special characters
big_data_clean<-big_data_clean %>% mutate_at("lab_result",.funs=gsub,pattern="[0-9?]",replacement = NA, ignore.case = T, perl = T)
#positive
big_data_clean$lab_result_pos<- grepl(paste(positive$positive, collapse="|"),big_data_clean$lab_result, ignore.case = T)
big_data_clean$lab_result_pos<-ifelse(big_data_clean$lab_result_pos==FALSE,NA,big_data_clean$lab_result_pos)
big_data_clean$lab_result_pos<-ifelse(big_data_clean$lab_result_pos==TRUE,"positive",big_data_clean$lab_result_pos)
#negative
big_data_clean$lab_result_neg<- grepl(paste(negative$negative, collapse="|"),big_data_clean$lab_result, ignore.case = T)
big_data_clean$lab_result_neg<-ifelse(big_data_clean$lab_result_neg==FALSE,NA,big_data_clean$lab_result_neg)
big_data_clean$lab_result_neg<-ifelse(big_data_clean$lab_result_neg==TRUE,"negative",big_data_clean$lab_result_neg)
#inconclusive
big_data_clean$lab_result_incon<- grepl(paste(inconclusive$inconclusive, collapse="|"),big_data_clean$lab_result, ignore.case = T)
big_data_clean$lab_result_incon<-ifelse(big_data_clean$lab_result_incon==FALSE,NA,big_data_clean$lab_result_incon)
big_data_clean$lab_result_incon<-ifelse(big_data_clean$lab_result_incon==TRUE,"inconclusive",big_data_clean$lab_result_incon)

big_data_clean$lab_result<- ifelse(!is.na(big_data_clean$lab_result) & !is.na(big_data_clean$lab_result_pos), big_data_clean$lab_result_pos,big_data_clean$lab_result)
big_data_clean$lab_result<-ifelse(!is.na(big_data_clean$lab_result) & !is.na(big_data_clean$lab_result_neg), big_data_clean$lab_result_neg, big_data_clean$lab_result)
big_data_clean$lab_result<-ifelse(!is.na(big_data_clean$lab_result) & !is.na(big_data_clean$lab_result_incon), big_data_clean$lab_result_incon, big_data_clean$lab_result)

#drop columns created, uncommment if needed for debugging
big_data_clean$lab_result_pos<- NULL
big_data_clean$lab_result_neg<- NULL
big_data_clean$lab_result_incon<- NULL


openxlsx::write.xlsx(big_data_clean,"inst/merged_linelist_cleaninginprogress.xlsx")





########################################################################################################################


