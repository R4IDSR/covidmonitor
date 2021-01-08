library(lubridate)
library(dplyr)
library(here)
library(rio)
library(utils)
library(knitr)
library(kableExtra)
library(gtable)
library(scales)
library(purrr)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(magrittr)
library(flextable)
library(rmarkdown)
library(linelist)

##################################
#Initialise Theme for plotting
pub_theme4 <- theme(text=element_text(family="Arial"),
                    axis.text.x=element_text(size=24,angle=45,color="black", vjust=0.5, hjust=0.5),
                    axis.text.y=element_text(size=24,color="black"),
                    axis.title.y=element_text(size=26,face="bold",color="black"),
                    axis.title.x=element_text(size=26,face="bold",color="black"),
                    panel.grid.major.y=element_line(size=.5, color="gray", linetype = "solid"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    #panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill = "white", colour = "grey50"),
                    axis.line=element_line(colour="black"),legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.key.size= unit(0.4, "cm"),
                    legend.title = element_text(size=26,face="bold.italic"),
                    legend.text=element_text(size=24, face="italic"),
                    #panel.border=element_rect(colour="black", fill=NA, size=1),
                    plot.margin=unit(c(0.2,0.2,0.2,0.2),"in"))

#make own colour palettes
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "PRGn"))(nb.cols)
mycolors2 <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

###################################


big_data_clean<-rio::import(here::here("inst/","Cleaned_linelist_2021-01-06.xlsx"), guess_max=100000) #guess max added to ensure integers are not read in as boolean
big_data_analyse<-big_data_clean
###########Filtering the data set ###########

#define date we are reporting to
dateofreport<-as.Date("2020-10-29", origin= "1899-12-30")
#filter for cases before dateofreport variable ie 29/10/2020

big_data_analyse<-dplyr::mutate(big_data_analyse, across(c("patcourse_datedeath","patcourse_datedischarge"), as.Date, origin= "1970-01-01")) %>%
  filter(report_date<=dateofreport)

#in GIN the variable for status was is case dead with 1= dead and 0 = alive. ) has been lost in the merging of linelists as was difficult to retain and merge all
#therfore populate patcourse_status if missing for alive only for ginuea
big_data_analyse$patcourse_status<-ifelse(big_data_analyse$country_iso=="GIN" & is.na(big_data_analyse$patcourse_status), "alive", big_data_analyse$patcourse_status)

#only include confirmed cases that are not missing outcome
big_data_analyse<- big_data_analyse %>% filter(report_classif_alice=="confirmed" & !is.na(patcourse_status))




#remove countries that do not have reports up until 2020-10-31
big_data_analyse<- big_data_analyse %>% group_by(country_iso) %>% mutate(lastreport_ =ifelse(max(report_date, na.rm = T)==dateofreport,1,0))
big_data_analyse<- big_data_analyse %>% group_by(country_iso) %>% filter(lastreport_==1)
############################################
###########Variable creation###########
big_data_analyse$agegroup<- cut(big_data_analyse$patinfo_ageonset, breaks = c(0, 15, 25, 35, 45, 55,65,75,Inf), right = FALSE,labels = c("< 15 years", "15-24 years","25-34 years","35-44 years","45-54 years","55-64 years","65-74 years","75 years +"))
#correcting if any "men" with pregancy entered into their comorbs
big_data_analyse$pregnancy<-ifelse(big_data_analyse$patinfo_sex=="M" & big_data_analyse$pregnancy==1,NA,big_data_analyse$pregnancy)

#ensuring only those not missing have this variable corrrect
big_data_analyse$capital_final<-ifelse(is.na(big_data_analyse$patinfo_resadmin1), NA, big_data_analyse$capital_final)

big_data_analyse<-dplyr::mutate(big_data_analyse, across(c(diabetes, asthma, hypertension,cancer,
                                                           renal_disease,cardiovascular_disease,obesity,tuberculosis,drepanocytosis,chronic_pulmonary), as.numeric))

big_data_analyse$comcond_preexsist_count_top10<-rowSums(select(ungroup(big_data_analyse), c(diabetes, asthma, hypertension,cancer,
                                                                                            renal_disease,cardiovascular_disease,obesity,tuberculosis,drepanocytosis,chronic_pulmonary)), na.rm = T)



###########################
#time to death analysis need to make missing problem dates entered in wrong for lab taken date date of death and date of date discharge
timetodeath<-ungroup(big_data_analyse)
timetodeath$lab_datetaken <- if_else(timetodeath$lab_datetaken< as.Date("2020-01-01") | timetodeath$lab_datetaken> as.Date(dateofreport),as.Date(NA),as.Date(timetodeath$lab_datetaken))
timetodeath$patcourse_datedeath <- if_else(timetodeath$patcourse_datedeath< as.Date("2020-01-01") | timetodeath$patcourse_datedeath> as.Date(dateofreport),as.Date(NA),as.Date(timetodeath$patcourse_datedeath))
timetodeath$patcourse_datedischarge <- if_else(timetodeath$patcourse_datedischarge< as.Date("2020-01-01") | timetodeath$patcourse_datedischarge> as.Date(dateofreport),as.Date(NA),as.Date(timetodeath$patcourse_datedischarge))
timetodeath$lab_resdate<-if_else(timetodeath$lab_resdate< as.Date("2020-01-01") | timetodeath$lab_resdate> as.Date(dateofreport),as.Date(NA),as.Date(timetodeath$lab_resdate))

#use this df in the time to death analysis
timetodeath_numbers<-timetodeath %>% filter(!is.na(lab_resdate) & !is.na(patcourse_status))
timetodeath_numbers$startdate<-timetodeath_numbers$lab_resdate
timetodeath_numbers$enddate<-dplyr::if_else(timetodeath_numbers$patcourse_status=="dead",timetodeath_numbers$patcourse_datedeath,timetodeath_numbers$patcourse_datedischarge)
timetodeath_numbers$enddate<-dplyr::if_else(timetodeath_numbers$patcourse_status=="alive" & is.na(timetodeath_numbers$enddate),dateofreport,timetodeath_numbers$enddate)
timetodeath_numbers<-timetodeath_numbers %>% filter(startdate<=dateofreport & enddate<=dateofreport)
timetodeath_numbers$comcond_preexist1_alice_binary<-ifelse(!is.na(timetodeath_numbers$comcond_preexist1_alice) & timetodeath_numbers$comcond_preexist1_alice=="yes",1,
                                                           ifelse(is.na(timetodeath_numbers$comcond_preexist1_alice),NA,0)) #this would still include UGA and STP in this as makes all not given=0
timetodeath_numbers$hcw_binary<-ifelse(!is.na(timetodeath_numbers$hcw) & timetodeath_numbers$hcw=="TRUE",1,0)
timetodeath_numbers$capital_final_binary<-ifelse(!is.na(timetodeath_numbers$capital_final) & timetodeath_numbers$capital_final=="TRUE",1,
                                                 ifelse(is.na(timetodeath_numbers$capital_final),NA,0))
timetodeath_numbers$pregant_binary<-ifelse(is.na(timetodeath_numbers$pregnancy) & timetodeath_numbers$patinfo_sex=="F",0,timetodeath_numbers$pregnancy)
timetodeath_numbers$patinfo_sex_binary<-ifelse(!is.na(timetodeath_numbers$patinfo_sex) & timetodeath_numbers$patinfo_sex=="M",1,
                                               ifelse(is.na(timetodeath_numbers$patinfo_sex),NA,0))

timetodeath_numbers$patinfo_age_binary<-ifelse(!is.na(timetodeath_numbers$patinfo_ageonset) & timetodeath_numbers$patinfo_ageonset>=60,1,
                                               ifelse(is.na(timetodeath_numbers$patinfo_ageonset),NA,0))


#define failure
timetodeath_numbers$vlfail<-ifelse(timetodeath_numbers$patcourse_status=="dead",1,0)
#Create person time variable (observation time)
timetodeath_numbers$perstime <- as.numeric(difftime(timetodeath_numbers$enddate, timetodeath_numbers$startdate, units = "days")) / 365.25

timetodeath_numbers<- timetodeath_numbers %>% filter(timetodeath_numbers$perstime>0)
#Add the survial object as variable to your dataset which is survival time (for kaplan-meier and cox)
timetodeath_numbers$SurvObj <- with(timetodeath_numbers, survival::Surv(time = perstime, event = vlfail))


#################
#dataset prep for lrm

fit<- big_data_analyse %>% filter(!is.na(patcourse_status))

fit$comcond_preexist1_alice_binary<-ifelse(!is.na(fit$comcond_preexist1_alice) & fit$comcond_preexist1_alice=="yes",1,
                                                           ifelse(is.na(fit$comcond_preexist1_alice),NA,0)) #this would still include UGA and STP in this as makes all not given=0
fit$hcw_binary<-ifelse(!is.na(fit$hcw) & fit$hcw=="TRUE",1,0)
fit$capital_final_binary<-ifelse(!is.na(fit$capital_final) & fit$capital_final=="TRUE",1,
                                                 ifelse(is.na(fit$capital_final),NA,0))
fit$pregant_binary<-ifelse(is.na(fit$pregnancy) & fit$patinfo_sex=="F",0,fit$pregnancy)
fit$patinfo_sex_binary<-ifelse(!is.na(fit$patinfo_sex) & fit$patinfo_sex=="M",1,
                                               ifelse(is.na(fit$patinfo_sex),NA,0))

#define failure
fit$vlfail<-ifelse(fit$patcourse_status=="dead",1,0)



#######################


all<- big_data_analyse %>% mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>%
  group_by(country_iso,country_full) %>%
  summarise(dayslastreport=as.numeric(difftime(as.Date("2020-10-29", origin= "1899-12-30"), max(report_date,na.rm = T))),
            datelastreport=format(max(report_date, na.rm = T), "%Y %B %d"),
            confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
            recovered=length(grep("recovered",patcourse_status_recovered,ignore.case = T)),
            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
            alive=length(grep("alive",patcourse_status, ignore.case = T)),
            hwc_confirmed=sum(hcw_all, na.rm=T),
            ncd_yes=length(grep("yes",comcond_preexist1_alice, ignore.case = T)),
            ncd_no=length(grep("^no$",comcond_preexist1_alice, ignore.case = T)),
            ncd_notreport=length(grep("not given",comcond_preexist1_alice, ignore.case = T))) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 0),
         hcwrate= round((hwc_confirmed/(confirmed))*100, digits = 0))
openxlsx::write.xlsx(all,"whoisddying_manuscript/summary_confirmedcases.xlsx")

hcw<-big_data_analyse %>% mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>% filter(hcw_all==1) %>%
  group_by(country_iso,country_full) %>% summarise(dayslastreport=as.numeric(difftime(as.Date("2020-10-30", origin= "1899-12-30"), max(report_date,na.rm = T))),
                                                   datelastreport=format(max(report_date, na.rm = T), "%Y %B %d"),
                                                   confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                   recovered=length(grep("recovered",patcourse_status_recovered,ignore.case = T)),
                                                   dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                   alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
mutate(CFR=round((dead/confirmed)*100, digits = 2))


cfr_bysexage<-big_data_analyse %>% filter(!is.na(patinfo_ageonset) & !is.na(patinfo_sex)) %>% group_by(agegroup,patinfo_sex) %>% mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>%
  summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
            alive=length(grep("alive",patcourse_status, ignore.case = T)),
            hwc_confirmed=sum(hcw_all, na.rm=T)) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2))


all_sexage<-big_data_analyse %>%  filter(!is.na(patinfo_ageonset) & !is.na(patinfo_sex)) %>% mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>%
  group_by(patinfo_sex,agegroup) %>%
  summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
            #total_reported=length(!is.na(patinfo_id)), #everyone in the line list?
            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
            alive=length(grep("alive",patcourse_status, ignore.case = T)),
            hwc_confirmed=sum(hcw_all, na.rm=T)) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 0),
         hcwrate= round((hwc_confirmed/(confirmed))*100, digits = 0),
         confirmed_perc=confirmed/sum(confirmed, na.rm = T)*100,
         dead_perc=dead/sum(dead, na.rm = T)*100)

all_sexage_wide<-tidyr::pivot_wider(all_sexage,agegroup,names_from= patinfo_sex,values_from=c(confirmed,alive,dead,hwc_confirmed,CFR,hcwrate))

summary_sexage_wide<-tidyr::pivot_wider(all_sexage,agegroup,names_from= patinfo_sex,values_from=confirmed) %>% mutate(male_perc=(`M`/(`M`+`F`))*100 , agegroup_perc=((`M`+`F`)/sum(`M`+`F`, na.rm = T))*100)

summary_sexage_dead_wide<-tidyr::pivot_wider(all_sexage,agegroup,names_from= patinfo_sex,values_from=dead) %>% mutate(male_perc=(`M`/(`M`+`F`))*100 , agegroup_perc=((`M`+`F`)/sum(`M`+`F`, na.rm = T))*100)


#ncd does not include those with no comcond entered ie. UGA
ncd<- big_data_analyse %>% group_by(country_iso) %>% filter(length(unique(as.list(comcond_preexist1_alice)))>1) %>%
  group_by(country_iso,comcond_preexist1_alice) %>%  mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>%
  summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
            alive=length(grep("alive",patcourse_status, ignore.case = T)),
            hwc_confirmed=sum(hcw_all, na.rm=T))

ncd_no_notreported<- ncd %>% group_by(country_iso) %>% dplyr::summarise(
  confirmed=sum(confirmed[comcond_preexist1_alice=='no' | comcond_preexist1_alice=='not given']),
  dead=sum(dead[comcond_preexist1_alice=='no' | comcond_preexist1_alice=='not given']),
  alive=sum(alive[comcond_preexist1_alice=='no' | comcond_preexist1_alice=='not given']),
  hwc_confirmed=sum(hwc_confirmed[comcond_preexist1_alice=='no' | comcond_preexist1_alice=='not given']))

ncd_no_notreported$comcond_preexist1_alice<-"no/not reported"

ncd_new<-merge(ncd,ncd_no_notreported, all=T) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2),
         hcwrate= round((hwc_confirmed/(confirmed))*100, digits = 0),
         confirmed_perc=confirmed/sum(confirmed, na.rm = T)*100,
         dead_perc=dead/sum(dead, na.rm = T)*100)

ncd_all<- ncd_new %>% group_by(comcond_preexist1_alice) %>%
  summarise(confirmed=sum(confirmed),
            dead=sum(dead),
            alive=sum(alive),
            hwc_confirmed=sum(hwc_confirmed)) %>% group_by(comcond_preexist1_alice) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2),
         hcwrate= round((hwc_confirmed/(confirmed))*100, digits = 2))

#chi squared analysis for dead vs alive in confirmed cases between comcond groups (yes vs. no/ not recorded)
ncd_all_chi<- big_data_analyse %>% filter(length(unique(as.list(comcond_preexist1_alice)))>1) %>% select(id,patinfo_id,country_iso,comcond_preexist1_alice, patcourse_status, report_classif_alice) %>%
  mutate(comcond_preexist1_alice=ifelse(comcond_preexist1_alice!="yes", "no/not reported",comcond_preexist1_alice))
unique(ncd_all_chi$country_iso) #coutrnies in ncd analysis

ncd_all_chi_summary<-ncd_all_chi %>% group_by(comcond_preexist1_alice) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
            alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2))

chisq.test(ncd_all_chi$comcond_preexist1_alice,ncd_all_chi$patcourse_status)
table(ncd_all_chi$comcond_preexist1_alice,ncd_all_chi$patcourse_status)

######

#ncds by type

ncd_type_count<-big_data_analyse %>% group_by(country_iso) %>% filter(length(unique(as.list(comcond_preexist1_alice)))>1)

diabetes<- ncd_type_count %>% group_by(diabetes) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                 dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(diabetes=ifelse(diabetes==1,"diabetes",NA)) %>% filter(!is.na(diabetes)) %>% rename("co-morbidity"="diabetes")

hta<-ncd_type_count %>% group_by(hypertension) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                               dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                 alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(hypertension=ifelse(hypertension==1,"hypertension",NA)) %>% filter(!is.na(hypertension)) %>% rename("co-morbidity"="hypertension")

asthma<- ncd_type_count %>% group_by(asthma) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                             dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(asthma=ifelse(asthma==1,"asthma",NA)) %>% filter(!is.na(asthma)) %>% rename("co-morbidity"="asthma")

cancer<-ncd_type_count %>% group_by(cancer) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                            alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(cancer=ifelse(cancer==1,"cancer",NA)) %>% filter(!is.na(cancer)) %>% rename("co-morbidity"="cancer")

obesity<-ncd_type_count %>% group_by(obesity) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                              dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                            alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(obesity=ifelse(obesity==1,"obesity",NA)) %>% filter(!is.na(obesity)) %>% rename("co-morbidity"="obesity")

cvd<-ncd_type_count %>% group_by(cardiovascular_disease) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                         dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                              alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(cardiovascular_disease=ifelse(cardiovascular_disease==1,"cardiovascular disease",NA)) %>% filter(!is.na(cardiovascular_disease)) %>% rename("co-morbidity"="cardiovascular_disease")

rd<-ncd_type_count %>% group_by(renal_disease) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                               dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                         alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(renal_disease=ifelse(renal_disease==1,"renal disease",NA)) %>% filter(!is.na(renal_disease)) %>% rename("co-morbidity"="renal_disease")

tb<-ncd_type_count %>% group_by(tuberculosis) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                              dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                               alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(tuberculosis=ifelse(tuberculosis==1,"tuberculosis",NA)) %>% filter(!is.na(tuberculosis)) %>% rename("co-morbidity"="tuberculosis")

pregnancy<-ncd_type_count %>% group_by(pregnancy) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                  dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                         alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(pregnancy=ifelse(pregnancy==1,"pregnancy",NA)) %>% filter(!is.na(pregnancy)) %>% rename("co-morbidity"="pregnancy")

drepanocytosis<-ncd_type_count %>% group_by(drepanocytosis) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                          alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(drepanocytosis=ifelse(drepanocytosis==1,"drepanocytosis",NA)) %>% filter(!is.na(drepanocytosis)) %>% rename("co-morbidity"="drepanocytosis")

pd<-ncd_type_count %>% group_by(chronic_pulmonary) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                   dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(chronic_pulmonary=ifelse(chronic_pulmonary==1,"chronic pulmonary disease",NA)) %>% filter(!is.na(chronic_pulmonary)) %>% rename("co-morbidity"="chronic_pulmonary")

not_spec<-ncd_type_count %>% group_by(not_specified_comorb) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                   alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(not_specified_comorb=ifelse(not_specified_comorb==1,"not specified",NA)) %>% filter(!is.na(not_specified_comorb)) %>% rename("co-morbidity"="not_specified_comorb")

other_comorb<-ncd_type_count %>% group_by(other_comorb) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                        dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                          alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(other_comorb=ifelse(other_comorb==1,"other",NA)) %>% filter(!is.na(other_comorb)) %>% rename("co-morbidity"="other_comorb")

ncd_type<-do.call("rbind",list(diabetes,hta,asthma,cvd,cancer, obesity,tb,pd,rd,drepanocytosis,pregnancy,not_spec,other_comorb)) %>% group_by(`co-morbidity`) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2))

openxlsx::write.xlsx(ncd_type, "whoisddying_manuscript/ncd_type_table.xlsx")


ncd_oneormore<-big_data_analyse %>% filter(comcond_preexist1_alice=="yes") %>% group_by(as.factor(comcond_preexsist_count_top10)) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                             dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                             alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2))


#plotting

#distribution of cases with age group
  g1<-ggplot(all_sexage,aes(agegroup,confirmed_perc, fill=patinfo_sex)) + geom_bar(stat="identity", width=.5, position = "dodge") +
    pub_theme4 + scale_fill_manual(values = mycolors[c(2,10)]) + labs(y="% of cases", x= "Age group", fill="Sex") +
    geom_text(aes(label=paste0(round(confirmed_perc), "%"), fontface= 2), size=8, position = position_dodge(width = 0.6), vjust=-0.2)
  ggsave("whoisddying_manuscript/age_sex_cases.png", width=20, height = 15)
 #distribution of death with age group
  g2<-ggplot(all_sexage,aes(agegroup,dead_perc, fill=patinfo_sex)) + geom_bar(stat="identity", width=.5, position = "dodge") +
    pub_theme4 + scale_fill_manual(values = mycolors[c(2,10)]) + labs(y="% of deaths", x= "Age group", fill="Sex") +
    geom_text(aes(label=paste0(round(dead_perc), "%"), fontface= 2),size=8,position = position_dodge(width = 0.6), vjust=-0.2)
  ggsave("whoisddying_manuscript/age_sex_death.png", width=20, height = 15)

  #cfr
  g3<-ggplot(cfr_bysexage,aes(agegroup,CFR,group=patinfo_sex, fill=patinfo_sex)) + geom_point(aes(colour=patinfo_sex, group=patinfo_sex), size=3, show.legend = F) + geom_line(aes(colour=patinfo_sex, group=patinfo_sex))+
    pub_theme4 + scale_colour_manual(values = mycolors[c(2,10)]) + labs(y="CFR (%)", x= "Age group", colour="Sex") +
    geom_text(aes(label=paste0(round(CFR, digits = 1), "%"), colour=patinfo_sex, fontface= 2), size=8,position = position_dodge(width = 1), vjust=-0.5)
  ggsave("whoisddying_manuscript/age_sex_cfr.png", width=20, height = 15)

  #ncd and CFR boxplot which shows median IQR of all the counries in this analysis
  g4<-ncd_new %>% filter(comcond_preexist1_alice=="no/not reported" |comcond_preexist1_alice=="yes") %>%
    ggplot(aes(comcond_preexist1_alice, CFR)) + geom_boxplot(aes(fill=as.factor(comcond_preexist1_alice)), show.legend = F) +
    pub_theme4 + theme(axis.text.x=element_text(angle=0)) + labs(y="CFR (%)", x= "Co-morbidity presence") + scale_fill_manual(values = mycolors2[c(1,3)])
  ggsave("whoisddying_manuscript/ncd_cfr_countriesmedian_boxplot.png", width=20, height = 15)

  g5<-ncd_all_chi_summary %>% ggplot(aes(comcond_preexist1_alice, CFR)) + geom_bar(aes(fill=as.factor(comcond_preexist1_alice)),stat="identity", show.legend = F) +
    pub_theme4 + theme(axis.text.x=element_text(angle=0)) +  labs(y="CFR (%)", x= "Co-morbidity presence") + scale_fill_manual(values = mycolors2[c(1,3)])
  ggsave("whoisddying_manuscript/ncd_cfr_alltogether_barplot.png", width=20, height = 15)


  #ncd type with CFR
  ncd_type_long<- ncd_type %>% tidyr::pivot_longer(c(dead,alive), names_to= "dead/alive")
  ncd_type_long$`co-morbidity`<-tools::toTitleCase(ncd_type_long$`co-morbidity`)

  cfr_labels<- ncd_type_long %>% select(c("co-morbidity","CFR")) %>% unique(.)

  g6<-ncd_type_long %>% ggplot(aes(factor(`co-morbidity`,levels=`co-morbidity`[`dead/alive`=="dead"][order(value[`dead/alive`=="dead"])]), value, fill=`dead/alive`, group=`dead/alive`)) + geom_bar(aes(fill=as.factor(`dead/alive`)),stat="identity", show.legend = T) +
    pub_theme4 + theme(axis.text.x=element_text(angle=0)) + scale_fill_manual(values = mycolors2[c(3,1)]) +
    coord_flip() + labs(y="Number of cases", x= "Condition", fill="Outcome") +   geom_text(data=cfr_labels,aes(x=`co-morbidity`,y=1400,label=paste0("CFR= ",round(CFR, digits = 1), "%"),fontface= 2),
                                                                                              inherit.aes=FALSE, size=8,angle=0,hjust=1)

  ggsave("whoisddying_manuscript/ncd_type_barplot.png", width=20, height = 15)


  #health care workers CFR
 hcw_long<- hcw %>%  tidyr::pivot_longer(c(dead,alive), names_to= "dead/alive")
 cfr_hcw_labels<- hcw_long %>% select(c("country_iso","CFR")) %>% unique(.)

  g7<-hcw_long %>% ggplot(aes(factor(country_iso,levels=country_iso[`dead/alive`=="dead"][order(value[`dead/alive`=="dead"])]), value, fill=`dead/alive`, group=`dead/alive`)) + geom_bar(stat="identity", width=.5, position = "stack") +
    pub_theme4 + theme(axis.text.x=element_text(angle=0)) + scale_fill_manual(values = mycolors2[c(3,1)]) + labs(y="Number of cases", x= "Country", fill="Outcome")

  ggsave("whoisddying_manuscript/hcw_cfr_barplot.png", width=20, height = 15)



###### cox regression time ot death
#dataset used in failure
  failure<-timetodeath_numbers %>% select(patinfo_sex_binary, patinfo_ageonset, hcw_binary,capital_final_binary,comcond_preexist1_alice_binary, pregant_binary,SurvObj)
  flextable::save_as_docx(gtsummary::as_flex_table(gtsummary::tbl_uvregression(failure,survival::coxph,y=SurvObj, exponentiate = T)),path="whoisddying_manuscript/HR.docx")

  failure_2<-timetodeath_numbers %>% select(patinfo_sex_binary, patinfo_age_binary, hcw_binary,capital_final_binary,comcond_preexist1_alice_binary, vlfail, perstime) %>%
    filter(!is.na(vlfail) & !is.na(patinfo_age_binary) & !is.na(patinfo_sex_binary) & !is.na(comcond_preexist1_alice_binary) & !is.na(capital_final_binary) & !is.na(hcw_binary))
  openxlsx::write.xlsx(failure_2, "failure2.xlsx")

  cox <-coxph(Surv(perstime,vlfail)~ patinfo_sex_binary + patinfo_age_binary+  hcw_binary + capital_final_binary + comcond_preexist1_alice_binary, data = failure_2)
  summary(cox)

 tbl_survfit_ex3 <-
   list(survfit(Surv(perstime,vlfail) ~ 1, failure_2),
        survfit(Surv(perstime,vlfail) ~ patinfo_sex_binary, failure_2),
        survfit(Surv(perstime,vlfail) ~ patinfo_age_binary, failure_2),
        survfit(Surv(perstime,vlfail) ~ hcw_binary, failure_2),
        survfit(Surv(perstime,vlfail) ~ capital_final_binary, failure_2),
        survfit(Surv(perstime,vlfail) ~ comcond_preexist1_alice_binary, failure_2)) %>%
   gtsummary::tbl_survfit(probs = c(0.5))


 survminer::ggsurvplot(
   fit = survfit(Surv(perstime,vlfail) ~ patinfo_sex_binary, failure_2),
   xlab = "time",
   ylab = "Overall survival probability")





###glm model
  library(rms)
  library(foreign)
  library(pROC)
  library(ResourceSelection)
  library(sjPlot)
  library(sjlabelled)
  library(sjmisc)
  library(ggplot2)


#model fit on cases with complete variables
  fit.dat<- fit %>% filter(!is.na(vlfail) & !is.na(patinfo_ageonset) & !is.na(patinfo_sex_binary) & !is.na(comcond_preexist1_alice_binary) & !is.na(capital_final_binary) & !is.na(hcw_binary))
  dd <- datadist(fit.dat)
  options(datadist='dd')

  set_label(fit.dat$vlfail) <- "Mortality"
  set_label(fit.dat$patinfo_ageonset) <- "Age"
  set_label(fit.dat$patinfo_sex_binary) <- "Sex (Male)"
  set_label(fit.dat$comcond_preexist1_alice_binary) <- "Presence of comorbidity"
  set_label(fit.dat$capital_final_binary) <- "Residence in capital city"
  set_label(fit.dat$hcw_binary) <- "Health Care Worker"


  #considering all of these variables, age can be continous
  fit.model<-lrm(vlfail~patinfo_ageonset + patinfo_sex_binary +comcond_preexist1_alice_binary +capital_final_binary+ hcw_binary, data=fit.dat, x=T, y=T)
  summary(fit.model)

  #need a glm oject in gtsummary same as above just allows tabulation
  fit.model.2 <- glm(vlfail~patinfo_ageonset + patinfo_sex_binary +comcond_preexist1_alice_binary +capital_final_binary+ hcw_binary, data=fit.dat, family = binomial)
  summary(fit.model.2)
  flextable::save_as_docx(gtsummary::as_flex_table(gtsummary::tbl_regression(fit.model.2,exponentiate = T)),path="whoisddying_manuscript/OR_forlrm.docx")


  # backward selection moethod with a p-value of 0.157
  fastbw(fit.model, type="individual", rule="p", sls=0.157)
  #no factors deleted

  # store the predicted probabilities by using the predict function
  predict.fit <- predict(fit.model, type="fitted")
  myROC <- roc(fit.model$y, predict.fit, pl=T, ci=TRUE)
  myROC
  hoslem.test(fit.model$y, predict.fit)

  g8<-plot_model(fit.model, colour= mycolours2,value.size = 9,
             value.offset = 0.3,
             dot.size = 5,
             line.size = 3,
             sort.est = T, vline.color = "black", show.values = T) + pub_theme4
  ggsave("whoisddying_manuscript/lrm_forrest.png",width=20, height = 15)

