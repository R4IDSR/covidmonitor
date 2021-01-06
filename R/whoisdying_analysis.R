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
                    axis.text.x=element_text(size=20,angle=45,color="black", vjust=0.5, hjust=0.5),
                    axis.text.y=element_text(size=20,color="black"),
                    axis.title.y=element_text(size=22,face="bold",color="black"),
                    axis.title.x=element_text(size=22,face="bold",color="black"),
                    panel.grid.major.y=element_line(size=.5, color="gray", linetype = "solid"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    #panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill = "white", colour = "grey50"),
                    axis.line=element_line(colour="black"),legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.key.size= unit(0.4, "cm"),
                    legend.title = element_text(size=22,face="bold.italic"),
                    legend.text=element_text(size=20, face="italic"),
                    #panel.border=element_rect(colour="black", fill=NA, size=1),
                    plot.margin=unit(c(0.2,0.2,0.2,0.2),"in"))

#make own colour palettes
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "PRGn"))(nb.cols)
mycolors2 <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

###################################


big_data_clean<-rio::import(here::here("inst/","Cleaned_linelist_2021-01-06.xlsx"), guess_max=100000) #guess max added to ensure integers are not read in as boolean

big_data_analyse<-dplyr::mutate(big_data_clean, across(contains("date"), as.Date, origin= "1899-12-30")) %>%
  filter(report_date<=as.Date("2020-10-30", origin= "1899-12-30"))

#remove countries that dont have up until the 2020-10-31
big_data_analyse<- big_data_analyse %>% group_by(country_iso) %>% mutate(lastreport_oct31 =ifelse(max(report_date)==as.Date("2020-10-30", origin= "1899-12-30"),1,0))
big_data_analyse<- big_data_analyse %>% group_by(country_iso) %>% filter(lastreport_oct31==1)

big_data_analyse$agegroup<- cut(big_data_analyse$patinfo_ageonset, breaks = c(0, 15, 25, 35, 45, 55,65,75,Inf), right = FALSE,labels = c("< 15 years", "15-24 years","25-34 years","35-44 years","45-54 years","55-64 years","65-74 years","75 years +"))



#in GIN the variable for status was is case dead with 1= dead and 0 = alive. ) has been lost in the merging of linelists as was difficult to retain and merge all
#therfore populate patcourse_status if missing for alive only for ginuea
big_data_analyse$patcourse_status<-ifelse(big_data_analyse$country_iso=="GIN" & is.na(big_data_analyse$patcourse_status), "alive", big_data_analyse$patcourse_status)

#only include confirmed cases
big_data_analyse<- big_data_analyse %>% filter(report_classif_alice=="confirmed")



timetodeath<-big_data_analyse %>% filter(!is.na(patcourse_dateonset) & !is.na(patcourse_datedeath)) %>% group_by(country_iso,country_full) %>%
  summarise(total=length(patcourse_datedeath))

all<- big_data_analyse %>% mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>%
  group_by(country_iso,country_full) %>%
  summarise(dayslastreport=as.numeric(difftime(Sys.Date(), max(report_date,na.rm = T))),
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
  group_by(country_iso,country_full) %>% summarise(dayslastreport=as.numeric(difftime(Sys.Date(), max(report_date,na.rm = T))),
                                                   datelastreport=format(max(report_date, na.rm = T), "%Y %B %d"),
                                                   confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                   recovered=length(grep("recovered",patcourse_status_recovered,ignore.case = T)),
                                                   dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                   alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
mutate(CFR=round((dead/confirmed)*100, digits = 2))

# remove TCD as this linelist is only dead
cfr_bysexage<-big_data_analyse %>% filter(!is.na(patinfo_ageonset) & !is.na(patinfo_sex) & country_iso!="TCD") %>% group_by(agegroup,patinfo_sex) %>% mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>%
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


#ncd does not include those with no comcond entered ie. UGA and STP
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
diabetes<- big_data_analyse %>% group_by(diabetes) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                 dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(diabetes=ifelse(diabetes==1,"diabetes",NA)) %>% filter(!is.na(diabetes)) %>% rename("co-morbidity"="diabetes")

hta<-big_data_analyse %>% group_by(hypertension) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                               dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                 alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(hypertension=ifelse(hypertension==1,"hypertension",NA)) %>% filter(!is.na(hypertension)) %>% rename("co-morbidity"="hypertension")

asthma<- big_data_analyse %>% group_by(asthma) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                             dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(asthma=ifelse(asthma==1,"asthma",NA)) %>% filter(!is.na(asthma)) %>% rename("co-morbidity"="asthma")

cancer<-big_data_analyse %>% group_by(cancer) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                            alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(cancer=ifelse(cancer==1,"cancer",NA)) %>% filter(!is.na(cancer)) %>% rename("co-morbidity"="cancer")

obesity<-big_data_analyse %>% group_by(obesity) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                              dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                            alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(obesity=ifelse(obesity==1,"obesity",NA)) %>% filter(!is.na(obesity)) %>% rename("co-morbidity"="obesity")

cvd<-big_data_analyse %>% group_by(cardiovascular_disease) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                         dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                              alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(cardiovascular_disease=ifelse(cardiovascular_disease==1,"cardiovascular disease",NA)) %>% filter(!is.na(cardiovascular_disease)) %>% rename("co-morbidity"="cardiovascular_disease")

rd<-big_data_analyse %>% group_by(renal_disease) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                               dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                         alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(renal_disease=ifelse(renal_disease==1,"renal disease",NA)) %>% filter(!is.na(renal_disease)) %>% rename("co-morbidity"="renal_disease")

tb<-big_data_analyse %>% group_by(tuberculosis) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                              dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                               alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(tuberculosis=ifelse(tuberculosis==1,"tuberculosis",NA)) %>% filter(!is.na(tuberculosis)) %>% rename("co-morbidity"="tuberculosis")

pregnancy<-big_data_analyse %>% group_by(pregnancy) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                  dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                         alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(pregnancy=ifelse(pregnancy==1,"pregnancy",NA)) %>% filter(!is.na(pregnancy)) %>% rename("co-morbidity"="pregnancy")

drepanocytosis<-big_data_analyse %>% group_by(drepanocytosis) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                          alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(drepanocytosis=ifelse(drepanocytosis==1,"drepanocytosis",NA)) %>% filter(!is.na(drepanocytosis)) %>% rename("co-morbidity"="drepanocytosis")

pd<-big_data_analyse %>% group_by(chronic_pulmonary) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                   dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(chronic_pulmonary=ifelse(chronic_pulmonary==1,"chronic pulmonary disease",NA)) %>% filter(!is.na(chronic_pulmonary)) %>% rename("co-morbidity"="chronic_pulmonary")

not_spec<-big_data_analyse %>% group_by(not_specified_comorb) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                   alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(not_specified_comorb=ifelse(not_specified_comorb==1,"not specified",NA)) %>% filter(!is.na(not_specified_comorb)) %>% rename("co-morbidity"="not_specified_comorb")

other_comorb<-big_data_analyse %>% group_by(other_comorb) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                                        dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                                          alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(other_comorb=ifelse(other_comorb==1,"other",NA)) %>% filter(!is.na(other_comorb)) %>% rename("co-morbidity"="other_comorb")

ncd_type<-do.call("rbind",list(diabetes,hta,asthma,cvd,cancer, obesity,tb,pd,rd,drepanocytosis,pregnancy,not_spec,other_comorb)) %>% group_by(`co-morbidity`) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2))


big_data_analyse<-dplyr::mutate(big_data_analyse, across(c(diabetes, asthma, hypertension,cancer,
                                                            renal_disease,cardiovascular_disease,obesity,tuberculosis,drepanocytosis,chronic_pulmonary), as.numeric))

big_data_analyse$comcond_preexsist_count_top10<-rowSums(select(ungroup(big_data_analyse), c(diabetes, asthma, hypertension,cancer,
                                                                             renal_disease,cardiovascular_disease,obesity,tuberculosis,drepanocytosis,chronic_pulmonary)), na.rm = T)

ncd_oneormore<-big_data_analyse %>% filter(comcond_preexist1_alice=="yes") %>% group_by(as.factor(comcond_preexsist_count_top10)) %>% summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
                                                             dead=length(grep("Dead",patcourse_status, ignore.case = T)),
                                                             alive=length(grep("alive",patcourse_status, ignore.case = T))) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 2))

#looking at asthma diabetes hta only and comparing to if having singlar vs that +one other ?




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
  cfr_labels<- ncd_type_long %>% select(c("co-morbidity","CFR")) %>% unique(.)

  g5<-ncd_type_long %>% ggplot(aes(factor(`co-morbidity`,levels=`co-morbidity`[`dead/alive`=="dead"][order(value[`dead/alive`=="dead"])]), value, fill=`dead/alive`, group=`dead/alive`)) + geom_bar(aes(fill=as.factor(`dead/alive`)),stat="identity", show.legend = T) +
    pub_theme4 + theme(axis.text.x=element_text(angle=0)) + scale_fill_manual(values = mycolors2[c(3,1)]) +
    coord_flip() + labs(y="Number of cases", x= "Co-morbidity", fill="Outcome") +   geom_text(data=cfr_labels,aes(x=`co-morbidity`,y=1400,label=paste0("CFR= ",round(CFR, digits = 1), "%"),fontface= 2),
                                                                                              inherit.aes=FALSE, size=8,angle=0,hjust=1)

  ggsave("whoisddying_manuscript/ncd_type_barplot.png", width=20, height = 15)


  #health care workers CFR
 hcw_long<- hcw %>%  tidyr::pivot_longer(c(dead,alive), names_to= "dead/alive")
 cfr_hcw_labels<- hcw_long %>% select(c("country_iso","CFR")) %>% unique(.)

  g6<-hcw_long %>% ggplot(aes(factor(country_iso,levels=country_iso[`dead/alive`=="dead"][order(value[`dead/alive`=="dead"])]), value, fill=`dead/alive`, group=`dead/alive`)) + geom_bar(stat="identity", width=.5, position = "stack") +
    pub_theme4 + theme(axis.text.x=element_text(angle=0)) + scale_fill_manual(values = mycolors2[c(3,1)]) + labs(y="Number of cases", x= "Country", fill="Outcome")

  ggsave("whoisddying_manuscript/hcw_cfr_barplot.png", width=20, height = 15)




