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
                    axis.text.x=element_text(size=14,angle=45,color="black", vjust=0.5, hjust=0.5),
                    axis.text.y=element_text(size=14,color="black"),
                    axis.title.y=element_text(size=14,face="bold",color="black"),
                    axis.title.x=element_text(size=14,face="bold",color="black"),
                    panel.grid.major.y=element_line(size=.5, color="gray", linetype = "solid"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    #panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill = "white", colour = "grey50"),
                    axis.line=element_line(colour="black"),legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.key.size= unit(0.4, "cm"),
                    legend.title = element_text(size=14,face="bold.italic"),
                    legend.text=element_text(size=14, face="italic"),
                    #panel.border=element_rect(colour="black", fill=NA, size=1),
                    plot.margin=unit(c(0.2,0.2,0.2,0.2),"in"))

#make own colour palettes
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "PRGn"))(nb.cols)


###################################


big_data_clean<-rio::import(here::here("inst/","Cleaned_linelist_2021-01-05.xlsx"), guess_max=100000) #guess max added to ensure integers are not read in as boolean

big_data_analyse<-dplyr::mutate(big_data_clean, across(contains("date"), as.Date, origin= "1899-12-30")) %>%
  filter(report_date<=as.Date("2020-10-30", origin= "1899-12-30"))

#remove countries that dont have up until the 2020-10-31
big_data_analyse<- big_data_analyse %>% group_by(country_iso) %>% mutate(lastreport_oct31 =ifelse(max(report_date)==as.Date("2020-10-30", origin= "1899-12-30"),1,0))
big_data_analyse<- big_data_analyse %>% group_by(country_iso) %>% filter(lastreport_oct31==1)

big_data_analyse$agegroup<- cut(big_data_analyse$patinfo_ageonset, breaks = c(0, 15, 25, 35, 45, 55,65,75,Inf), right = FALSE,labels = c("< 15 years", "15-24 years","25-34 years","35-44 years","45-54 years","55-64 years","65-74 years","75 years +"))



#in GIN the variable for status was is case dead with 1= dead and 0 = alive. ) has been lost in the merging of linelists as was difficult to retain and merge all
#therfore populate patcourse_status if missing for alive only for ginuea
big_data_analyse$patcourse_status<-ifelse(big_data_analyse$country_iso=="GIN" & is.na(big_data_analyse$patcourse_status), "alive", big_data_analyse$patcourse_status)

big_data_analyse$comcond_preexist1_alice<-ifelse(is.na(big_data_analyse$comcond_preexist1_alice), "not reported", big_data_analyse$comcond_preexist1_alice)



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
            ncd_notreport=length(grep("reported",comcond_preexist1_alice, ignore.case = T))) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 0),
         hcwrate= round((hwc_confirmed/(confirmed))*100, digits = 0))


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


ncd<- big_data_analyse %>% group_by(comcond_preexist1) %>%  mutate(hcw_all=ifelse(grepl("confirmed",report_classif_alice, ignore.case = T) & grepl("TRUE", hcw, ignore.case = T),1,0)) %>%
  summarise(confirmed=length(grep("Confirmed",report_classif_alice,ignore.case = T)),
            dead=length(grep("Dead",patcourse_status, ignore.case = T)),
            alive=length(grep("alive",patcourse_status, ignore.case = T)),
            hwc_confirmed=sum(hcw_all, na.rm=T)) %>%
  mutate(CFR=round((dead/confirmed)*100, digits = 0),
         hcwrate= round((hwc_confirmed/(confirmed))*100, digits = 0),
         confirmed_perc=confirmed/sum(confirmed, na.rm = T)*100,
         dead_perc=dead/sum(dead, na.rm = T)*100)



#distribution of cases with age group
  g1<-ggplot(all_sexage,aes(agegroup,confirmed_perc, fill=patinfo_sex)) + geom_bar(stat="identity", width=.5, position = "dodge") +
    pub_theme4 + scale_fill_manual(values = mycolors[c(2,10)]) + labs(y="% of cases", x= "Age group", fill="Sex") +
    geom_text(aes(label=paste0(round(confirmed_perc), "%"), fontface= 2),position = position_dodge(width = 0.6), vjust=-0.2)
  ggsave("whoisddying_manuscript/age_sex_cases.png", width=20, height = 15)
 #distribution of death with age group
  g2<-ggplot(all_sexage,aes(agegroup,dead_perc, fill=patinfo_sex)) + geom_bar(stat="identity", width=.5, position = "dodge") +
    pub_theme4 + scale_fill_manual(values = mycolors[c(2,10)]) + labs(y="% of deaths", x= "Age group", fill="Sex") +
    geom_text(aes(label=paste0(round(dead_perc), "%"), fontface= 2),position = position_dodge(width = 0.6), vjust=-0.2)
  ggsave("whoisddying_manuscript/age_sex_death.png", width=20, height = 15)

  #cfr
  g3<-ggplot(cfr_bysexage,aes(agegroup,CFR,group=patinfo_sex, fill=patinfo_sex)) + geom_point(aes(colour=patinfo_sex, group=patinfo_sex), size=3, show.legend = F) + geom_line(aes(colour=patinfo_sex, group=patinfo_sex))+
    pub_theme4 + scale_colour_manual(values = mycolors[c(2,10)]) + labs(y="CFR (%)", x= "Age group", colour="Sex") +
    geom_text(aes(label=paste0(round(CFR, digits = 1), "%"), colour=patinfo_sex, fontface= 2),position = position_dodge(width = 1), vjust=-0.5)
  ggsave("whoisddying_manuscript/age_sex_cfr.png", width=20, height = 15)

g4<-all %>% filter(comcond_preexist1=="yes" | comcond_preexist1=="no") %>% ggplot(aes(comcond_preexist1,CFR)) + geom_bar(stat = "identity", width=.5) + pub_theme4
