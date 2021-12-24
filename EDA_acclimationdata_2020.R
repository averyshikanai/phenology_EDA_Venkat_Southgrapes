
#load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(car)
library(survival)
library(survminer)
#read in data frames
pheno_2020<-read.csv("D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\pheno_grape2020.csv",
                     colClasses = c(rep("factor",4),rep("numeric",27)), check.names = F)

pheno_2021<-read.csv("D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\pheno_grape2021.csv",
                     colClasses = c(rep("factor",4),rep("numeric",22)), check.names = F)


merged_df<-merge(pheno_2020,pheno_2021, all = T)
longpheno<-pivot_longer(merged_df, cols=!c(1,2,3,4), names_to = "yyyymmdd", values_to = "bbch")

longpheno<-dplyr::mutate(longpheno, DOY=yday(yyyymmdd))


##filter out some typos
longpheno<-filter(longpheno, bbch<100)

longpheno<-unite(longpheno, ID, c(Vine,yyyymmdd), remove=F)

##convert to a wide format. each row is a vine.1 2 3 4 are observations of the vine

widephenophase<-pivot_wider(data=longpheno, id_cols = c(ID,Male,Female,yyyymmdd,DOY,Vine), names_from=Observation_no, values_from=bbch)
widephenophase<- apply(widephenophase,2,as.character)
widephenophase<-as.data.frame(widephenophase)
widephenophase<-widephenophase[,-11]

widephenophase[widephenophase == "NULL"] = NA
widephenophase$'1'<-as.numeric(widephenophase$'1')
widephenophase$'2'<-as.numeric(widephenophase$'2')
widephenophase$'3'<-as.numeric(widephenophase$'3')
widephenophase$'4'<-as.numeric(widephenophase$'4')


write.csv(widephenophase,"D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\widephenophase.csv")




###get T or F booleans for different BBCH ranges
for(i in 1:length(widephenophase$ID)) {
  ifelse((widephenophase[i,7]>=10| 
           widephenophase[i,8]>=10| 
           widephenophase[i,9]>=10 | 
           widephenophase[i,10]>=10 ), 
  widephenophase[i,11]<-T,
  widephenophase[i,11]<-F)
}

widephenophase<-dplyr::rename(widephenophase, BBCH10="V11") 
  

for(i in 1:length(widephenophase$ID)) {
  ifelse( ((10<=widephenophase[i,7] &  widephenophase[i,7]<=50)| 
            (10<=widephenophase[i,8] &  widephenophase[i,8]<=50)| 
            (10<=widephenophase[i,9] &  widephenophase[i,9]<=50) | 
            (10<=widephenophase[i,10] &  widephenophase[i,10]<=50)), 
         widephenophase[i,12]<-T,
         widephenophase[i,12]<-F)
}
widephenophase<-dplyr::rename(widephenophase, BBCH10_50="V12") 

for(i in 1:length(widephenophase$ID)) {
  ifelse( ((50<=widephenophase[i,7] &  widephenophase[i,7]<=59)| 
             (50<=widephenophase[i,8] &  widephenophase[i,8]<=59)| 
             (50<=widephenophase[i,9] &  widephenophase[i,9]<=59) | 
             (50<=widephenophase[i,10] &  widephenophase[i,10]<=59)), 
          widephenophase[i,13]<-T,
          widephenophase[i,13]<-F)
}

widephenophase<-dplyr::rename(widephenophase, BBCH50_59="V13") 


for(i in 1:length(widephenophase$ID)) {
  ifelse( ((60<=widephenophase[i,7] &  widephenophase[i,7]<=69)| 
             (60<=widephenophase[i,8] &  widephenophase[i,8]<=69)| 
             (60<=widephenophase[i,9] &  widephenophase[i,9]<=69) | 
             (60<=widephenophase[i,10] &  widephenophase[i,10]<=69)), 
          widephenophase[i,14]<-T,
          widephenophase[i,14]<-F)
}

widephenophase<-dplyr::rename(widephenophase, BBCH60_69="V14") 

for(i in 1:length(widephenophase$ID)) {
  ifelse( (65<=widephenophase[i,7] | 
             65<=widephenophase[i,8] | 
             65<=widephenophase[i,9] | 
             65<=widephenophase[i,10] ), 
          widephenophase[i,15]<-T,
          widephenophase[i,15]<-F)
}

widephenophase<-dplyr::rename(widephenophase, BBCH65="V15") 




for(i in 1:length(widephenophase$ID)) {
  ifelse( (65<=widephenophase[i,7] | 
             65<=widephenophase[i,8] | 
             65<=widephenophase[i,9] | 
             65<=widephenophase[i,10] ), 
          widephenophase[i,15]<-T,
          widephenophase[i,15]<-F)
}




for(i in 1:length(widephenophase$ID)) {
  ifelse( ((70<=widephenophase[i,7] &  widephenophase[i,7]<=79)| 
             (70<=widephenophase[i,8] &  widephenophase[i,8]<=79)| 
             (70<=widephenophase[i,9] &  widephenophase[i,9]<=79) | 
             (70<=widephenophase[i,10] &  widephenophase[i,10]<=79)), 
          widephenophase[i,16]<-T,
          widephenophase[i,16]<-F)
}

widephenophase<-dplyr::rename(widephenophase, BBCH70_79="V16") 


for(i in 1:length(widephenophase$ID)) {
  ifelse( ((80<=widephenophase[i,7] &  widephenophase[i,7]<=89)| 
             (80<=widephenophase[i,8] &  widephenophase[i,8]<=89)| 
             (80<=widephenophase[i,9] &  widephenophase[i,9]<=89) | 
             (80<=widephenophase[i,10] &  widephenophase[i,10]<=89)), 
          widephenophase[i,17]<-T,
          widephenophase[i,17]<-F)
}

widephenophase<-dplyr::rename(widephenophase, BBCH80_89="V17") 


widephenophase<-mutate(widephenophase,year=year(widephenophase$yyyymmdd))
head(widephenophase)

##example of a graph showing when phenophases happen
ggplot(na.omit(widephenophase), aes(x=DOY,BBCH50_59,color=year))+
  geom_jitter(width=0.5, height=.1,alpha=0.1)+
  facet_wrap(~year)


# phenophases -------------------------------------------------------------


phenophases<-read.csv("D:\\NDSU_work\\grapes\\copyofgrapedata\\first_yes_southgrapes.csv",check.names = F)



phenophases<-plyr::mutate(phenophases,year=year(yyyymmdd) )
vineyear<-paste(phenophases$Vine,phenophases$year)
phenophases<-cbind(vineyear,phenophases)
head(phenophases)

budburst<-phenophases[!is.na(phenophases$budburst_onset_doy),]
budburst<-budburst[c(3,4,5,6,7,33,39)]
leafonset<-phenophases[!is.na(phenophases$leaf_onset_doy),]
leafonset<-leafonset[c(3,4,5,6,7,34,39)]
floweronset<-phenophases[!is.na(phenophases$flower_onset_doy),]
floweronset<-floweronset[c(3,4,5,6,7,35,39)]
fruitonset<-phenophases[!is.na(phenophases$fruit_onset_doy),]
fruitonset<-fruitonset[c(3,4,5,6,7,37,39)]
verasion<-phenophases[!is.na(phenophases$verasion_doy),]
verasion<-verasion[c(3,4,5,6,7,38,39)]


onsetdf<-Reduce(function(x, y) merge(x, y, all=TRUE), list(budburst,floweronset,leafonset,fruitonset,verasion))
onsetdf$Date <-as_date(onsetdf$yyyymmdd)
write.csv(onsetdf,"D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\onsetdoy.csv")




###example of how to do proportional hazard regression for
##time to event analysis.

onsetdf20<-filter(onsetdf,year=="2020")
onsetdf21<-filter(onsetdf, year=="2021")

#choose budburst columns
budburst_onset20<-onsetdf20[1:7]
#omit rows with other data
budburst_onset20<-na.omit(budburst_onset20)
#status 1 indicates the event happened 
# if you had any missing data (which you dont), then status would 
#be zero
status<-c(rep(1,times=length(budburst_onset20$Vine)))
budburst_onset20<-cbind(status,budburst_onset20)


results.cox1<-coxph(Surv(budburst_onset_doy,status)~Female,data=budburst_onset20)
results.cox1
summary(results.cox1)
#No difference by female parent

results.cox2<-coxph(Surv(budburst_onset_doy,status)~Male,data=budburst_onset20)
results.cox2
summary(results.cox2)
#male parent matters





#choose budburst columns
budburst_onset21<-onsetdf21[1:7]
#omit rows with other data
budburst_onset21<-na.omit(budburst_onset21)
#status 1 indicates the event happened 
# if you had any missing data (which you dont), then status would 
#be zero
status<-c(rep(1,times=length(budburst_onset21$Vine)))
budburst_onset21<-cbind(status,budburst_onset21)


results.cox1<-coxph(Surv(budburst_onset_doy,status)~Female,data=budburst_onset21)
results.cox1
summary(results.cox1)
#No difference by female parent

results.cox2<-coxph(Surv(budburst_onset_doy,status)~Male,data=budburst_onset21)
results.cox2

summary(results.cox2)
#male parent matters





#### but it looks like there is a year by parent interaction....
#the sign of the coefficients are reversed.






