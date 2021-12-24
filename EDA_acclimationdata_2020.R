
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(car)

pheno_2020<-read.csv("D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\pheno_grape2020.csv",
                     colClasses = c(rep("factor",4),rep("numeric",27)), check.names = F)
head(pheno_2020)
pheno_2020<-pheno_2020[1:4368,1:31]
head(pheno_2020)



pheno_2021<-read.csv("D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\pheno_grape2021.csv",
                     colClasses = c(rep("factor",4),rep("numeric",22)), check.names = F)


test<-merge(pheno_2020,pheno_2021, all = T)
longpheno<-pivot_longer(test, cols=!c(1,2,3,4), names_to = "yyyymmdd", values_to = "bbch")

longpheno<-dplyr::mutate(longpheno, DOY=yday(yyyymmdd))

longpheno<-filter(longpheno, bbch<100)

longpheno<-unite(longpheno, ID, c(Vine,yyyymmdd), remove=F)

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




###this works....
for(i in 1:length(widephenophase$ID)) {
  ifelse((widephenophase[i,7]>=10| 
           widephenophase[i,8]>=10| 
           widephenophase[i,9]>=10 | 
           widephenophase[i,10]>=10 ), 
  widephenophase[i,11]<-T,
  widephenophase[i,11]<-F)
}

widephenophase<-dplyr::rename(widephenophase, BBCH10="V11") 
  
###this works....
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


ggplot(na.omit(widephenophase), aes(x=DOY,BBCH50_59,color=year))+
  geom_jitter(width=0.5, height=.1,alpha=0.1)+
  facet_wrap(~year)


# phenophases -------------------------------------------------------------


phenophases<-read.csv("D:\\NDSU_work\\grapes\\copyofgrapedata\\first_yes_southgrapes.csv",check.names = F)



phenophases<-plyr::mutate(phenophases,year=year(yyyymmdd) )
vineyear<-paste(phenophases$Vine,phenophases$year)
phenophases<-cbind(vineyear,phenophases)


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
onsetdf20<-filter(onsetdf,year=="2020")
onsetdf21<-filter(onsetdf, year=="2021")

tidy_onset<-read.csv("D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\20_21_corr_table.csv")
tidy_onset<-filter(tidy_onset,leaf_onset_doy_2021<160)
tidy_onset<-filter(tidy_onset,leaf_onset_doy_2020<175)
library(GGally)
ggpairs(tidy_onset[2:8])


# chillhours-----------This is not complete- Just messing around---------------------------------------------------
library(evobiR)
library(dplyr)
library(lubridate)
library(weathermetrics)
library(fruclimadapt)
library(chillR)
###you need a csv file from NDAWN listing daily highs and lows 
fargodf<-read.csv(header=T,"D:\\NDSU_work\\grapes\\grapephenoanalysis_14DEC2021\\fargo_daily.csv")
fargodf$Date<-as_date(fargodf$Date)

fargodf<-mutate(fargodf,DOY= yday(Date))
fargodf<-mutate(fargodf,Tmax=(fahrenheit.to.celsius(Max_Temp_F, round = 2)))
fargodf<-mutate(fargodf,Tmin=(fahrenheit.to.celsius(Min_Temp_F, round = 2)))
###Columnsfor hourly_temps
fargodf<-na.omit(fargodf)
fargodf<-fargodf[,c(4,5,6,11,12)]

##need to have dplyr loaded, not plyr
#hourlytempsfargo <- hourly_temps(fargodf,46.89)
hourlytempsfargo<-make_hourly_temps(latitude=46.89,fargodf)
hourtempsfargo<-stack_hourly_temps(fargodf,latitude=46.89)

# Calculate chill starting on DOY 1
Chillfargo<- chill_hours(hourlytempsfargo,1)
# Calculate growing_degree_hours
GDD<- GDD(hourtempsfargo$hourtemps, summ = TRUE, Tbase = 10)

# Combine the datasets Chill and GDH in a dataframe with a format compatible with the function phenology_sequential
fargochill_GDH<- merge(Chillfargo,GDD) %>%
  select(Date, Year, Month, Day, DOY, Chill,GDD) %>%
  arrange(Date)





#read in yielddata with GDD 
onsetdf<-plyr::rename(onsetdf, Date=yyyymmdd)

chillonset<-merge(fargochill_GDH, onsetdf, by="Date",all.y=F)

ggplot(data=chillonset, aes(x=Date,y=GD))+
  geom_point()


