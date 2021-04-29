
## Data Cleaning ##

##ESS-Data

setwd("~/Desktop/Replication")

ESS.data <- read.csv("ESS1-9e01_1.csv", header = T, na = "NA")

#Deleting RU (remained after cleaning it only with the ESS cleaning tool)
ESS.data = filter(ESS.data, cntry != "RU")

#Subsetting the data

ESS_subset <- subset(ESS.data, select = c(cntry, stfdem, gndr,agea, pdjobyr, hinctnta, clsprty, polintr, eisced, trstprl, trstlgl , trstplc, trstplt, trstprt, trstep, trstun))

#Recoding the variables 

install.packages("car")
library(car)

ESS_subset$stfdem.rc <- car::recode(ESS_subset$stfdem, 'c(77,88,99)=NA')

ESS_subset$gndr.rc <- as.numeric(ESS_subset$gndr=="2")

ESS_subset$agea.rc <- recode(ESS_subset$agea, '0:21=1;22:30=2;31:40=3;41:50=4;51:60=5;61:70=6;71:90=7')

ESS_subset$pdjobyr.rc <- recode(ESS_subset$pdjobyr, '2012=1;1950:2012=0;c(6666,7777,8888,9999)=NA')

ESS_subset$hinctnta.rc <- recode(ESS_subset$hinctnta, '1:3=1;4:7=2;8:10=3;c(77,88,99)=NA')

ESS_subset$clsprty.rc <- recode(ESS_subset$clsprty, '2=0;c(7,8,9)=NA')

ESS_subset$polintr.rc <- recode(ESS_subset$polintr, 'c(7,8,9)=NA')

ESS_subset$eisced.rc <- recode(ESS_subset$eisced, 'c(2,3,4)=2;c(5,6)=3;7=4;c(55,88, 0)=NA')

ESS_subset <- subset(ESS_subset, select = - c(stfdem, gndr, agea, pdjobyr, hinctnta, clsprty, polintr, eisced))

# Trust in institution mean Thereby, we assume that the mean is NA if there is one value 77,88 or 99. 

ESS.subset.1 <- subset(ESS.data, select = c(trstprl, trstlgl , trstplc, trstplt, trstprt, trstep, trstun))
options(max.print=9999999)
trstinst_means <- print(rowMeans(ESS.subset.1, na.rm = TRUE)) 
trstinst <- as.data.frame(trstinst_means)

#Every value above 10 included 77,88, or 99 --> NA
trstinst [trstinst > 10 ] <- NA

#Adding trstinst to the ESS_dataset
ESS_subset$trstinst <- trstinst$trstinst_means
ESS_subset <- subset(ESS_subset, select = -c(trstlgl , trstplc, trstplt, trstprt, trstep, trstun))
                                               
#WLG

ESS_subset_WLG <- subset(ESS.data, select = c(cntry,prtvtcbe, prtvtacy, prtvtcbg,prtvtccz,prtvtcdk, prtvtdee, prtvtcfi, prtvtcfr, prtvdde2, prtvtdhu, prtvtais,prtvtaie, prtvtbil,prtvtbit,prtvalt3,prtvtenl,prtvtano,prtvtcpl,prtvtbpt,prtvtcsk,prtvtdsi,prtvtces,prtvtbse,prtvtdch,prtvtgb))

#belgium
ESS_subset_WLG$prtvtcbeR <- recode(ESS_subset_WLG$prtvtcbe,'2=1;8=1;12=1;13=1;
                                                          1=0;3=0;4=0;5=0;6=0;7=0;9=0;10=0;11=0;14=0;15=0;16=0;17=0;18=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcbeR)

#czechia
ESS_subset_WLG$prtvtcczR <- recode(ESS_subset_WLG$prtvtccz,'1=0;2=0;3=1;4=1;5=1;8=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtccz)

#denmark
ESS_subset_WLG$prtvtcdkR <- recode(ESS_subset_WLG$prtvtcdk,'1=1;4=1;2=1;
                                                          3=0;5=0;6=0;7=0;9=0;10=0;8=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcdkR)

#estonia 
ESS_subset_WLG$prtvtdeeR <- recode(ESS_subset_WLG$prtvtdee,'1=1;3=1;
                                                          2=0;4=0;5=0;6=0;7=0;9=0;8=0;10=0;11=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtdeeR)

#finland
ESS_subset_WLG$prtvtcfiR <- recode(ESS_subset_WLG$prtvtcfi,'1=1;2=1;5=1;12=1;13=1;14=1;
                                                          ;3=0;4=0;6=0;7=0;8=0;9=0;10=0;11=0;15=0;16=0;17=0;18=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcfiR)

#france
ESS_subset_WLG$prtvtcfrR <- recode(ESS_subset_WLG$prtvtcfr,'2=0;8=0;12=1;13=0;
                                                          1=0;3=0;4=0;5=0;6=0;7=1;9=1;10=0;11=0;14=0;15=0;16=0;17=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcfrR)

#germany 
ESS_subset_WLG$prtvtddeR <- recode(ESS_subset_WLG$prtvdde2,'2=1;4=1;
                                                          1=0;3=0;5=0;6=0;7=0;8=0;9=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtddeR)

#iceland
ESS_subset_WLG$prtvtaisR <- recode(ESS_subset_WLG$prtvtais,'2=0;8=0;
                                                          1=1;3=0;4=1;5=0;6=0;7=0;9=0;10=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtaisR)

#ireland 
ESS_subset_WLG$prtvtaieR <- recode(ESS_subset_WLG$prtvtaie,'2=1;8=0;
                                                          1=0;3=0;4=0;5=1;6=0;7=0;9=0;10=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtaieR)

#ISRAEL 
ESS_subset_WLG$prtvtbilR <- recode(ESS_subset_WLG$prtvtbil,'2=1;8=0;12=0;13=0;
                                                          1=1;3=1;4=1;5=0;6=1;7=0;9=1;10=0;11=0;14=0;15=0;16=0;17=0;18=0;19=0;20=0;21=0;22=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtbilR)

#italy 
ESS_subset_WLG$prtvtbitR <- recode(ESS_subset_WLG$prtvtbit,'2=0;8=1;12=0;13=0;
                                                          1=1;3=1;4=0;5=1;6=0;7=0;9=0;10=0;11=1;14=0;15=0;16=0;17=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtbitR)

#lituania
ESS_subset_WLG$prtvtaltR <- recode(ESS_subset_WLG$prtvalt3,'2=0;8=1;12=0;13=0;
                                                          1=0;3=1;4=0;5=0;6=0;7=1;9=1;10=0;11=0;14=0;15=0;16=0;17=0;18=0;19=0;20=0;21=0;22=0;23=0;24=0;25=0;26=0;27=0;28=0;66=NA;77=NA;88=NA;99=NA;33=0;44=0;55=0')
hist(ESS_subset_WLG$prtvtaltR)

#netherlands
ESS_subset_WLG$prtvtenlR <- recode(ESS_subset_WLG$prtvtenl,'2=1;8=0;12=0;13=0;
                                                          1=1;3=0;4=0;5=0;6=0;7=0;9=0;10=0;11=0;14=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtenlR)

#norway
ESS_subset_WLG$prtvtanoR <- recode(ESS_subset_WLG$prtvtano,'2=1;8=0;
                                                          1=0;3=1;4=0;5=0;6=1;7=0;9=0;10=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtanoR)

#poland
ESS_subset_WLG$prtvtcplR <- recode(ESS_subset_WLG$prtvtcpl,'2=1;8=0;
                                                          1=0;3=0;4=0;5=1;6=0;7=0;9=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcplR)

#portugal
ESS_subset_WLG$prtvtbptR <- recode(ESS_subset_WLG$prtvtbpt,'2=1;8=0;12=0;13=0;
                                                          1=0;3=0;4=0;5=0;6=0;7=0;9=0;10=1;11=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtbptR)

#slovakia
ESS_subset_WLG$prtvtdsiR <- recode(ESS_subset_WLG$prtvtdsi,'2=0;8=1;12=0;
                                                          1=1;3=1;4=1;5=1;6=0;7=0;9=0;10=0;11=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtdsiR)

#sweden
ESS_subset_WLG$prtvtbseR <- recode(ESS_subset_WLG$prtvtbse,'2=1;8=0;
                                                          1=1;3=1;4=0;5=1;6=0;7=0;9=0;10=0;11=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtbseR)

#switzerland
ESS_subset_WLG$prtvtdchR <- recode(ESS_subset_WLG$prtvtdch,'2=1;8=0;12=0;13=0;
                                                          1=1;3=1;4=1;5=0;6=0;7=1;9=0;10=0;11=0;14=0;15=0;16=0;17=0;19=0;20=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtdchR)

#uk
ESS_subset_WLG$prtvtgbbR <- recode(ESS_subset_WLG$prtvtgb,'2=0;12=0;13=0;
                                                          1=1;3=1;4=0;5=0;6=0;7=0;9=0;11=0;14=0;15=0;16=0;17=0;18=0;19=0;20=0;21=0;22=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtgbbR)

#bulgaria
ESS_subset_WLG$prtvtcbgR<-recode(ESS_subset_WLG$prtvtcbg,'1=1;2=0;3=0;4=0;5=0;6=0;7=0;8=0;9=0;10=0;11=0;12=0;13=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcbg)

#cyprus
ESS_subset_WLG$prtvtacyR <- recode(ESS_subset_WLG$prtvtacy,'1=1;2=0;3=0;4=0;5=0;6=0;7=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtacyR)

#spain
ESS_subset_WLG$prtvtcesR <- recode(ESS_subset_WLG$prtvtces,'1=1;2=0;3=0;4=0;5=0;6=0;7=0;8=0;9=0;10=0;11=0;12=0;13=0;14=0;15=0;16=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcesR)

#slovakia
ESS_subset_WLG$prtvtcskR <- recode(ESS_subset_WLG$prtvtcsk, '1=0;2=0;3=1;4=0;5=0;6=0;7=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcskR)

#hungary
ESS_subset_WLG$prtvtdhuR <- recode(ESS_subset_WLG$prtvtdhu, '1=0;2=0;3=1;4=0;5=0;6=0;7=0;8=0;9=0;10=0;11=0;12=0;13=0;55=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtdhuR)

#Going trough the observations and transferring them to the empty data frame 

library(tidyverse)
library(readr)

ESS_subset_WLG_new <-  tibble(count = 1:nrow(ESS_subset_WLG), country = "temp" , prtv = 444)

for (i in 1:nrow(ESS_subset_WLG)) {
  ESS_subset_WLG_new[i, "count"] = i
  ESS_subset_WLG_new[i,"country"] = unname(unlist(ESS_subset_WLG[i,"cntry"]))
  prvt_value = unlist(ESS_subset_WLG[i,3:length(ESS_subset_WLG)])
  prvt_value = (unname(prvt_value[!is.na(prvt_value)]))
  if (identical(prvt_value, numeric(0))) {ESS_subset_WLG_new[i, "prtv"] = NA} else 
  {ESS_subset_WLG_new[i, "prtv"] = unname(prvt_value[!is.na(prvt_value)])}
}

#Adding the WLG column to the main data frame
ESS_subset$wl <- ESS_subset_WLG_new$prtv

#SGI-Data
#Sorting of the data has been done on Excel and then converted to csv. 
#Merging of the SGI and the ESS data frame does not seem necessary at this point. 

##SGI-Data
#Sorting of the data has been done on Excel and then converted to csv. 

ESS.data <-read.table("SGI2014.txt", header = T, na = "NA")

#### Merging of the SGI and the ESS data frame does not seem necessary at this point. 

#####################

## The System Level Variables: Block Wise Regression 

## Loading the Dataframes into R 

## Golder 

# Note: The data for Cyprus was not available in this data base. We took the data from Casal Bértoa, F. (2021): Database on WHO GOVERNS in Europe and beyond, PSGo. Spefically, we miss data from: . Also, the data that is available stems from different years, namely 2009 and 2011. This also does not correspond to the year in which elections have been held. todo: compare the number 
# of parties of 2009 for the elections that have been held, no necessarly the same. 

setwd("~/Desktop/Replication")

Golder.data <- read.csv("es_data-v2_0_1.csv", header = T, na = "NA")

## DPI 

DPI.data <- read.csv("Database_of_political_institutions_2015.csv", header = T, na = "NA")

#Cleaned on Excel and manual input (Enep_PR_Data)

## World Bank (Inflation, GDP Growth and GDP per Capita)

# As to the GDP per Capita, we do not know which GDP per Capita measure they have used, specifically whether they have used the GDP measured in local currency without depriciation adjustment or GDP measured in current US Dollar. We chose real GDP per capita. 
# We use Inflation, consumer prices (annual %)	GDP growth (annual %)	GDP per capita (constant 2010 US$)
#Manual Import of the File through the Environment. 

##System Level Control Variable / SWD Estimation

#Add economic system level control variable to the ESS_subset

#Adding the country code of the main data frame to the Worldbank_Econ data frame

Worldbank_Econ$cntry <- WLG.df$country

ESS_subset <-  left_join(ESS_subset, Worldbank_Econ, by = 'cntry')
ESS_subset <- select(ESS_subset, select = -c(country, Country, ID))

#Adding the political system level control variables to the ESS_subset

colnames(Enep_PR_Data)[1] <- "cntry"

ESS_subset <-  left_join(ESS_subset, Enep_PR_Data, by = 'cntry')
ESS_subset <- select(ESS_subset, select = -c(ID))

#Outputting the finalized dataset

write_csv(ESS_subset, "ESS_subset.csv")

#For Model 2: Speratly regressing the system level control variables on SWD

mod.swd.econ <- lm (stfdem.rc~Inflation+GDPg+GDPpc, data = ESS_subset)
summary(mod.swd.econ)

mod.swd.party <- lm (stfdem.rc~Enep+PR, data = ESS_subset)
summary(mod.swd.party)

#How to integrate this into the main model?

