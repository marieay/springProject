## Data Cleaning ##

##ESS-Data

setwd("~/Desktop/Replication")

ESS.data <- read.csv("ESS1-9e01_1.csv", header = T, na = "NA")


#Subsetting the data

ESS.subset <- subset(ESS.data, select = c(cntry, stfdem, gndr,agea, pdjobyr, hinctnta, clsprty, polintr, eisced))

#Recoding the variables 

install.packages("car")
library(car)


ESS.subset$stfdem.rc <- car::recode(ESS.subset$stfdem, 'c(77,88,99)=NA')

ESS.subset$gndr.rc <- as.numeric(ESS.subset$gndr=="2")

ESS.subset$agea.rc <- recode(ESS.subset$agea, '0:21=1;22:30=2;31:40=3;41:50=4;51:60=5;61:70=6;71:90=7')

ESS.subset$pdjobyr.rc <- recode(ESS.subset$pdjobyr, '2012=1;1950:2012=0;c(6666,7777,8888,9999)=NA')

ESS.subset$hinctnta.rc <- recode(ESS.subset$hinctnta, '1:3=1;4:7=2;8:10=3;c(77,88,99)=NA')

ESS.subset$clsprty.rc <- recode(ESS.subset$clsprty, '2=0;c(7,8,9)=NA')

ESS.subset$polintr.rc <- recode(ESS.subset$polintr, 'c(7,8,9)=NA')

ESS.subset$eisced.rc <- recode(ESS.subset$eisced, 'c(2,3,4)=2;c(5,6)=3;7=4;c(55,88, 0)=NA')

#Treating refused answers, "Dont know" answers etc. as missing values - or should we use methods like multiple imputation?

<<<<<<< HEAD
ESS.subset <- subset(ESS.subset, select = - c(trstprl))
=======
ESS.subset <- subset(ESS.subset, select = - c(stfdem, gndr, agea, pdjobyr, hinctnta, clsprty, polintr, eisced))
>>>>>>> 9e6c83dffee1b55007c38049ca0f911a367a406f

#### What is missing: Trust in Institutions Mean, Winner Loser Dummy Variable

# Trust in institution Mean 
ess6 <- ESS6e02_4_spss_1_
ESS.subset.1 <- subset(ESS.data, select = c(trstprl, trstlgl , trstplc, trstplt, trstprt, trstep, trstun)
                       options(max.print=9999999)
trstinst_means <- print(rowMeans(ESS.subset.1, na.rm = TRUE)) 
# I dont know how to view them all though

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
DPI.data$pr

#Cleaned on Excel and manual input (Enep_PR_Data)

## World Bank (Inflation, GDP Growth and GDP per Capita)

# As to the GDP per Capita, we do not know which GDP per Capita measure they have used, specifically whether they have used the GDP measured in local currency without depriciation adjustment or GDP measured in current US Dollar. We chose real GDP per capita. 
# We use Inflation, consumer prices (annual %)	GDP growth (annual %)	GDP per capita (constant 2010 US$)
#Manual Import of the File through the Environment. 

##System Level Control Variable / SWD Estimation

cntrystfm <- aggregate(x = ESS.subset$stfdem.rc, by = list(ESS.subset$cntry), FUN = mean, na.rm = T)
class(cntrystfm)

cntrystfm <- cntrystfm[-c(23),]

mod.swd.party <- lm (cntrystfm$x~Enep_PR_Data$Enep+Enep_PR_Data$PR)
summary(mod.swd)

mod.swd.econ <- lm (cntrystfm$x~Worldbank_Econ$Inflation+Worldbank_Econ$GDPg+Worldbank_Econ$GDPpc)
summary(mod.swd.econ)

#Why have the researches used those indicators even though the p-value yre very high and also the estimates in the model 2 seem off. 

#Residual Versus Fits Plot for mod.swd.econ --> Linearity can be assumed. Should we run other diagnostics?

plot(y=mod.swd.econ$residuals, x=mod.swd.econ$fitted.values, xlab ="Fitted Values", ylab = "Residuals")
names(mod.swd.econ)


#### What is missing: Trust in Institutions Mean, Winner Loser Dummy Variable


##SGI-Data
#Sorting of the data has been done on Excel and then converted to csv. 

ESS.data <-read.table("SGI2014.txt", header = T, na = "NA")

#### Merging of the SGI and the ESS data frame does not seem necessary at this point. 




#####################

## Golder 

setwd("~/Desktop/Replication")

Golder.data <- read.csv("es_data-v2_0_1.csv", header = T, na = "NA")

## DPI 

DPI.data <- read.csv("Database_of_political_institutions_2015.csv", header = T, na = "NA")
DPI.data$pr
