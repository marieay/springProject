
## Data Cleaning ##

##ESS-Data

setwd("~/Desktop/Replication")

ESS.data <- read.csv("ESS1-9e01_1.csv", header = T, na = "NA")
ESS.data

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

ESS.subset <- subset(ESS.subset, select = - c(stfdem, gndr, agea, pdjobyr, hinctnta, clsprty, polintr, eisced))

#### What is missing: Trust in Institutions Mean, Winner Loser Dummy Variable

# Trust in institution Mean:
trstinst_BE <- ESS.data[c(1202:3070),c(6,13:19)]
summary(trstinst_BE)
assign("mean_trstinst_BE", 5.028)

trstinst_BG <- ESS.data[c(3071:5330),c(6,13:19)]
summary(trstinst_BG)
assign("mean_trstinst_BG", 2.867)

trstinst_CH <- ESS.data[c(5331:6823),c(6,13:19)]
summary(trstinst_CH)
assign("mean_trstinst_CH", 5.734)

trstinst_CY <- ESS.data[c(6824:7939),c(6,13:19)]
summary(trstinst_CY)
assign("mean_trstinst_CY", 3.770)

trstinst_CZ <- ESS.data[c(7940:9948),c(6,13:19)]
summary(trstinst_CZ)
assign("mean_trstinst_CZ", 3.750)

trstinst_DE <- ESS.data[c(9949:12906),c(6,13:19)]
summary(trstinst_DE)
assign("mean_trstinst_DE", 4.871)

trstinst_DK <- ESS.data[c(12907:14556),c(6,13:19)]
summary(trstinst_DK)
assign("mean_trstinst_DK", 6.289)

trstinst_EE <- ESS.data[c(14557:16936),c(6,13:19)]
summary(trstinst_EE)
assign("mean_trstinst_EE", 4.459)

trstinst_ES <- ESS.data[c(16937:18825),c(6,13:19)]
summary(trstinst_ES)
assign("mean_trstinst_ES", 3.630)

trstinst_FI <- ESS.data[c(18826:21022),c(6,13:19)]
summary(trstinst_FI)
assign("mean_trstinst_FI", 6.051)

trstinst_FR <- ESS.data[c(21023:22990),c(6,13:19)]
summary(trstinst_FR)
assign("mean_trstinst_FR", 4.403)

trstinst_GB <- ESS.data[c(22991:25276),c(6,13:19)]
summary(trstinst_GB)
assign("mean_trstinst_GB", 4.557)

trstinst_HU <- ESS.data[c(25277:27290),c(6,13:19)]
summary(trstinst_HU)
assign("mean_trstinst_HU", 4.214)

trstinst <- ESS.data[c(27291:29918),c(6,13:19)]
summary(trstinst_IE)
assign("mean_trstinst_IE", 4.470)

trstinst_IL <- ESS.data[c(29919:32426),c(6,13:19)]
summary(trstinst_IL)
assign("mean_trstinst_IL", 4.099)

trstinst_IS <- ESS.data[c(32427:33178),c(6,13:19)]
summary(trstinst_IS)
assign("mean_trstinst_IS", 5.126)

trstinst_IT <- ESS.data[c(33179:34138),c(6,13:19)]
summary(trstinst_IT)
assign("mean_trstinst_IT", 3.892)

trstinst_LT <- ESS.data[c(34139:36247),c(6,13:19)]
summary(trstinst_LT)
assign("mean_trstinst_LT", 4.037)

trstinst_NL <- ESS.data[c(36248:38092),c(6,13:19)]
summary(trstinst_NL)
assign("mean_trstinst_NL", 5.443)

trstinst_NO <- ESS.data[c(38093:39716),c(6,13:19)]
summary(trstinst_NO)
assign("mean_trstinst_NO", 6.073)

trstinst_PL <- ESS.data[c(39717:41614),c(6,13:19)]
summary(trstinst_PL)
assign("mean_trstinst_PL", 3.652)

trstinst_PT <- ESS.data[c(42615:43765),c(6,13:19)]
summary(trstinst_PT)
assign("mean_trstinst_PT", 3.217)

trstinst_SE <- ESS.data[c(46250:48096),c(6,13:19)]
summary(trstinst_SE)
assign("mean_trstinst_SE", 5.643)

trstinst_SI <- ESS.data[c(38097:49353),c(6,13:19)]
summary(trstinst_SI)
assign("mean_trstinst_SI", 4.179)

trstinst_SK <- ESS.data[c(49354:51200),c(6,13:19)]
summary(trstinst_SK)
assign("mean_trstinst_SK", 3.424)

# Creating a data frame called: df_trstinst

df_trstinst <- data.frame(mean_trstinst_BE,mean_trstinst_BG, mean_trstinst_CH,  mean_trstinst_CY,  mean_trstinst_CZ,  mean_trstinst_DE,  mean_trstinst_DK,  mean_trstinst_EE,  mean_trstinst_ES,  mean_trstinst_FI,  mean_trstinst_FR,  mean_trstinst_GB,  mean_trstinst_HU,  mean_trstinst_IE,  mean_trstinst_IL,  mean_trstinst_IS,  mean_trstinst_IT,  mean_trstinst_LT,  mean_trstinst_NL,  mean_trstinst_NO,  mean_trstinst_PL,  mean_trstinst_PT,  mean_trstinst_SE,  mean_trstinst_SI,  mean_trstinst_SK)

#SGI-Data
#Sorting of the data has been done on Excel and then converted to csv. 
#Merging of the SGI and the ESS data frame does not seem necessary at this point. 

#####################

## The System Level Variables: Block Wise Regression 

## Loading the Dataframes into R 

## Golder 

# Note: The data for Cyprus was not available in this data base. We took the data from Casal Bértoa, F. (2021): Database on WHO GOVERNS in Europe and beyond, PSGo. Spefically, we miss data from: . Also, the data that is available stems from different years, namely 2009 and 2011. This also does not correspond to the year in which elections have been held. todo: compare the number 
# of parties of 2009 for the elections that have been held, no necessarly the same. 

setwd("~/Desktop/Replication")

Golder.data <- read.csv("es_data-v2_0_1.csv", header = T, na = "NA")

## DPI 

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

