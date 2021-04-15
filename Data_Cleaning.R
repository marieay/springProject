
## Data Cleaning ##

##ESS-Data

setwd("~/Desktop/Replication")

ESS.data <- read.csv("ESS1-9e01_1.csv", header = T, na = "NA")

#Subsetting the data

ESS.subset <- subset(ESS.data, select = c(cntry, stfdem, gndr,agea, pdjobyr, hinctnta, clsprty, polintr, eisced))

#Recoding the variables 

install.packages("car")
library(car)

ESS.subset$gndr.rc <- as.numeric(ESS.subset$gndr=="2")

ESS.subset$agea.rc <- recode(ESS.subset$agea,'0:21=1;22:30=2;31:40=3;41:50=4;51:60=5;61:70=6;71:90=7')

ESS.subset$pdjobyr.rc <- recode(ESS.subset$pdjobyr, '2012=1;1950:2012=0;c(6666,7777,8888,9999)="NA"')

ESS.subset$hinctnta.rc <- recode(ESS.subset$hinctnta, '1:3=1;4:7=2;8:10=3;c(77,88,99)="NA"')

ESS.subset$clsprty.rc <- recode(ESS.subset$clsprty,'2=0;c(7,8,9)="NA"')

ESS.subset$polintr.rc <- recode(ESS.subset$polintr,'c(7,8,9)="NA"')

ESS.subset$eisced.rc <- recode(ESS.subset$eisced,'c(2,3,4)=2;c(5,6)=3;7=4;c(55,0)="NA"')

#Treating refused answers, "Dont know" answers etc. as missing values - or should we use methods like multiple imputation?

ESS.subset <- subset(ESS.subset, select = - c(trstprl))

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