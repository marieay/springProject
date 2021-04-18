read.csv("/Users/marieaymon/Downloads", header = TRUE)


# VARIABLE CLEANING -> WINNER LOSER GAP 

#noticed issue: most prtvt data is invalid/missing (valid answers usually only around 2%)
# can this still lead to tangible results?! 

votehist <- hist(ESS6e02_4$vote) #check to see if people voted seeing that when looking at the party voted for variables, in most cases only 2% of the answers are valid 
# clearly that not why theres barely any valid answers.. 


#subset data 
install.packages('car')
library(car)

ESS.subsetWLG <- subset(ESS6e02_4, select = c(prtvtcbe, prtvtacy, prtvtcbg, prtvtccz,prtvtcdk, prtvtdee, prtvtcfi, prtvtcfr, prtvdde2, prtvtdhu, prtvtais,prtvtaie, prtvtbil,prtvtbit,prtvalt3,prtvtenl,prtvtano,prtvtcpl,prtvtbpt,prtvtcsk,prtvtdsi,prtvtces,prtvtbse,prtvtdch,prtvtgb))

# this one doesnt work for some reason.. 
ESS6e02_4$prtvtcbe <- as.character(ESS6e02_4$prtvtcbe)
ESS6e02_4$prtvtcbe <- as.numeric(ESS6e02_4$prtvtcbe==c(2,8,12,13))
hist(ESS6e02_4$prtvtcbe) #looks wrong 

#yay
ESS6e02_4$prtvtccz<- recode(ESS6e02_4$prtvtccz,'3=1;4=1;5=1;1=0;2=0;8=0')
hist(ESS6e02_4$prtvtccz)

ESS.subsetWLG$prtvtcdk <- as.character(ESS.subsetWLG$prtvtcdk)
ESS.subsetWLG$prtvtcdk <- as.numeric(ESS.subsetWLG$prtvtcdk%in%c(1,2,4))
hist(ESS.subsetWLG$prtvtcdk)   #for sure wrong -> seems like this one counted NA somehow

ESS.subsetWLG$prtvtdee <- as.character(ESS.subsetWLG$prtvtdee)
ESS.subsetWLG$prtvtdee <- as.numeric(ESS.subsetWLG$prtvtdee=c(1,3))
hist(ESS.subsetWLG$prtvtdee) #seems wrong as well 

ESS.subsetWLG$prtvtcfi <- as.character(ESS.subsetWLG$prtvtcfi)
ESS.subsetWLG$prtvtcfi <- as.numeric(ESS.subsetWLG$prtvtcfi==c(1,2,5,12,13,14))
hist(ESS.subsetWLG$prtvtcfi) #fishy 



ESS.subsetWLG$prtvtdde2 <- as.character(ESS.subsetWLG$prtvdde2)
ESS.subsetWLG$prtvdde2 <- as.numeric(ESS.subsetWLG$prtvdde2==c(2,4))
hist(ESS.subsetWLG$prtvdde2) #cant recycle st 

ESS.subsetWLG$prtvtais <- as.character(ESS.subsetWLG$prtvtais)
ESS.subsetWLG$prtvtais <- as.numeric(ESS.subsetWLG$prtvtais%in%c(7,9))
hist(ESS.subsetWLG$prtvtais) # longer object lenght not subset & counted NA

ESS6e02_4$prtvtbil <- as.character(ESS6e02_4$prtvtbil)
ESS6e02_4$prtvtbil <- as.numeric(ESS6e02_4$prtvtbil==c(1,2,3,4,6,9))
hist(ESS6e02_4$prtvtbil) #worked but looks wrong 

ESS6e02_4$prtvtbit <- as.character(ESS6e02_4$prtvtbit)
ESS6e02_4$prtvtbit <- as.numeric(ESS6e02_4$prtvtbit==c(1,3,5,8,11))
hist(ESS6e02_4$prtvtbit) #maybe its right

ESS.subsetWLG$prtvalt3 <- as.character(ESS.subsetWLG$prtvalt3)
ESS.subsetWLG$prtvalt3 <- as.numeric(ESS.subsetWLG$prtvalt3==c(3,8,7,9))
hist(ESS.subsetWLG$prtvalt3) #idk.. 

ESS.subsetWLG$prtvtenl<- as.character(ESS.subsetWLG$prtvtenl)
ESS.subsetWLG$prtvtenl <- as.numeric(ESS.subsetWLG$prtvtenl==c(1,2))
hist(ESS.subsetWLG$prtvtenl) # i mean... 

ESS.subsetWLG$prtvtano <- as.character(ESS.subsetWLG$prtvtano)
ESS.subsetWLG$prtvtano <- as.numeric(ESS.subsetWLG$prtvtano==c(2,3,6))
hist(ESS.subsetWLG$prtvtano)

ESS.subsetWLG$prtvtcpl <- as.character(ESS.subsetWLG$prtvtcpl)
ESS.subsetWLG$prtvtcpl <- as.numeric(ESS.subsetWLG$prtvtcpl==c(2,5))
hist(ESS.subsetWLG$prtvtcpl)

ESS.subsetWLG$prtvtbpt <- as.character(ESS.subsetWLG$prtvtbpt)
ESS.subsetWLG$prtvtbpt <- as.numeric(ESS.subsetWLG$prtvtbpt==c(2,10))
hist(ESS.subsetWLG$prtvtbpt) #doesnt look right..again

ESS.subsetWLG$prtvtdsi <- as.character(ESS.subsetWLG$prtvtdsi)
ESS.subsetWLG$prtvtdsi <- as.numeric(ESS.subsetWLG$prtvtdsi==c(1,3,4,5,8))
hist(ESS.subsetWLG$prtvtdsi) #nope

ESS.subsetWLG$prtvtbse <- as.character(ESS.subsetWLG$prtvtbse)
ESS.subsetWLG$prtvtbse <- as.numeric(ESS.subsetWLG$prtvtbse==c(1,2,3,5))
hist(ESS.subsetWLG$prtvtbse)

ESS.subsetWLG$prtvtdch <- as.character(ESS.subsetWLG$prtvtdch)
ESS.subsetWLG$prtvtdch <- as.numeric(ESS.subsetWLG$prtvtdch==c(1,2,3,4,7))
hist(ESS.subsetWLG$prtvtdch)

ESS.subsetWLG$prtvtgb <- as.character(ESS.subsetWLG$prtvtgb)
ESS.subsetWLG$prtvtgb <- as.numeric(ESS.subsetWLG$prtvtgb==c(1,3))
hist(ESS.subsetWLG$prtvtgb) #maybe

#these are good now

ESS.subsetWLG$prtvtcbg <- as.character(ESS.subsetWLG$prtvtcbg)
ESS.subsetWLG$prtvtcbg<-as.numeric(ESS.subsetWLG$prtvtcbg=="1")
hist(ESS.subsetWLG$prtvtcbg)


ESS.subsetWLG$prtvtacy <- as.character(ESS.subsetWLG$prtvtacy)
ESS.subsetWLG$prtvtacy <- as.numeric(ESS.subsetWLG$prtvtacy=="1")
hist(ESS.subsetWLG$prtvtacy)

ESS.subsetWLG$prtvtces <- as.character(ESS.subsetWLG$prtvtces)
ESS.subsetWLG$prtvtces <- as.numeric(ESS.subsetWLG$prtvtces=='1')
hist(ESS.subsetWLG$prtvtces)

ESS.subsetWLG$prtvtcsk <- as.character(ESS.subsetWLG$prtvtcsk)
ESS.subsetWLG$prtvtcsk <- as.numeric(ESS.subsetWLG$prtvtcsk=='3')
hist(ESS.subsetWLG$prtvtcsk)

ESS.subsetWLG$prtvtdhu <- as.character(ESS.subsetWLG$prtvtdhu)
ESS.subsetWLG$prtvtdhu <- as.numeric(ESS.subsetWLG$prtvtdhu=='3')
hist(ESS.subsetWLG$prtvtdhu)

ESS.subsetWLG$prtvtcfrrec <- recode(ESS.subsetWLG$prtvtcfr, '7=1;9=1;1:6=0;8=0;10:15=0')
hist(ESS.subsetWLG$prtvtcfrrec)

ESS.subsetWLG$prtvtcfr <- as.character(ESS.subsetWLG$prtvtcfr)
ESS.subsetWLG$prtvtcfr <- as.numeric(ESS.subsetWLG$prtvtcfr==c(7,9))
hist(ESS.subsetWLG$prtvtcfr)

