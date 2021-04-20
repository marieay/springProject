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

HEAD

#yay belgium
ESS.subsetWLG$prtvtcbeR <- recode(ESS.subsetWLG$prtvtcbe,'2=1;8=1;12=1;13=1;
                                                          1=0;3=0;4=0;5=0;6=0;7=0;9=0;10=0;11=0;14=0;15=0;16=0;17=0;18=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtcbeR)

#CZECHIA problem
ESS.subsetWLG$prtvtcczR <- recode(ESS6e02_4$prtvtccz,'3=1;4=1;5=1;1=0;2=0;8=0')
hist(ESS.subsetWLG$prtvtccz)

#denmark
ESS.subsetWLG$prtvtcdkR <- recode(ESS.subsetWLG$prtvtcdk,'1=1;4=1;2=1;
                                                          3=0;5=0;6=0;7=0;9=0;10=0;8=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtcdkR)

#estonia 
ESS.subsetWLG$prtvtdeeR <- recode(ESS.subsetWLG$prtvtdee,'1=1;3=1;
                                                          2=0;4=0;5=0;6=0;7=0;9=0;8=0;10=0;11=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtdeeR)

#finland
ESS.subsetWLG$prtvtcfiR <- recode(ESS.subsetWLG$prtvtcfi,'1=1;2=1;5=1;12=1;13=1;14=1;
                                                          ;3=0;4=0;6=0;7=0;8=0;9=0;10=0;11=0;15=0;16=0;17=0;18=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtcfiR)

#france
ESS.subsetWLG$prtvtcfrR <- recode(ESS.subsetWLG$prtvtcfr,'2=0;8=0;12=1;13=0;
                                                          1=0;3=0;4=0;5=0;6=0;7=1;9=1;10=0;11=0;14=0;15=0;16=0;17=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtcfrR)

#germany 
ESS.subsetWLG$prtvdde2R <- recode(ESS.subsetWLG$prtvdde2,'2=1;4=1;
                                                          1=0;3=0;5=0;6=0;7=0;8=0;9=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvdde2R)

#iceland
ESS.subsetWLG$prtvtaisR <- recode(ESS.subsetWLG$prtvtais,'2=0;8=0;
                                                          1=1;3=0;4=1;5=0;6=0;7=0;9=0;10=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtaisR)

#ireland 
ESS.subsetWLG$prtvtaieR <- recode(ESS.subsetWLG$prtvtaie,'2=1;8=0;
                                                          1=0;3=0;4=0;5=1;6=0;7=0;9=0;10=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtaieR)

#ISRAEL 
ESS.subsetWLG$prtvtbilR <- recode(ESS.subsetWLG$prtvtbil,'2=1;8=0;12=0;13=0;
                                                          1=1;3=1;4=1;5=0;6=1;7=0;9=1;10=0;11=0;14=0;15=0;16=0;17=0;18=0;19=0;20=0;21=0;22=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtbilR)

#italy 
ESS.subsetWLG$prtvtbitR <- recode(ESS.subsetWLG$prtvtbit,'2=0;8=1;12=0;13=0;
                                                          1=1;3=1;4=0;5=1;6=0;7=0;9=0;10=0;11=1;14=0;15=0;16=0;17=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtbitR)

#lituania
ESS.subsetWLG$prtvalt3R <- recode(ESS.subsetWLG$prtvalt3,'2=0;8=1;12=0;13=0;
                                                          1=0;3=1;4=0;5=0;6=0;7=1;9=1;10=0;11=0;14=0;15=0;16=0;17=0;18=0;19=0;20=0;21=0;22=0;23=0;24=0;25=0;26=0;27=0;28=0;66=0;77=0;88=0;99=0;33=0;44=0;55=0')
hist(ESS.subsetWLG$prtvalt3R)

#netherlands
ESS.subsetWLG$prtvtenlR <- recode(ESS.subsetWLG$prtvtenl,'2=1;8=0;12=0;13=0;
                                                          1=1;3=0;4=0;5=0;6=0;7=0;9=0;10=0;11=0;14=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtenlR)

#norway
ESS.subsetWLG$prtvtanoR <- recode(ESS.subsetWLG$prtvtano,'2=1;8=0;
                                                          1=0;3=1;4=0;5=0;6=1;7=0;9=0;10=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtanoR)

#poland
ESS.subsetWLG$prtvtcplR <- recode(ESS.subsetWLG$prtvtcpl,'2=1;8=0;
                                                          1=0;3=0;4=0;5=1;6=0;7=0;9=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtcplR)

#portugal
ESS.subsetWLG$prtvtbptR <- recode(ESS.subsetWLG$prtvtbpt,'2=1;8=0;12=0;13=0;
                                                          1=0;3=0;4=0;5=0;6=0;7=0;9=0;10=1;11=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtbptR)

#slovakia
ESS.subsetWLG$prtvtdsiR <- recode(ESS.subsetWLG$prtvtdsi,'2=0;8=1;12=0;
                                                          1=1;3=1;4=1;5=1;6=0;7=0;9=0;10=0;11=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtdsiR)

#sweden
ESS.subsetWLG$prtvtbseR <- recode(ESS.subsetWLG$prtvtbse,'2=1;8=0;
                                                          1=1;3=1;4=0;5=1;6=0;7=0;9=0;10=0;11=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtbseR)

#switzerland
ESS.subsetWLG$prtvtdchR <- recode(ESS.subsetWLG$prtvtdch,'2=1;8=0;12=0;13=0;
                                                          1=1;3=1;4=1;5=0;6=0;7=1;9=0;10=0;11=0;14=0;15=0;16=0;17=0;19=0;20=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtdchR)

#uk
ESS.subsetWLG$prtvtgbR <- recode(ESS.subsetWLG$prtvtgb,'2=0;12=0;13=0;
                                                          1=1;3=1;4=0;5=0;6=0;7=0;9=0;11=0;14=0;15=0;16=0;17=0;18=0;19=0;20=0;21=0;22=0;66=0;77=0;88=0;99=0')
hist(ESS.subsetWLG$prtvtgbR)


#bulgaria
ESS.subsetWLG$prtvtcbg <- as.character(ESS.subsetWLG$prtvtcbg)
ESS.subsetWLG$prtvtcbg<-as.numeric(ESS.subsetWLG$prtvtcbg=="1")
hist(ESS.subsetWLG$prtvtcbg)


#cyprus
ESS.subsetWLG$prtvtacy <- as.character(ESS.subsetWLG$prtvtacy)
ESS.subsetWLG$prtvtacy <- as.numeric(ESS.subsetWLG$prtvtacy=="1")
hist(ESS.subsetWLG$prtvtacy)

#spain
ESS.subsetWLG$prtvtces <- as.character(ESS.subsetWLG$prtvtces)
ESS.subsetWLG$prtvtces <- as.numeric(ESS.subsetWLG$prtvtces=='1')
hist(ESS.subsetWLG$prtvtces)


#slovakia
ESS.subsetWLG$prtvtcsk <- as.character(ESS.subsetWLG$prtvtcsk)
ESS.subsetWLG$prtvtcsk <- as.numeric(ESS.subsetWLG$prtvtcsk=='3')
hist(ESS.subsetWLG$prtvtcsk)


#hungary
ESS.subsetWLG$prtvtdhu <- as.character(ESS.subsetWLG$prtvtdhu)
ESS.subsetWLG$prtvtdhu <- as.numeric(ESS.subsetWLG$prtvtdhu=='3')
hist(ESS.subsetWLG$prtvtdhu)