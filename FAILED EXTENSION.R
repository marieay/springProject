ESS6e02_ex <- ESS6e02_4 %>% filter(!grepl('AL|XK|UA|RU|BE|CH|CZ|DE|DK|ES|FI|IE|IL|IS|IT|LT|NL|PL|PT|SE|SK', cntry))

ESS6e02_ex <- subset(ESS6e02_ex, select=c(cntry, stfdem, prtvtgb, prtvtdhu, prtvtdee, prtvtcbg, prtvtano, prtvtdsi, prtvtcfr, prtvtacy))

bgvt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtcbg))
bg<-bgvt %>% filter(!grepl('GB|HU|EE|SI|NO|CY|FR', cntry))
bg$pol <- car::recode(bg$prtvtcbg, "1=7.4;2=3.1964;3=5.46; 4=8.8;5=8.7;6=5;7=8.7;8=6.1111;9-13=NA")
table(bg$prtvtcbg)

pol_meanbg <- bg %>%
  na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))


ggplot(data=pol_meanbg, aes(x=pol, y=avg_polstf)) +
  geom_point()+
  xlim(0,10)+ylim(0,10) 


ESS_subset_WLG$prtvtcbgR<-recode(ESS_subset_WLG$prtvtcbg,'1=1;2=0;3=0;4=0;5=0;6=0;7=0;8=0;9=0;10=0;11=0;12=0;13=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtcbg)



esvt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtdee))
es<-esvt %>% filter(!grepl('GB|HU|SI|NO|BG|CY|FR', cntry))
es$pol <- car::recode(es$prtvtdee, "1=8.4962;2=3.9562;3=7.8969;4=7.4;5=4.1915;6=5.5556;7=7.5789;8=8.8;9=1.3;10-11=NA")
table(es$prtvtdee)

pol_meanes <- es %>%
  na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))


ggplot(data=pol_meanes, aes(x=pol, y=avg_polstf)) +
  geom_point()+
  xlim(0,10)+ylim(0,10) 


ESS_subset_WLG$prtvtdeeR <- recode(ESS_subset_WLG$prtvtdee,'1=1;3=1;
                                                          2=0;4=0;5=0;6=0;7=0;9=0;8=0;10=0;11=0;66=NA;77=NA;88=NA;99=NA')
hist(ESS_subset_WLG$prtvtdeeR)





gbvt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtgb))
gb<-gbvt %>% filter(!grepl('BG|HU|EE|SI|NO|CY|FR', cntry))
gb$pol <- car::recode(gb$prtvtgb, "1=8.2021;2=4.3562;3=4.0932;4=3.5871;5=NA;6=NA;7=NA;11=NA;12=NA;13=NA;14=NA;15=NA;16=NA;17=NA;18=NA;19=NA;20=NA;21=NA;22=NA")
table(gb$prtvtgb)

pol_meangb <- gb %>%
  na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))


ggplot(data=pol_meangb, aes(x=pol, y=avg_polstf)) +
  geom_point()+
  xlim(0,10)+ylim(0,10) 








huvt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtdhu))
hu<-huvt %>% filter(!grepl('GB|BG|EE|SI|NO|CY|FR', cntry))
hu$pol <- car::recode(hu$prtvtdhu, "1=NA;2=NA;3=6.5432;4=8.7000;5=2.9000;6=NA;7=NA;8=2.8743;9=NA;10=NA;11=NA;12=NA;13=NA;55=NA")
table(hu$prtvtdhu)

pol_meanhu <- hu %>%
  na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))


ggplot(data=pol_meanhu, aes(x=pol, y=avg_polstf)) +
  geom_point()+
  xlim(0,10)+ylim(0,10) 





novt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtano))
no<-novt %>% filter(!grepl('GB|HU|EE|SI|BG|CY|FR', cntry))
no$pol <- car::recode(no$prtvtano, "1=0.4089;2=1.5839;3=3.3706;4=5.1467;5=5.8516;6=4.6565;7=7.8994;8=8.7595;9=4.4293;10=NA")
table(no$prtvtano)

pol_meanno <- no %>%
  na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))

ggplot(data=pol_meanno, aes(x=pol, y=avg_polstf)) +
  geom_point()+
  xlim(0,10)+ylim(0,10)






slvt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtdsi))
sl<-slvt %>% filter(!grepl('GB|HU|EE|BG|NO|CY|FR', cntry))
sl$pol <- car::recode(sl$prtvtdsi, "1=3.2292;2=3.7351;3=7.9345;4=6.9996;5=6.6953;6=3.0637;7=3.4;8=6;9=3.4;10=4.7941;11=3.4; 12=NA")
table(sl$prtvtdsi)

pol_meansl <- sl %>%
  na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))


ggplot(data=pol_meansl, aes(x=pol, y=avg_polstf)) +
  geom_point()+
xlim(0,10)+ylim(0,10) 




frvt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtcfr))
fr<-frvt %>% filter(!grepl('GB|HU|EE|SI|NO|CY|BG', cntry))
fr$pol <- car::recode(fr$prtvtcfr, #"1=6;2=9.6854;3=3.9896;4=1.4;5=0; HERE6=5;7=8.7;8=6.1111;9-13=NA")
table(fr$prtvtcfr)

pol_meanfr <- fr %>%
  na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))


ggplot(data=pol_meanfr, aes(x=pol, y=avg_polstf)) +
  geom_point()+
  xlim(0,10)+ylim(0,10) 






cyvt <- subset(ESS6e02_ex, select = c(cntry, stfdem, prtvtacy))
cy<-cyvt %>% filter(!grepl('GB|HU|EE|SI|NO|FR|BG', cntry))
cy$pol <- car::recode(cy$prtvtacy, #"1=7.4;2=3.1964;3=5.46; 4=8.8;5=8.7;6=5;7=8.7;8=6.1111;9-13=NA")
table(cy$prtvtacy)
                      
pol_meancy <- cy %>%
    na.omit() %>%
  select(stfdem, pol) %>%
  group_by(pol) %>%
  summarize(avg_polstf = mean(stfdem))
                      
                      
ggplot(data=pol_meancy, aes(x=pol, y=avg_polstf)) +
  geom_point()+
  xlim(0,10)+ylim(0,10) 


