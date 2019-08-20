# Install and Load Packages --------------------------------------------------------

#install.packages('tidyverse')#install.packages('vegan')
#install.packages('MASS');install.packages("devtools"); devtools::install_github("gavinsimpson/ggvegan")
install.packages('ggplot2');install.packages('dplyr')


#install.packages('scales')
#install.packages('viridis'); 
#install.packages('lmerTest'); install.packages('gridExtra');install.packages('ggplot2');library(lmerTest); install.packages('ggpubr')
#install.packages('colorspace'); install.packages('ggpubr')


library('tidyverse'); library('ggplot2')
#library('scales')
 library('dplyr')
#library('ggpubr');library('viridis'); 
#library('gridExtra')


setwd("C:\\Users\\Julia Michaels\\Google Drive\\Dissertation Chapter 3\\Dissertation_Chapter_3")




# Load data ---------------------------------------------------------------
data_loggers_2018<-read.csv('2018_Levelloggers.csv')
staff_gauge_2018<-read.csv('2017-2018 Staff Gauges All.csv')
precip_2018<-read.csv('precipitation data_2018.csv')
char<-read.csv('Pool characteristics.csv')


# Calculate precip by day -------------------------------------------------

precip_2018_by_day<-precip_2018 %>%
  group_by(Date) %>% 
  summarize(rainfall=sum(Precip, na.rm = TRUE)) %>% 
  slice(2:n()) %>% 
  mutate(rainfall=rainfall/10) 

# Calculate total days of inundation from data loggers --------------------

DL2018<-data_loggers_2018%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 

dl_days_2018<-c()
for(i in 2:ncol(DL2018)){
  dl_days_2018[i]<-sum(DL2018[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

dl_days_2018<-tibble(dl_days_2018, Pool.ID=colnames(DL2018))

dl_days_2018<-dl_days_2018[-15,]


# format data -------------------------------------------------------------

staff_gauge_2018$Date<-as.Date(staff_gauge_2018$Date)

data_loggers_2018$Date<-as.Date(data_loggers_2018$Date)


DL2018<-data_loggers_2018%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 

DL2018[DL2018 < 0] = 0


# Calculate total days inundation -----------------------
dl_days<-c()
for(i in 2:ncol(DL2018)){
  dl_days[i]<-sum(DL2018[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

dl_days_daily<-tibble(dl_days, Pool.ID=colnames(DL2018))
dl_days_daily<-dl_days_daily[2:34,]


# Calculate total days inundation from weekly selection -----------------------

# subset dataloggers as if they were sampled weekly 
dl_days_daily_2<-dl_days_daily %>% 
  mutate(dl_subset_days=dl_days) %>% 
  mutate(Method='Daily')


DL2018$Date<-as.factor(DL2018$Date)
DL_weekly_subset<- DL2018%>% 
  dplyr::filter(Date %in% c('2017-11-06','2017-11-13',	'2017-11-20',	'2017-11-27',	'2017-12-04',	'2017-12-11',	'2017-12-18',	'2017-12-25',	'2018-01-01',	'2018-01-08',	'2018-01-15', '2018-01-22',	'2018-01-29',	'2018-02-05',	'2018-02-12',	'2018-02-19',	'2018-02-26',	'2018-03-05',	'2018-03-12',	'2018-03-19',	'2018-03-26',	'2018-04-02',	'2018-04-09',	'2018-04-16',	'2018-04-23',	'2018-04-30',	'2018-05-07',	'2018-05-14',	'2018-05-21',	'2018-05-28'))

#write.csv(DL_weekly_subset, 'weekly.csv')
interval_days<-c(1,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7)

DL_weekly_subset<-data.frame(DL_weekly_subset[, 2:ncol(DL_weekly_subset)])


DL_weekly_subset[DL_weekly_subset > 0] = 1

DL_weekly_subset$Interval_Days<-interval_days

DL_weekly<-DL_weekly_subset[,1:33] * DL_weekly_subset[,34]

dl_days_weekly<-c()
for(i in 1:ncol(DL_weekly)){
  dl_days_weekly[i]<-sum(DL_weekly[,i], na.rm=TRUE)
}

dl_days_weekly<-tibble(dl_days_weekly, Pool.ID=colnames(DL2018[,2:34]))

#Compare daily and weekly datalogger 
compare_weekly<-full_join(dl_days_daily, dl_days_weekly)%>% 
  mutate(compare=dl_days-dl_days_weekly)%>% 
  mutate(Method ="Weekly")
colnames(compare_weekly)[3] <- "dl_subset_days"

mean(compare_weekly$compare, na.rm=TRUE)
median(compare_weekly$compare, na.rm=TRUE)
mean(abs(compare_weekly$compare), na.rm=TRUE)
median(compare_weekly$compare, na.rm=TRUE)
t.test(compare_weekly$dl_days, compare_weekly$dl_subset_days, paired=TRUE)
fit_weekly<-lm(dl_days ~ dl_subset_days, data = compare_weekly)
summary(fit_weekly)

# Calculate total days inundation from biweekly selection -----------------------

# subset dataloggers as if they were sampled biweekly (keeping the dates that staff gauges really were sampled)
DL2018$Date<-as.factor(DL2018$Date)

DL_biweekly_subset<- DL2018%>% 
  filter(Date %in% c('2017-11-06','2017-11-20',	'2017-12-04',	'2017-12-18',	'2018-01-01',	'2018-01-15',	'2018-01-29',	'2018-02-12',	'2018-02-26',	'2018-03-12',	'2018-03-26',	'2018-04-09',	'2018-04-23',	'2018-05-07',	'2018-05-21'))
#write.csv(DL_biweekly_subset, 'biweekly.csv')
interval_days<-c(1, 14,14,14,14,14,14,14,14,14,14,14,14,14,14)

DL_biweekly_subset<-data.frame(DL_biweekly_subset[, 2:ncol(DL_biweekly_subset)])

DL_biweekly_subset[DL_biweekly_subset > 0] = 1

DL_biweekly_subset$Interval_Days<-interval_days

DL_biweekly<-DL_biweekly_subset[,1:33] * DL_biweekly_subset[,34]

dl_days_biweekly<-c()
for(i in 1:ncol(DL_biweekly)){
  dl_days_biweekly[i]<-sum(DL_biweekly[,i], na.rm=TRUE)
}

dl_days_biweekly<-tibble(dl_days_biweekly, Pool.ID=colnames(DL2018[,2:34]))

#Compare daily and weekly datalogger 
compare_biweekly<-full_join(dl_days_daily, dl_days_biweekly)%>% 
  mutate(compare=dl_days-dl_days_biweekly)%>% 
  mutate(Method ="Every Two Weeks")
colnames(compare_biweekly)[3] <- "dl_subset_days"

mean(compare_biweekly$compare, na.rm=TRUE)
mean(abs(compare_biweekly$compare), na.rm=TRUE)
t.test(compare_biweekly$dl_days, compare_biweekly$dl_subset_days)



fit_biweekly<-lm(dl_days ~ dl_subset_days, data = compare_biweekly)
summary(fit_biweekly)

# Calculate total days inundation from monthly selection -----------------------

# subset dataloggers as if they were sampled monthly (keeping the dates that staff gauges really were sampled)
DL2018$Date<-as.factor(DL2018$Date)
DL_monthly_subset<- DL2018%>% 
  filter(Date %in% c('2017-11-06','2017-12-04',	'2018-01-01',	'2018-01-29',	'2018-02-26',	'2018-03-26',	'2018-04-23',	'2018-05-21'))
write.csv(DL_monthly_subset, 'monthly.csv')
interval_days<-c(1,28, 28, 28,28,28,28,28)
DL_monthly_subset<-data.frame(DL_monthly_subset[, 2:ncol(DL_monthly_subset)])

DL_monthly_subset[DL_monthly_subset > 0] = 1

DL_monthly_subset$Interval_Days<-interval_days

DL_monthly<-DL_monthly_subset[,1:33] * DL_monthly_subset[,34]


dl_days_monthly<-c()
for(i in 1:ncol(DL_monthly)){
  dl_days_monthly[i]<-sum(DL_monthly[,i], na.rm=TRUE)
}

dl_days_monthly<-tibble(dl_days_monthly, Pool.ID=colnames(DL2018[,2:34]))

#Compare daily and weekly datalogger 
compare_monthly<-full_join(dl_days_daily, dl_days_monthly)%>% 
  mutate(compare=dl_days-dl_days_monthly)%>% 
  mutate(Method ="Every Four Weeks")
colnames(compare_monthly)[3] <- "dl_subset_days"

mean(compare_monthly$compare, na.rm=TRUE)
mean(abs(compare_monthly$compare), na.rm=TRUE)
t.test(compare_monthly$dl_days, compare_monthly$dl_subset_days)

fit_monthly<-lm(dl_days ~ dl_subset_days, data = compare_monthly)
summary(fit_monthly)



#Graph

all_methods<-full_join(compare_weekly, compare_biweekly, by=c("dl_days", "Pool.ID", "Method", "compare", 'dl_subset_days'))
all_methods<-full_join(all_methods, compare_monthly, by=c("dl_days", "Pool.ID", "Method", "compare", 'dl_subset_days'))

all_methods$Method <- factor(all_methods$Method, levels=c("Weekly", "Every Two Weeks","Every Four Weeks", 'dl_subset_days'))
all_method<-all_methods %>% 
  filter(!Pool.ID=="D5.13")

compare_weekly<-compare_weekly %>% 
  filter(!Pool.ID=="D5.13")
sd(compare_weekly$dl_days)
mean(compare_weekly$dl_days)
mean(compare_weekly$dl_subset_days)


compare_biweekly<-compare_biweekly %>% 
  filter(!Pool.ID=="D5.13")
sd(compare_biweekly$dl_days)
mean(compare_biweekly$dl_days)
mean(compare_biweekly$dl_subset_days)

compare_monthly<-compare_monthly %>% 
  filter(!Pool.ID=="D5.13")
sd(compare_monthly$dl_days)
mean(compare_monthly$dl_days)
mean(compare_monthly$dl_subset_days)

library('ggplot2')

plot1<-ggplot(data=all_methods, aes(x=dl_days,y=dl_subset_days))+
  geom_point(aes(color = Method), position='jitter', size=3)+
  xlim(0,60)+
  ylim(0,60)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, aes(color=Method))+
  labs(y="Hydroperiod measured by Subsetted Sampling", x="Hydroperiod measured by Daily Sampling")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=25))+
  theme(axis.title.y =element_text(size=18))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))+
  theme(strip.text =element_text(size=30))+
  scale_color_discrete(name="Sampling Frequency")

compare_weekly<-full_join(compare_weekly, dl_days_daily_2)
compare_all<-full_join(compare_weekly, compare_biweekly)
compare_all<-full_join(compare_all, compare_monthly)
#write.csv(compare_all, "compare_all.csv")

library('lme4')
m1<-lmer(dl_subset_days~Method+(1|Pool.ID), data=compare_all)
summary(anova(m1))
summary(aov(dl_subset_days~Method+ Error(1/Pool.ID), data = compare_all))



contin<-read.csv('Continuous_Days_Inundation.csv') %>% 
  mutate(Title='Continuous Hydroperiod')

all_methods<-all_methods %>% 
  mutate(Title='Cumulative Hydroperiod')
  

all<-full_join(contin, all_methods) %>% 
  mutate(difference=dl_days-dl_subset_days) 

#write.csv(all, 'all2.csv')
all$Method<-as.factor(all$Method)
#library('dplyr')
all_averaged<-all%>% 
  dplyr::group_by(Method) %>% 
  summarize(difference=median(difference))

plot2<-ggplot(data=all, aes(x=dl_days,y=dl_subset_days))+
  geom_point(aes(color = Method), position='jitter', size=3)+
  xlim(0,100)+
  ylim(0,100)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, aes(color=Method))+
  labs(y="Hydroperiod measured by Subsetted Sampling", x="Hydroperiod measured by Daily Sampling")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=25))+
  theme(axis.title.y =element_text(size=22))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))+
  theme(strip.text =element_text(size=30))+
  scale_color_discrete(name="Sampling Frequency")+
  facet_wrap(~Title)

#theme(legend.position ='none')

#install.packages('gridExtra'); library('gridExtra')
DS_1<-grid.arrange(plot1, plot2, nrow=1) #3000x466

fit_weekly<-lm(dl_days ~ dl_subset_days, data = all[all$Method=="Weekly" & all$Title=="Continuous Hydroperiod",])
summary(fit_weekly)

fit_biweekly<-lm(dl_days ~ dl_subset_days, data = all[all$Method=="Every Two Weeks" & all$Title=="Continuous Hydroperiod",])
summary(fit_biweekly)

fit_four_weeks<-lm(dl_days ~ dl_subset_days, data = all[all$Method=="Every Four Weeks" & all$Title=="Continuous Hydroperiod",])
summary(fit_four_weeks)




# Hydrographs -------------------------------------------------------------
sg_subset_2018<-data.frame(staff_gauge_2018[, 2:ncol(staff_gauge_2018)])
sg_subset_2018[sg_subset_2018 < 0] = 0

sg_subset_depth<-sg_subset_2018
sg_subset_2018$Date<-staff_gauge_2018$Date
#sg_subset_2018[is.na(sg_subset_2018)] = 1 #make all na vals (days we didnt check staff gauge) 1 because we cant assume it was dry
sg_hydrograph_weekly<-sg_subset_2018 

#library('tidyr')
#Daily Datalogger
DL<-DL2018 %>% 
  group_by(Date) %>% 
  summarise_all(funs(median)) 

DL_hydrograph<-gather(DL, "Pool.ID", "Level", 2:34)

DL_hydrograph$Date<-as.Date(DL_hydrograph$Date)
DL_hydrograph_subset<-DL_hydrograph #pull out for later analysis

#install.packages('dplyr')
#library('dplyr')
DL_hydrograph<- DL_hydrograph%>% 
  #group_by(Date, Pool.ID)%>% 
  #summarize(Level=mean(Level))%>% 
  filter(Date>='2017-11-06') %>% 
  filter(Date<='2018-06-01') %>% 
  mutate(Method="Hourly datalogger") %>% 
  mutate(Method2="Datalogger")

DL_hydrograph[DL_hydrograph < 0] = 0
DL_hydrograph$Date<-as.factor(DL_hydrograph$Date)

#detatch(package:plyr)

#calculate depth
Daily_avg<-DL_hydrograph %>% 
  group_by(Pool.ID) %>% 
  summarise(Daily_Avg_Level=mean(Level))


#Weekly staff gauge 
sg_hydrograph_weekly_temp<-as.data.frame(sg_hydrograph_weekly[,colnames(sg_hydrograph_weekly) %in% DL_hydrograph$Pool.ID])

sg_hydrograph_weekly<-sg_hydrograph_weekly_temp %>% 
  mutate(Date=sg_hydrograph_weekly$Date)


sg_hydrograph_weekly<-gather(sg_hydrograph_weekly, "Pool.ID", "Level", 1:32)
sg_hydrograph_weekly$Date<-as.Date(sg_hydrograph_weekly$Date)


weekly_hydrograph<-sg_hydrograph_weekly %>% 
  filter(Date>='2017-11-06') %>% 
  filter(Date<='2018-03-19') %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level))%>% 
  mutate(Method="Weekly") %>% 
  mutate(Method2="Staff Gauge")

weekly_hydrograph$Date<-as.factor(weekly_hydrograph$Date)
sg_hydrograph_weekly$Date<-as.factor(sg_hydrograph_weekly$Date)




hydrograph_all<-full_join(weekly_hydrograph, DL_hydrograph, by=c("Pool.ID", "Method", "Method2", "Date", "Level"))

hydrograph_averaged<-hydrograph_all %>% 
  group_by(Method, Date) %>% 
  summarize(Level=mean(Level))

hydrograph_averaged$Method <- factor(hydrograph_averaged$Method, levels=c("Hourly datalogger", "Weekly"),
                                     labels=c("Daily", "Weekly"))



##Weekly hydrograph

DL_hydrograph_subset$Date<-as.factor(DL_hydrograph_subset$Date)
DL_hydrograph_model<- DL_hydrograph_subset%>% 
  group_by(Date, Pool.ID)%>% 
  summarize(Level=mean(Level))%>% 
  filter(Date %in% c('2017-11-06','2017-11-13',	'2017-11-20',	'2017-11-27',	'2017-12-04',	'2017-12-11',	'2017-12-18',	'2017-12-25',	'2018-01-01',	'2018-01-08',	'2018-01-15', '2018-01-22',	'2018-01-29',	'2018-02-05',	'2018-02-12',	'2018-02-19',	'2018-02-26',	'2018-03-05',	'2018-03-12',	'2018-03-19',	'2018-03-26',	'2018-04-02',	'2018-04-09',	'2018-04-16',	'2018-04-23',	'2018-04-30',	'2018-05-07',	'2018-05-14',	'2018-05-21',	'2018-05-28')) %>% 
  mutate(Method2="Datalogger") %>% 
  mutate(Method="Weekly") 
DL_hydrograph_model$Date<-as.Date(DL_hydrograph_model$Date)
DL_hydrograph_model[DL_hydrograph_model < 0] = 0
DL_hydrograph_model$Date<-as.factor(DL_hydrograph_model$Date)


#calculate depth
weekly_avg<-DL_hydrograph_model%>% 
  group_by(Pool.ID) %>% 
  summarize(Weekly_Avg_Level=mean(Level))


#biweekly hydrograph

DL_hydrograph_model_biweekly<- DL_hydrograph_subset%>% 
  group_by(Date, Pool.ID)%>% 
  summarize(Level=mean(Level))%>% 
  filter(Date %in%c('2017-11-06','2017-11-20',	'2017-12-04',	'2017-12-18',	'2018-01-01',	'2018-01-15',	'2018-01-29',	'2018-02-12',	'2018-02-26',	'2018-03-12',	'2018-03-26',	'2018-04-09',	'2018-04-23',	'2018-05-07',	'2018-05-21')) %>% 
  mutate(Method="Every Two Weeks") %>% 
  mutate(Method2="Datalogger")
DL_hydrograph_model_biweekly$Date<-as.Date(DL_hydrograph_model_biweekly$Date)
DL_hydrograph_model_biweekly[DL_hydrograph_model_biweekly < 0] = 0
DL_hydrograph_model_biweekly$Date<-as.factor(DL_hydrograph_model_biweekly$Date)

#calculate depth
biweekly_avg<-DL_hydrograph_model_biweekly %>% 
  filter(!is.na(Level)) %>% 
  group_by(Pool.ID) %>% 
  summarize(Biweekly_Avg_Level=mean(Level))

#monthly hydrograph

DL_hydrograph_model_monthly<- DL_hydrograph_subset%>% 
  group_by(Date, Pool.ID)%>% 
  summarize(Level=mean(Level))%>% 
  filter(Date %in%  c('2017-11-06','2017-12-04',	'2018-01-01',	'2018-01-29',	'2018-02-26',	'2018-03-26',	'2018-04-23',	'2018-05-21')) %>% 
  mutate(Method="Every Four Weeks") %>% 
  mutate(Method2="Datalogger")
DL_hydrograph_model_monthly$Date<-as.Date(DL_hydrograph_model_monthly$Date)
DL_hydrograph_model_monthly[DL_hydrograph_model_monthly < 0] = 0
DL_hydrograph_model_monthly$Date<-as.factor(DL_hydrograph_model_monthly$Date)

hydrograph_all<-full_join(hydrograph_all, DL_hydrograph_model, by=c("Method", "Method2", "Date", "Level", "Method2", "Pool.ID"))
hydrograph_all<-full_join(hydrograph_all, DL_hydrograph_model_biweekly, by=c("Method", "Method2", "Date", "Level", "Method2", "Pool.ID"))
hydrograph_all<-full_join(hydrograph_all, DL_hydrograph_model_monthly, by=c("Method", "Method2", "Date", "Level", "Method2", "Pool.ID"))

#calculate depth
monthly_avg<-DL_hydrograph_model_monthly %>% 
  filter(!is.na(Level)) %>% 
  group_by(Pool.ID) %>% 
  summarize(Monthly_Avg_Level=mean(Level))



##precipitation

precip_2018_by_day$Date<-as.Date(precip_2018_by_day$Date)

precip_2018_by_day<-precip_2018_by_day%>% 
  filter(Date>='2017-11-06')%>% 
  filter(Date<='2018-06-01')
precip_2018_by_day$Date<-as.factor(precip_2018_by_day$Date)

precip_2018_by_day$Date<-as.Date(precip_2018_by_day$Date)

precip_2018_by_day<-precip_2018_by_day%>% 
  filter(Date>='2017-11-06')%>% 
  filter(Date<='2018-06-01')
precip_2018_by_day$Date<-as.factor(precip_2018_by_day$Date)
precip_2018_by_day$Method<-'Precipitation'
precip_2018_by_day$Method2<-'Staff Gauge'
colnames(precip_2018_by_day)[2] <- "Level"
hydrograph_all<-full_join(hydrograph_all, precip_2018_by_day, by=c("Method", "Method2", "Date", "Level"))


hydrograph_averaged<-hydrograph_all %>% 
  group_by(Method, Method2, Date) %>% 
  summarize(Level=mean(Level))





hydrograph_averaged$Method <- factor(hydrograph_averaged$Method, levels=c("Hourly datalogger", "Weekly", "Every Two Weeks",  "Every Four Weeks", "Precipitation"),
                                     labels=c("Daily", "Weekly", "Every Two Weeks",  "Every Four Weeks", "Precipitation"))

#graph

ggplot(data=hydrograph_averaged, mapping=aes(x=Date, y=Level, color=Method, linetype=Method2, group=interaction(Method, Method2)))+
  geom_line(size=1.5)+
  scale_linetype_manual(values=c("dotdash", "solid"))+
  geom_point(color='grey', size=1.25)+
  facet_wrap(~Method, nrow=5)+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=90, size=.5))+
  labs(title="Average Vernal Pool Depth (cm) by Measurement Method, 2017-2018", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(axis.title.y =element_blank())+
  theme(strip.text = element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=25))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.ticks.x =element_line(size=1))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)


# compare depths ----------------------------------------------------------

depth<-full_join(Daily_avg, weekly_avg, by='Pool.ID')
depth<-full_join(depth, biweekly_avg, by='Pool.ID')
depth<-full_join(depth, monthly_avg, by='Pool.ID')

sd(depth$Weekly_Avg_Level)

write.csv(depth, "depth.csv")
avg_depth<-gather(depth, Method,"Level", -Pool.ID)
aov1<-aov(Level~Method+Error(Pool.ID), avg_depth)
aov2<-anova(Level~Method+Error(Pool.ID), avg_depth)
summary(aov1)
TukeyHSD(aov1)
install.packages('lme4'); library('lme4')
m2<-lmer(Level~Method+(1|Pool.ID), data=avg_depth)
summary(anova(m2))
summary(aov(Level~Method+ Error(1/Pool.ID), data = avg_depth))



# compare staff gauge and data logger -------------------------------------------

SG<-staff_gauge_2018                   #Load staff gauge data
DL$Date<-as.factor(DL$Date)
SG$Date<-as.factor(SG$Date)
joined<-full_join(DL, SG, "Date") #Join data logger and staff gaugue
joined[joined < 0] = 0
joined<-joined %>% 
  filter(Date<='2018-03-20')
joined1<-joined


D0<-ggplot(joined, mapping=aes(joined$Date, joined$C2.10.x, group=1, linetype='C2-14 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-10 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
 # scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-10')+
  ylim(0,40)+
  theme(legend.position = 'none')+
  theme(axis.text.y =element_text(size=20))+
 



D01<-ggplot(joined, mapping=aes(joined$Date, joined$C3.12.x, group=1, linetype='C3-12 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C3.12 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5.10')+
  ylim(0,40)+
  theme(legend.position = 'none')+
  theme(axis.text.y =element_text(size=20))






D02<-ggplot(joined, mapping=aes(joined$Date, joined$D5.10.x, group=1, linetype='C2-14 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5.10 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5.10')+
  ylim(0,40)+
  theme(legend.position = 'none')+
  theme(axis.text.y =element_text(size=20))



D03<-ggplot(joined, mapping=aes(joined$Date, joined$C3.13.x, group=1, linetype='C3-13 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C3.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C3.13 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C3-13')+
  ylim(0,40)+
  theme(legend.position = 'none')+
  theme(axis.text.y =element_text(size=20))

D1<-ggplot(joined, mapping=aes(joined$Date, joined$C2.14.x, group=1, linetype='C2-14 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-14 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-14')+
  ylim(0,40)+
  theme(legend.position = 'none')+
  theme(axis.text.y =element_text(size=20))



D2<-ggplot(joined, mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$E5.27.x, group=1, linetype='E5-27 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-17 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-17')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



D3<-ggplot(joined, mapping=aes(joined$Date, joined$C2.18.x, group=1, linetype='C2-18 Datalogger'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C2.18.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-18 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-18')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D4<-ggplot(joined, mapping=aes(joined$Date, joined$C3.16.x, group=1, linetype='C3-16 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C3.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C3-16 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('C3-16')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



D5<-ggplot(joined, mapping=aes(joined$Date, joined$D5.01.x, group=1, linetype='D5-01 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.01.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-01 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-01')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D6<-ggplot(joined, mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='D5-02 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.02.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-02 Staff Gauge", values ="red")+
  # xlim('2017-11-06', '2018-06-01')+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-02')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D7<-ggplot(joined, mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.03.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-03 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-03')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D8<-ggplot(joined, mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='D5-08 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$E5.05.x, group=1, linetype='E5-05 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.08.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-08 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-08')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



D9<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.15.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-15 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D5-15 Datalogger","D5-03 Datalogger"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-15')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



D10<-ggplot(joined, mapping=aes(joined$Date, joined$D5.11.x, group=1, linetype='D5-11 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.11.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-11 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-11')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D11<-ggplot(joined, mapping=aes(joined$Date, joined$D5.13.x, group=1, linetype='D5-13 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-13 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-13')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D12<-ggplot(joined, mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='D5-02 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-14 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-14')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D13<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.15.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-15 Staff Gauge", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-15')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D14<-ggplot(joined, mapping=aes(joined$Date, joined$D5.16.x, group=1, linetype='D5-16 Datalogger'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-16 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-16')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D15<-ggplot(joined, mapping=aes(joined$Date, joined$D5.17.x, group=1, linetype='D5-17 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-17 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-17')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D16<-ggplot(joined, mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Datalogger'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.22.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-22 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D5-22 Datalogger","D5-03 Datalogger"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-22')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D17<-ggplot(joined, mapping=aes(joined$Date, joined$D5.25.x, group=1, linetype='D5-25 Datalogger'))+
  geom_line()+
  geom_line(mapping=aes(joined$Date, joined$D5.25.x, group=1, linetype='D5-25 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.25.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-25 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-25')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D18<-ggplot(joined, mapping=aes(joined$Date, joined$D5.29.x, group=1, linetype='D5-29 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-29 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-29')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D19<-ggplot(joined, mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D5.29.x, group=1, linetype='D5-29 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-30 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-30')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D20<-ggplot(joined, mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Datalogger'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.39.y, color="red"))+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.x =element_blank())+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-39 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D5-39 Datalogger","D5-15 Datalogger"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-39')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



D21<-ggplot(joined, mapping=aes(joined$Date, joined$D6.37.x, group=1, linetype='D6-37 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$C2.18.x, group=1, linetype='C2-18 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D6.37.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-37 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D6-37')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D22<-ggplot(joined, mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D6.37.x, group=1, linetype='D6-37 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D6.61.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-61 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D6-61')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D23<-ggplot(joined, mapping=aes(joined$Date, joined$E5.04.x, group=1, linetype='E5.04 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.04.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-04 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('E5-04')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D24<-ggplot(joined, mapping=aes(joined$Date, joined$E5.05.x, group=1, linetype='E5-05 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.05.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5.05 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('E5-05')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



D25<-ggplot(joined, mapping=aes(joined$Date, joined$D6.40.x, group=1, linetype='D6-40 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D6.40.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-40 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("D6-40 Datalogger","C2-17 Datalogger"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D6-40')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D26<-ggplot(joined, mapping=aes(joined$Date, joined$E5.27.x, group=1, linetype='E5-27 Datalogger'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.27.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-27 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("E5-27 Datalogger","D5-39 Datalogger"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('E5-27')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D27<-ggplot(joined, mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Datalogger'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-29 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('E5-29')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



D28<-ggplot(joined, mapping=aes(joined$Date, joined$E5.30.x, group=1, linetype='E5-30 Datalogger'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='D5-08 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-30 Staff Gauge", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('E5-30')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


D29<-ggplot(joined, mapping=aes(joined$Date, joined$E5.31.x, group=1, linetype='E5-31 Datalogger'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$C2.19, group=1, linetype='C2-19 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.31.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-31 Staff Gauge", values ="red")+
  scale_linetype_manual(labels=c("E5-31 Datalogger","C2-19 Datalogger"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('E5-31')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

D1<-grid.arrange(D1, D2, D3, nrow=1) #3000x466
D2<-grid.arrange(D4, D5, D6, nrow=1) #3000x466
D3<-grid.arrange(D7, D8, D9, nrow=1) #3000x466
D4<-grid.arrange(D10, D11, D12, nrow=1) #3000x466
D5<-grid.arrange(D13, D14, D15, nrow=1) #3000x466
D6<-grid.arrange(D16, D17, D18, nrow=1) #3000x466
D7<-grid.arrange(D19, D20, D21, nrow=1) #3000x466
D8<-grid.arrange(D22, D23, D24, nrow=1) #3000x466
D9<-grid.arrange(D25, D26, D27, nrow=1) #3000x466
D10<-grid.arrange(D28, D29, nrow=1) #3000x466




# compare weekly datalogger to daily datalogger -------------------------------------------


DL$Date<-as.Date(DL$Date)
DL<-DL %>% 
filter(Date<='2018-06-01')
DL$Date<-as.factor(DL$Date)
DL_subset<-DL %>% 
  filter(Date %in% c('2017-11-06','2017-11-13','2017-11-20','2017-11-30',	'2017-12-07',	'2017-12-15',	'2017-12-22',	'2017-12-29',	'2018-01-07',	'2018-01-14',	'2018-01-21',	'2018-01-29',	'2018-02-05',	'2018-02-13', '2018-02-19',	'2018-02-26',	'2018-03-05',	'2018-03-12',	'2018-03-19',	'2018-03-25',	'2018-03-30',	'2018-04-07',	'2018-04-13',	'2018-04-22',	'2018-04-29',	'2018-05-08',	'2018-05-15',	'2018-05-22',	'2018-06-01')) 
DL$Date<-as.factor(DL$Date)
DL_subset$Date<-as.factor(DL_subset$Date)
joined<-full_join(DL, DL_subset, "Date") #Join data logger and staff gaugue
joined[joined < 0] = 0


# Variable Inundation -----------------------------------------------------


DV0<-ggplot(joined, mapping=aes(joined$Date, joined$E5.30.x, group=1, linetype='D5-02 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$E5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-30 Weekly Sampling", values ="red")+
  # xlim('2017-11-06', '2018-06-01')+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('E5-30')+
  theme(legend.position = 'none')+
  ylim(0,40)


DV01<-ggplot(joined, mapping=aes(joined$Date, joined$C3.12.x, group=1, linetype='D5-02 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C3.12.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C3-12 Weekly Sampling", values ="red")+
  # xlim('2017-11-06', '2018-06-01')+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C3-12')+
  theme(legend.position = 'none')+
  ylim(0,40)

DV02<-ggplot(joined, mapping=aes(joined$Date, joined$C3.13.x, group=1, linetype='D5-02 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$C3.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C3-13 Weekly Sampling", values ="red")+
  # xlim('2017-11-06', '2018-06-01')+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C3-13')+
  theme(legend.position = 'none')+
  ylim(0,40)





DV1<-ggplot(joined, mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='D5-02 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Datalogger'))+
  geom_point(mapping=aes(joined$Date, joined$D5.02.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-02 Weekly Sampling", values ="red")+
  # xlim('2017-11-06', '2018-06-01')+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-02')+
  theme(legend.position = 'none')+
  ylim(0,40)



DV2<-ggplot(joined, mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='D5-08 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$E5.05.x, group=1, linetype='E5-05 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.08.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-08 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-08')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DV3<-ggplot(joined, mapping=aes(joined$Date, joined$D5.11.x, group=1, linetype='D5-11 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.11.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-11 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-11')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DV4<-ggplot(joined, mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Daily Sampling'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.02.x, group=1, linetype='D5-02 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-14 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-14')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV5<-ggplot(joined, mapping=aes(joined$Date, joined$D5.16.x, group=1, linetype='D5-16 Daily Sampling'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-16 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-16')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DV6<-ggplot(joined, mapping=aes(joined$Date, joined$D5.17.x, group=1, linetype='D5-17 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-17 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-17')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DV7<-ggplot(joined, mapping=aes(joined$Date, joined$D5.25.x, group=1, linetype='D5-25 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.25.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-25 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-25')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV8<-ggplot(joined, mapping=aes(joined$Date, joined$D5.29.x, group=1, linetype='D5-29 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-29 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-29')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV9<-ggplot(joined, mapping=aes(joined$Date, joined$D5.30.x, group=1, linetype='D5-30 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D5.29.x, group=1, linetype='D5-29 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-30 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-30')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DV10<-ggplot(joined, mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D6.37.x, group=1, linetype='D6-37 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D6.61.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-61 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D6-61')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV11<-ggplot(joined, mapping=aes(joined$Date, joined$E5.04.x, group=1, linetype='E5.04 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$E5.04.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-04 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('E5-04')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV12<-ggplot(joined, mapping=aes(joined$Date, joined$E5.05.x, group=1, linetype='E5-05 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D6.61.x, group=1, linetype='D6-61 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$E5.05.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5.05 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('E5-05')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



DV13<-ggplot(joined, mapping=aes(joined$Date, joined$E5.30.x, group=1, linetype='E5-30 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.08.x, group=1, linetype='D5-08 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$E5.30.y, color="red"), size=2)+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-30 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DV13<-ggplot(joined, mapping=aes(joined$Date, joined$D5.13.x, group=1, linetype='D5-13 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-13 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D5-13')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))





DV14<-ggplot(joined, mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$E5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-29 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('E5-29')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))




DV15<-ggplot(joined, mapping=aes(joined$Date, joined$D6.37.x, group=1, linetype='D6-37 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$C2.18.x, group=1, linetype='C2-18 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D6.37.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-37 Weekly Sampling", values ="red")+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('D6-37')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV16<-ggplot(joined, mapping=aes(joined$Date, joined$C3.16.x, group=1, linetype='C3-16 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$E5.29.x, group=1, linetype='E5-29 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$C3.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C3-16 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  theme(axis.title.y =element_text(size=20))+
  ggtitle('C3-16')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV17<-ggplot(joined, mapping=aes(joined$Date, joined$C2.10.x, group=1, linetype='C2-10 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$C2.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-10 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-10')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV18<-ggplot(joined, mapping=aes(joined$Date, joined$C2.18.x, group=1, linetype='C2-18 Daily Sampling'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.14.x, group=1, linetype='D5-14 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$C2.18.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-18 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-18')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DV_1<-grid.arrange(DV1, DV2, DV3, nrow=1) #3000x466
DV_2<-grid.arrange(DV4, DV5, DV6, nrow=1) #3000x466
DV_3<-grid.arrange(DV7, DV8, DV9, nrow=1) #3000x466
DV_4<-grid.arrange(DV10, DV11, DV12, nrow=1) #3000x466
DV_5<-grid.arrange( DV14, DV15, DV16, nrow=1) #3000x466
DV_6<-grid.arrange(DV17, DV18, nrow=1) #3000x466
DV_7<-grid.arrange(DV01, DV02, nrow=1) #3000x466







# Stable Inundation -------------------------------------------------------



DS1<-ggplot(joined, mapping=aes(joined$Date, joined$C2.14.x, group=1))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$C2.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-14 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-14')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DS2<-ggplot(joined, mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Daily Sampling'))+
  geom_line()+
  #  geom_line(mapping=aes(joined$Date, joined$E5.27.x, group=1, linetype='E5-27 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$C2.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "C2-17 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('C2-17')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))





DS4<-ggplot(joined, mapping=aes(joined$Date, joined$D5.01.x, group=1, linetype='D5-01 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.01.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-01 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-01')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DS5<-ggplot(joined, mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.03.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-03 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-03')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DS6<-ggplot(joined, mapping=aes(joined$Date, joined$D5.10.x, group=1, linetype='D5-10 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-10 Weekly Sampling", values ="red")+
  scale_linetype_manual(labels=c("D5-10 Daily Sampling","D5-03 Daily Sampling"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-10')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DS7<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.15.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-15 Weekly Sampling", values ="red")+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-15')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DS8<-ggplot(joined, mapping=aes(joined$Date, joined$D5.22.x, group=1, linetype='D5-22 Daily Sampling'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.03.x, group=1, linetype='D5-03 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.22.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-22 Weekly Sampling", values ="red")+
  scale_linetype_manual(labels=c("D5-22 Daily Sampling","D5-03 Daily Sampling"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-22')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DS9<-ggplot(joined, mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Daily Sampling'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.15.x, group=1, linetype='D5-15 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D5.39.y, color="red"))+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.title.x =element_blank())+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D5-39 Weekly Sampling", values ="red")+
  scale_linetype_manual(labels=c("D5-39 Daily Sampling","D5-15 Daily Sampling"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D5-39')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))


DS10<-ggplot(joined, mapping=aes(joined$Date, joined$D6.40.x, group=1, linetype='D6-40 Daily Sampling'))+
  geom_line()+
  # geom_line(mapping=aes(joined$Date, joined$C2.17.x, group=1, linetype='C2-17 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$D6.40.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "D6-40 Weekly Sampling", values ="red")+
  scale_linetype_manual(labels=c("D6-40 Daily Sampling","C2-17 Daily Sampling"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('D6-40')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))



DS11<-ggplot(joined, mapping=aes(joined$Date, joined$E5.27.x, group=1, linetype='E5-27 Daily Sampling'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$D5.39.x, group=1, linetype='D5-39 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$E5.27.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-27 Weekly Sampling", values ="red")+
  scale_linetype_manual(labels=c("E5-27 Daily Sampling","D5-39 Daily Sampling"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('E5-27')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))

DS12<-ggplot(joined, mapping=aes(joined$Date, joined$E5.31.x, group=1, linetype='E5-31 Daily Sampling'))+
  geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$C2.19, group=1, linetype='C2-19 Daily Sampling'))+
  geom_point(mapping=aes(joined$Date, joined$E5.31.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_color_manual(name="", labels = "E5-31 Weekly Sampling", values ="red")+
  scale_linetype_manual(labels=c("E5-31 Daily Sampling","C2-19 Daily Sampling"), values=c("solid", "dashed"))+
  theme(axis.title.x =element_blank())+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('E5-31')+
  theme(legend.position = 'none')+
  ylim(0,40)+
  theme(axis.text.y =element_text(size=20))


DS_1<-grid.arrange(DS1, DS2, DS4, nrow=1) #3000x466
DS_2<-grid.arrange(DS5, DS6, DS7, nrow=1) #3000x466
DS_3<-grid.arrange(DS8, DS9, DS10, nrow=1) #3000x466
DS_4<-grid.arrange(DS11, DS12,DV13, nrow=1) #3000x466





# Counting drydown/wetup events -------------------------------------------
#variable_wetups<-  c(1,5,2,1,1,2,6,1,1,3,3,1,3,3,3,1,2,5,6,3) old calc
variable_drydowns<- c(0,1,1,1,0,1,0,0,1,1,2,0,0,0,0,1,2,1,2,1,0,0,0)
variable_wetups<-   c(1,0,2,6,3,0,5,2,0,0,0,1,1,5,0,0,1,0,0,0,3,5,3)
total_variable<-    c(1,1,3,7,3,1,5,2,1,1,2,1,1,5,0,1,3,1,2,1,3,5,3)

#stable_wetups<-c(0,1,0,0,0,0,0,0,0,1,0,1) old calc

stable_drydowns<-c(0,1,0,0,0,0,0,1,0)
stable_wetups<-  c(0,0,0,0,0,1,0,0,0)
total_stable<- c(0,1,0,0,0,1,0,1,0)


variable_wetups<-tibble(event=variable_wetups, type='Variable', type2='Wetup')
stable_wetups<-tibble(event=stable_wetups, type='Stable', type2='Wetup')
wetups<-full_join(variable_wetups, stable_wetups)

variable_drydowns<-tibble(event=variable_drydowns, type='Variable', type2='Drydown')
stable_drydowns<-tibble(event=stable_drydowns, type='Stable', type2='Drydown')
drydowns<-full_join(variable_drydowns, stable_drydowns)

all_events<-full_join(drydowns, wetups)

library(ggplot2)
ggplot(all_events, aes(fill=type2))+
  geom_histogram(aes(x=event), binwidth=1)+
  labs(y="Count", x="# Wet-up/Dry-down events missed by weekly sampling")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(strip.text  =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(axis.text.x =element_text(size=30))+
  theme(axis.text.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  xlim(.5,6)+
  #scale_x_continuous(breaks=seq(1,6,1))+
  facet_wrap(~type)



# size difference ----------------------------------------------------------
t.test(Pool.Area..m2.~Inundation.Type, data=char)
summary(aov(Pool.Area..m2.~Inundation.Type, data=char))

char<-char[-c(32:33),]
size_v_var<-ggplot(data=char, aes(x=Inundation.Type, y=Pool.Area..m2.))+
  geom_boxplot()+
  labs(x="", y="Pool Area (m2)")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(strip.text  =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(axis.text.x =element_text(size=30))+
  theme(axis.text.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))


# cumulative hydroperiod difference ----------------------------------------------------------
t.test(Cumulative.Hydroperiod~Inundation.Type, data=char)
summary(aov(Cumulative.Hydroperiod~Inundation.Type, data=char))

cumul_v_var<-ggplot(data=char, aes(x=Inundation.Type, y=Cumulative.Hydroperiod))+
  geom_boxplot()+
  labs(title="Inundation Type by Pool Area", x="Inundation Type", y="Cumulative Hydroperiod (days)")+
  theme(plot.title=element_text(size=35, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(strip.text  =element_text(size=30))+
  theme(axis.title.y =element_text(size=25))+
  theme(axis.text.x =element_text(size=30))+
  theme(axis.text.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  ylim(0,100)

# cumulative  hydroperiod error difference ----------------------------------------------------------
t.test(Cum_abs_difference_weekly_. ~ Inundation.Type, data=char)
summary(aov(Cum_abs_difference_weekly_.~Inundation.Type, data=char))

cum_v_var_percent_error<-ggplot(data=char, aes(x=Inundation.Type, y=Cum_abs_difference_weekly_.))+
  geom_boxplot()+
  labs(title="% Error in Daily vs. Weekly Measurement\nof Cumlative Hydroperiod", x="Inundation Type", y="% absolute difference")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(strip.text  =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(axis.text.x =element_text(size=30))+
  theme(axis.text.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  ylim(0,100)



# continuous  hydroperiod difference ----------------------------------------------------------
t.test(Continuous.Hydroperiod~Inundation.Type, data=char)
summary(aov(Continuous.Hydroperiod~Inundation.Type, data=char))

contin_v_var<-ggplot(data=char, aes(x=Inundation.Type, y=Continuous.Hydroperiod))+
  geom_boxplot()+
  labs(x="", y="Max. Continuous Hydroperiod (days)")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(strip.text  =element_text(size=30))+
  theme(axis.title.y =element_text(size=25))+
  theme(axis.text.x =element_text(size=30))+
  theme(axis.text.y =element_text(size=20))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  ylim(0,100)


# continuous  hydroperiod error difference ----------------------------------------------------------
t.test(Con_abs_difference_weekly_. ~ Inundation.Type, data=char)
summary(aov(Con_abs_difference_weekly_.~Inundation.Type, data=char))

contin_v_var_percent_error<-ggplot(data=char, aes(x=Inundation.Type, y=Con_abs_difference_weekly_.))+
  geom_boxplot()+
  labs(title="% Error in Daily vs. Weekly Measurement\nof Maximum Continuous Hydroperiod", x="Inundation Type", y="% absolute difference")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(strip.text  =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(axis.text.x =element_text(size=30))+
  theme(axis.text.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  ylim(0,100)





# Redo All Hydrographs ----------------------------------------------------


P1<-ggplot(joined, mapping=aes(joined$Date, joined$C2.10.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C2.10.y, color="red"))+
  geom_line(joined1, mapping=aes(joined1$Date, joined1$C2.10.x, group=1, linetype='C2-10 Staff Gauge'))+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 1')+
  theme(legend.position = 'none')+
  ylim(0,40)

P2<-ggplot(joined, mapping=aes(joined$Date, joined$C2.14.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C2.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 2')+
  theme(legend.position = 'none')+
  ylim(0,40)

P3<-ggplot(joined, mapping=aes(joined$Date, joined$C2.17.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C2.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 3')+
  theme(legend.position = 'none')+
  ylim(0,40)

P4<-ggplot(joined, mapping=aes(joined$Date, joined$C2.18.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C2.18.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 4')+
  theme(legend.position = 'none')+
  ylim(0,40)

P5<-ggplot(joined, mapping=aes(joined$Date, joined$C3.12.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C3.12.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 5')+
  theme(legend.position = 'none')+
  ylim(0,40)

P6<-ggplot(joined, mapping=aes(joined$Date, joined$C3.13.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C3.13.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 6')+
  theme(legend.position = 'none')+
  ylim(0,40)

P7<-ggplot(joined, mapping=aes(joined$Date, joined$C3.16.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$C3.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 7')+
  theme(legend.position = 'none')+
  ylim(0,40)

P8<-ggplot(joined, mapping=aes(joined$Date, joined$D5.01.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.01.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 8')+
  theme(legend.position = 'none')+
  ylim(0,40)

P9<-ggplot(joined, mapping=aes(joined$Date, joined$D5.02.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.02.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 9')+
  theme(legend.position = 'none')+
  ylim(0,40)

P10<-ggplot(joined, mapping=aes(joined$Date, joined$D5.03.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.03.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 10')+
  theme(legend.position = 'none')+
  ylim(0,40)

P11<-ggplot(joined, mapping=aes(joined$Date, joined$D5.08.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.08.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 11')+
  theme(legend.position = 'none')+
  ylim(0,40)

P12<-ggplot(joined, mapping=aes(joined$Date, joined$D5.10.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.10.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 12')+
  theme(legend.position = 'none')+
  ylim(0,40)

P13<-ggplot(joined, mapping=aes(joined$Date, joined$D5.11.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.11.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 13')+
  theme(legend.position = 'none')+
  ylim(0,40)

P15<-ggplot(joined, mapping=aes(joined$Date, joined$D5.14.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.14.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 15')+
  theme(legend.position = 'none')+
  ylim(0,40)

P16<-ggplot(joined, mapping=aes(joined$Date, joined$D5.15.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.15.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 16')+
  theme(legend.position = 'none')+
  ylim(0,40)

P17<-ggplot(joined, mapping=aes(joined$Date, joined$D5.16.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.16.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 17')+
  theme(legend.position = 'none')+
  ylim(0,40)

P18<-ggplot(joined, mapping=aes(joined$Date, joined$D5.17.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.17.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 18')+
  theme(legend.position = 'none')+
  ylim(0,40)

P19<-ggplot(joined, mapping=aes(joined$Date, joined$D5.22.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.22.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 19')+
  theme(legend.position = 'none')+
  ylim(0,40)

P20<-ggplot(joined, mapping=aes(joined$Date, joined$D5.25.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.25.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 20')+
  theme(legend.position = 'none')+
  ylim(0,40)

P21<-ggplot(joined, mapping=aes(joined$Date, joined$D5.29.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 21')+
  theme(legend.position = 'none')+
  ylim(0,40)

P22<-ggplot(joined, mapping=aes(joined$Date, joined$D5.30.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 22')+
  theme(legend.position = 'none')+
  ylim(0,40)

P23<-ggplot(joined, mapping=aes(joined$Date, joined$D5.39.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D5.39.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 23')+
  theme(legend.position = 'none')+
  ylim(0,40)

P24<-ggplot(joined, mapping=aes(joined$Date, joined$D6.37.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D6.37.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 24')+
  theme(legend.position = 'none')+
  ylim(0,40)

P25<-ggplot(joined, mapping=aes(joined$Date, joined$D6.40.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D6.40.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 25')+
  theme(legend.position = 'none')+
  ylim(0,40)

P26<-ggplot(joined, mapping=aes(joined$Date, joined$D6.61.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$D6.61.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 26')+
  theme(legend.position = 'none')+
  ylim(0,40)

P27<-ggplot(joined, mapping=aes(joined$Date, joined$E5.04.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.04.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 27')+
  theme(legend.position = 'none')+
  ylim(0,40)

P28<-ggplot(joined, mapping=aes(joined$Date, joined$E5.05.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.05.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 28')+
  theme(legend.position = 'none')+
  ylim(0,40)

P29<-ggplot(joined, mapping=aes(joined$Date, joined$E5.27.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.27.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 29')+
  theme(legend.position = 'none')+
  ylim(0,40)

P30<-ggplot(joined, mapping=aes(joined$Date, joined$E5.29.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.29.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 30')+
  theme(legend.position = 'none')+
  ylim(0,40)

P31<-ggplot(joined, mapping=aes(joined$Date, joined$E5.30.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.30.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 31')+
  theme(legend.position = 'none')+
  ylim(0,40)

P32<-ggplot(joined, mapping=aes(joined$Date, joined$E5.31.x, group=1))+
  geom_line()+
  geom_point(mapping=aes(joined$Date, joined$E5.31.y, color="red"))+
  geom_hline(yintercept=0, linetype="dashed", color="grey")+
  labs(y="Depth (cm)", x="Date", size=3)+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.text.y =element_text(size=20))+
  theme(axis.title.x =element_blank())+
  theme(legend.title = element_blank())+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  scale_x_discrete(breaks=DL_hydrograph_model$Date)+
  theme(axis.text.x =element_text(size=15, angle=60, hjust=1))+
  ggtitle('Pool 32')+
  theme(legend.position = 'none')+
  ylim(0,40)
