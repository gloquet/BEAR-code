

#Tobias Piechowiak

library('ggplot2')
library('partykit')
library('rpart.plot')
library('rpart')

path <- '../../Data/data/'

data_time <- read.csv(paste0(path,'Tid_mellem_udlevering_og_andet_besoeg.csv'))
data_SSQ_1 <- read.csv(paste0(path,'SSQ_1.csv'))
data_SSQ_2 <- read.csv(paste0(path,'SSQ_2.csv'))
data_SSQ_age_gender <- read.csv(paste0(path,'SSQ_age_gender.csv'))
data_experience <- read.csv(paste0(path,'HA_experience.csv'))

audiogram <- read.csv2(paste0(path,'audiogramdata.csv'))




#processing date 
data_time$Time_diff_visits <- as.numeric(as.Date(data_time[,4]) - as.Date(data_time[,3]))


#processing SSQ
data_SSQ <- merge(data_SSQ_1,data_SSQ_2,by = c('record_id'))
data_SSQ <- merge(data_SSQ,data_SSQ_age_gender,by = c('record_id'))
data_SSQ <- data_SSQ[,-c(2,40)]



#merge time and SSQ 
data_1 <- merge(data_SSQ,data_time,by = c('record_id'))


#mean across SSQs
data_1 <- transform(data_1,SSQ1 = rowMeans(data_1[,seq(2,18,1)],na.rm = TRUE))
data_1 <- transform(data_1,SSQ2 = rowMeans(data_1[,seq(21,37,1)],na.rm = TRUE))

data_1$diff_SSQ <- data_1$SSQ2 - data_1$SSQ1

data_1$sex <- as.factor(data_1$sex)
data_1$rand_apparat <- as.factor(data_1$rand_apparat)
data_1$own_ha <- as.factor(data_1$own_ha)
data_1$ha_dispencer <- as.factor(data_1$ha_dispencer)


data_1 <- merge(data_1,data_experience,by = c('record_id'))

names(data_1)[names(data_1) == 'age.x'] <- 'age'
names(data_1)[names(data_1) == 'sex.x'] <- 'sex'

#plot some shit 
ggplot(data_1, aes(diff_SSQ)) + 
  geom_histogram(data = subset(data_1,rand_apparat == 1), fill = 'red', alpha = 0.7, binwidth = 4,na.rm = TRUE) +
  geom_histogram(data = subset(data_1,rand_apparat == 2), fill = 'green', alpha = 0.7, binwidth = 4,na.rm = TRUE) +
  geom_histogram(data = subset(data_1,rand_apparat == 3), fill = 'blue', alpha = 0.7, binwidth = 4,na.rm = TRUE) +
  facet_grid(~ rand_apparat) +
  geom_vline(xintercept=c(0), color = 'black') 


ggplot(data_1, aes(x=Time_diff_visits, y=diff_SSQ)) + 
  geom_point() + stat_smooth(method=lm)


ggplot(data_1, aes(diff_SSQ)) + 
  geom_histogram(data = subset(data_1,sex.x == 1),fill='blue',alpha = 0.7, binwidth = 4,na.rm = TRUE) + 
  geom_histogram(data = subset(data_1,sex.x == 2),fill = 'red', alpha = 0.7, binwidth = 4,na.rm = TRUE) +
  geom_vline(xintercept = c(0), color='black') +
  facet_grid(~sex)


ggplot(data_1, aes(x=age.x, y=diff_SSQ)) + 
  geom_point() + 
  stat_smooth(method=lm)


#tree model 
fit_tree <- rpart(diff_SSQ ~ sex + rand_apparat + age + Time_diff_visits + SSQ1 + ha_use_time + ha_usetime_hours_per_day + 
                    own_ha + ha_dispencer, data = data_1)
rpart.plot(fit_tree, type=2,clip.right.labs = TRUE, branch=1,yesno=2)




#save csv 
write.csv(data_1,'SSQ_age_gender_TimeVists_manufacturers.csv')

