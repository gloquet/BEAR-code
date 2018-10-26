
# #install packages
# install.packages("dplyr")
# install.packages("stats")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("reshape2")
# install.packages("NbClust")
# install.packages('rpart')
# install.packages('partykit')
# install.packages('rpart.plot')
# install.packages('rattle')
#install.packages('readr')
#install.packages('xlsx')
#install.packages('zoo')


#load packages
library("dplyr")
library("stats")
library("ggplot2")
library("gridExtra")
library("reshape2")
library("NbClust")
library('rpart')
library('partykit')
library('rpart.plot')
library('rattle')
library('readr')
library('xlsx')
library('zoo')


#data path
data.path <- "../../Data/Audiogram/"

#load data
data.left <- read.xlsx(paste0(data.path,"AC_L_DATA.xlsx"),1)
data.right <- read.xlsx(paste0(data.path,"AC_R_DATA.xlsx"),1)

data.reference <- read.xlsx(paste0(data.path,"Bisgaard.xlsx"),1)

data.reference <- data.reference %>% select(c(4,6,7,9,11,13,14))
colnames(data.reference)[c(1:7)] <- c('SA','0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz','8kHz')
data.reference[,2:7] <- -1 * data.reference[,2:7]


#frequencies 250, 500, 1000, 2000, 4000, 6000, 8000
data.left.audiogram <- data.left %>% select(c(2,3,5,6,8,10,12,13))
data.right.audiogram <- data.right %>% select(c(2,3,5,6,8,10,12,13))

#interpolate NAs
data.left.audiogram <- na.fill(na.approx(data.left.audiogram),'extend')
data.right.audiogram <- na.fill(na.approx(data.right.audiogram),'extend')

#bind left and right
data.audiogram <- as.data.frame(rbind(data.left.audiogram,data.right.audiogram))
colnames(data.audiogram)[c(3:8)] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')

#make thresholds negative for hearing loss (HL)
data.audiogram[,3:8] <- -1 * data.audiogram[,3:8] 

#k-means
#number of cluster = 11
set.seed(1234)
fit.km.10 <- data.audiogram %>% select(-starts_with(c('Age'))) %>% 
  select(-starts_with(c('Gender'))) %>% kmeans(10,nstart=25)


centers.melted.10 <- fit.km.10$centers %>% melt
names(centers.melted.10) <- c('clusterNr','frequency','HL')


ggplot(data=centers.melted.10,aes(x=frequency,y=HL,group=1))  + 
  geom_line(color='blue') +
  geom_point(color='blue') +
  facet_wrap(~clusterNr) 




#plot standard audiograms
centers.standard <- data.reference %>% melt
names(centers.standard) <- c('clusterNr','frequency','HL')

ggplot(data=centers.standard,aes(x=frequency,y=HL,group=1))  + 
  geom_line(color='blue') +
  geom_point(color='blue') +
  facet_wrap(~clusterNr) 


#plot both 

centers.melted.10$clusterNr[centers.melted.10$clusterNr==1] <- c('N1')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==2] <- c('S1')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==8] <- c('N2')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==4] <- c('N7')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==5] <- c('S2')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==10] <- c('N5')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==3] <- c('N4')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==9] <- c('S3')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==6] <- c('N6')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==7] <- c('N3')


df <- bind_rows(list(Standard_Audiograms=centers.standard,BEAR_kmeans =centers.melted.10),
                .id = 'source')


ggplot(data= df, aes(x=frequency, y=HL, color=source, group=source)) +
         geom_point() +
         geom_line() +
         geom_line() +
         scale_color_manual(values=c('blue','red')) +
         facet_wrap(~clusterNr) 
        



# ggplot() +
#   geom_line(data=centers.standard,aes(x=frequency,y=HL,group=1),color='blue') +
#   geom_point(data=centers.standard,aes(x=frequency,y=HL),color='blue') +
#   facet_wrap(~clusterNr) +
#   geom_line(data=centers.melted.10,aes(x=frequency,y=HL,group=2),color='red') +
#   geom_point(data=centers.melted.10,aes(x=frequency,y=HL),color='red') +
#   facet_wrap(~clusterNr) 









