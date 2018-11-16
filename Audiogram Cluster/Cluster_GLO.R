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
# install.packages('readr')
# install.packages('xlsx')
# install.packages('zoo')
# install.packages('class')


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
library('class')


#data path
data.path <- "C:/Users/j5e4/Documents/MATLAB/BEAR/Bisgaard Standard Audiograms/"

#load data
data.left <- read.xlsx(paste0(data.path,"AC_L_DATA.xlsx"),1)
data.right <- read.xlsx(paste0(data.path,"AC_R_DATA.xlsx"),1)
colnames(data.left)[c(6,7,9,11,13,14)]<-c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
colnames(data.right)[c(6,7,9,11,13,14)]<-c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')

data.reference <- read.xlsx(paste0(data.path,"Bisgaard.xlsx"),1)

data.reference <- data.reference %>% select(c(4,6,7,9,11,13,14))
colnames(data.reference)[c(1:7)] <- c('SA','0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')


#frequencies 250, 500, 1000, 2000, 4000, 6000
data.left.audiogram <- data.left %>% select(c(3,4,6,7,9,11,13,14))
data.right.audiogram <- data.right %>% select(c(3,4,6,7,9,11,13,14))

#handling NAs for the left ear
data.left.onlyaudio <- data.left.audiogram %>% select(c(3,4,5,6,7,8))
data.left.onlyaudio[data.left.onlyaudio=="NA"] <- NA
nn = rowSums(is.na(data.left.onlyaudio)) >= 3   # Tracks NAs and sums where 50% is missing
data.left.audiogram<-data.left.audiogram[!nn,]  # Delete rows where 50% is missing
data.left.audiogram <- as.data.frame(na.fill(na.approx(data.left.audiogram),'extend')) #interpolate NAs (linear)

#handling NAs for the right ear
data.right.onlyaudio <- data.right.audiogram %>% select(c(3,4,5,6,7,8))
data.right.onlyaudio[data.right.onlyaudio=="NA"] <- NA
mm = rowSums(is.na(data.right.onlyaudio)) >= 3
data.right.audiogram<-data.right.audiogram[!mm,]
data.right.audiogram <- as.data.frame(na.fill(na.approx(data.right.audiogram),'extend'))

#distance (or (dis)similarity) between each pair of observations
#data.right.onlyaudio <- data.right.audiogram %>% select(c(3,4,5,6,7,8))
#data.scale <- scale(data.right.onlyaudio) # scaling/standardizing data to not depend to an arbitrary variable 
#distance_right <- get_dist(data.scale)
#fviz_dist(distance_right, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#k-means
#number of cluster = 10
set.seed(1234) # Starting point in the generation of a sequence of random numbers 
fit.km.10 <- data.right.audiogram %>% select(-starts_with(c('Age'))) %>% 
  select(-starts_with(c('Gender'))) %>% kmeans(10,nstart=25)

centers.melted.10 <- fit.km.10$centers %>% melt
names(centers.melted.10) <- c('clusterNr','Frequency','HL')


ggplot(data=centers.melted.10,aes(x=Frequency,y=HL,group=1))  + 
  geom_line(color='blue') +
  geom_point(color='blue') +
  facet_wrap(~clusterNr) 


#plot standard audiograms
centers.standard <- data.reference %>% melt
names(centers.standard) <- c('clusterNr','Frequency','HL')

ggplot(data=centers.standard,aes(x=Frequency,y=HL,group=1))  + 
  geom_line(color='blue') +
  geom_point(color='blue') +
  facet_wrap(~clusterNr) 


#plot both 

centers.melted.10$clusterNr[centers.melted.10$clusterNr==1] <- c('N1')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==2] <- c('N3')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==8] <- c('N2')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==4] <- c('N6')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==5] <- c('N4')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==10] <- c('N5')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==3] <- c('N7')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==9] <- c('S1')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==6] <- c('S2')
centers.melted.10$clusterNr[centers.melted.10$clusterNr==7] <- c('S3')


df <- bind_rows(list(Standard_Audiograms=centers.standard,BEAR_kmeans =centers.melted.10),
                .id = 'source')

ggplot(data=df, aes(x=Frequency , y=HL, color=source, group=source)) +
  geom_point() +
  geom_line() +
  geom_line() +
  scale_color_manual(values=c('red','blue')) +
  facet_wrap(~clusterNr) +
  scale_y_reverse(lim=c(120,-10))
