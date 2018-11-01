
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
#install.packages('class')
install.packages('partykit')

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
library('partykit')


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



#Learning Vector Quantization
library('class')

cd <-  data.audiogram %>% select(-starts_with(c('Age'))) %>% 
  select(-starts_with(c('Gender'))) %>% lvqinit(factor(fit.km.10$cluster),k=1)

cd1 <- data.audiogram %>% select(-starts_with(c('Age'))) %>% 
  select(-starts_with(c('Gender'))) %>% olvq1(factor(fit.km.10$cluster),cd)

tmp <- data.frame(cd1$x,cd1$cl)

centers.lvq <- tmp %>% group_by(cd1.cl) %>% summarize_all(mean)



levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==1] <- c('N1')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==2] <- c('S1')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl == 8]  <- c('N2')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==4] <- c('N7')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==5] <- c('S2')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==10] <- c('N5')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==3] <- c('N4')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==9] <- c('S3')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==6] <- c('N6')
levels(centers.lvq$cd1.cl)[centers.lvq$cd1.cl==7] <- c('N3')


colnames(centers.lvq) <- c('cluster','0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')


centers.lvq.10 <- centers.lvq %>% melt
names(centers.lvq.10) <- c('clusterNr','frequency','HL')

df1 <- bind_rows(list(Standard_Audiograms=centers.standard,Vector_Quantization =centers.lvq.10),
                .id = 'source')


#plot 
ggplot(data= df1, aes(x=frequency, y=HL, color=source, group=source)) +
  geom_point() +
  geom_line() +
  geom_line() +
  scale_color_manual(values=c('blue','red')) +
  facet_wrap(~clusterNr) 

Percentage <-  100*fit.km.10$size/sum(fit.km.10$size)


St.Audiograms <- c('N1','S1','N4','N7','S2','N6','N3','N2','S3','N5')
distr <- data.frame(St.Audiograms,Percentage)

#relative distribution of clusters 
#[1]  7.343192 13.768485 14.660887  1.733809 10.632330  5.787863 15.731770 15.680775  4.130546 10.530342


ggplot(data=distr,aes(x=St.Audiograms,y=Percentage)) +
         geom_bar(stat='identity',width=0.7, fill = 'blue') + 
         scale_y_continuous(limits = c(0, 30)) + 
         xlab('Audiogram class') +
         ylab('Percentage of all data')


# Assign Audiograms 

reference <- data.reference[,2:7]
target <- data.audiogram[,3:8]
classVector <- data.reference[,1]


rowReference <- nrow(reference)
rowTarget <- nrow(target)
  
tmp <- rep(0,rowReference)
cls <- rep(0,rowTarget)
  
for (idx in 1:rowTarget){
  for (ldx in 1:rowReference){
    tmp[ldx] <- sqrt(1/rowReference * sum((target[idx,] - reference[ldx,])^2))
  }
  cls[idx] <- which.min(tmp)
}
 
clusterAud <- classVector[cls]


audiogram.left <- data.frame(data.left.audiogram,clusterAud[1:1961])
audiogram.right <- data.frame(data.right.audiogram,clusterAud[1962:3922]) 

audiogram.left <- data.frame(audiogram.left,data.left[,1])
audiogram.right <- data.frame(audiogram.right,data.right[,1])

colnames(audiogram.left)[9:10] <- c('Class','ID')
colnames(audiogram.right)[9:10] <- c('Class','ID')

# write.csv(audiogram.left,file='Audiogram_left_merged.csv')
# write.csv(audiogram.right,file='Audiogram_right_merged.csv')

#N1 <- audiogram.left[audiogram.left[,9]== 'N1',]
mean_aud.left <- audiogram.left[,3:8] %>% aggregate(by=list(audiogram.left$Class),FUN=mean) 
std_aud.left <- audiogram.left[,3:8] %>% aggregate(by=list(audiogram.left$Class),FUN=sd)

mean_aud.right <- audiogram.right[,3:8] %>% aggregate(by=list(audiogram.right$Class),FUN=mean)
std_aud.right <- audiogram.right[,3:8] %>% aggregate(by=list(audiogram.right$Class),FUN=sd)

colnames(audiogram.left)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
colnames(audiogram.right)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')

audiogram.left[,3:8] <- -1 * audiogram.left[,3:8]
audiogram.right[,3:8] <- -1 * audiogram.right[,3:8]

left <- melt(audiogram.left[,-c(1,2)],by=c('Class'))
right <- melt(audiogram.right[,-c(1,2)],by=c('Class'))
names(left) <- c('clusterNr','ID','frequency','HL')
names(right) <- c('clusterNr','ID','frequency','HL')

# N1 <- audiogram.left[audiogram.left$Class == 'N1',]
# colnames(N1)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# N1 <- melt(N1,by=c('Class'))
# N2 <- audiogram.left[audiogram.left$Class == 'N2',]
# colnames(N2)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# N2 <- melt(N2)
# N3 <- audiogram.left[audiogram.left$Class == 'N3',] 
# colnames(N3)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# N3 <- melt(N3)
# N4 <- audiogram.left[audiogram.left$Class == 'N4',] 
# colnames(N4)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# N4 <- melt(N4)
# N5 <- audiogram.left[audiogram.left$Class == 'N5',] 
# colnames(N5)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# N5 <- melt(N5)
# N6 <- audiogram.left[audiogram.left$Class == 'N6',] 
# colnames(N6)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# N6 <- melt(N6)
# N7 <- audiogram.left[audiogram.left$Class == 'N7',] 
# colnames(N7)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# N7 <- melt(N7)
# S1 <- audiogram.left[audiogram.left$Class == 'S1',] 
# colnames(S1)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# S1 <- melt(S1)
# S2 <- audiogram.left[audiogram.left$Class == 'S2',] 
# colnames(S2)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# S2 <- melt(S2)
# S3 <- audiogram.left[audiogram.left$Class == 'S3',] 
# colnames(S3)[3:8] <- c('0.25kHz','0.5kHz','1kHz','2kHz','4kHz','6kHz')
# S3 <- melt(S3)
# 


df.left <- bind_rows(list(Standard_Audiograms=centers.standard,BEAR_audiograms =left),
                 .id = 'source')

#plot those 
#left ear
ggplot(data=left,aes(x=frequency,HL)) + 
  geom_line(color='red',aes(group=ID),alpha=0.2) + 
  geom_line(data=centers.standard,aes(x=frequency,HL,
                                      group=clusterNr),color='blue',size=1.5)+
  facet_wrap(~clusterNr) +
  labs(title='Left ear')
  
#right ear
ggplot(data=right,aes(x=frequency,HL)) + 
  geom_line(color='red',aes(group=ID),alpha=0.2) + 
  geom_line(data=centers.standard,aes(x=frequency,HL,
                                      group=clusterNr),color='blue',size=1.5)+
  facet_wrap(~clusterNr)+
  labs(title='Right ear')

len <- matrix(nrow=10,ncol=2,rep(0,20))
bla <- c('N1','N2','N3','N4','N5','N6','N7','S1','S2','S3')
for (idx in 1:10) {
  len[idx,1] <- dim(audiogram.left[audiogram.left$Class == bla[idx],])[1]
  len[idx,2] <- dim(audiogram.right[audiogram.right$Class == bla[idx],])[1]
}


audiogram.left$Gender <- as.factor(audiogram.left$Gender)
audiogram.right$Gender <- as.factor(audiogram.right$Gender)

levels(audiogram.left$Gender) <- c('F','M')
levels(audiogram.right$Gender) <- c('F','M')

#classification tree left
fit.tree.left <- ctree(Age + Gender ~ Class, data = audiogram.left)
plot(fit.tree.left,main='Classification based on age and gender left ear')

#classification tree right
fit.tree.right <- ctree(AGE + Gender ~ Class, data = audiogram.right)
plot(fit.tree.right,main='Classification based on age and gender right ear')









