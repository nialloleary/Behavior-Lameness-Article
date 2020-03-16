#Start----
library(broom); library(QuantPsyc); library(ggplot2)3

PoolA<-rbind.data.frame(SumDataList[[1]],SumDataList[[3]],
                        SumDataList[[4]],# 3.a
                        SumDataList[[6]]) #4a
PoolB<-rbind.data.frame(SumDataList[[2]],SumDataList[[3]],
                        SumDataList[[5]],# 3.b
                        SumDataList[[7]]) #4b

PoolA<-PoolA[,1:16]
PoolB<-PoolB[,1:16]

#T test between 0 and 2 ----
Pool0<-PoolA[PoolA$loco==0,]
Pool2<-PoolA[PoolA$loco==2,]

Tres <- data.frame(matrix(ncol = 2, nrow = 15))
for (i in 2:15){
  iRes<-t.test(Pool0[,i],Pool2[i,])
  Tres[i,1]<-iRes$p.value
}
 #4, 5,6,8,10,11

Pool0<-PoolB[PoolB$loco==0,]
Pool2<-PoolB[PoolB$loco==2,]

for (i in 2:15){
  iRes<-t.test(Pool0[,i],Pool2[i,])
  Tres[i,2]<-iRes$p.value
}
options(scipen=999)

rownames(Tres)<- colnames(PoolA)[2:16]
Tres$av<-(Tres$X1+Tres$X2)/2
#5, 6,8, 9,10
#Both 5 Standup,6 Laydown,8 Laying index,10 walking index 
names(PoolB)

#lm analysis----
#create a table and attached standarised coefficient
#create a table and attached standarised coefficient
PTa<- data.frame(matrix(ncol = 3, nrow = 14))
PTb<- data.frame(matrix(ncol = 3, nrow = 14))
colnames(PTa)<-c('var','Estimate','pval')
colnames(PTb)<-c('var','Estimate','pval')

#PoolA<-PoolA[(PoolA$loco!=1),]
#PoolB<-PoolB[(PoolB$loco!=1),] # binary analysis 0/2 still insignificant. For A Standing time becomes significant, laying counter doesn't 

for (i in 2:15){
  PTa[i-1,1]<-(names(PoolA[i]))
  mod<-tidy((glm(scale(PoolA$loco)~scale(PoolA[,i])+as.factor(PoolA$Lse))))# standardised coefficients
  
  PTa[i-1,2]<-(mod$estimate[2])
  
  PTa[i-1,3]<-(mod$p.value[2])
}

for (i in 2:15){
  PTb[i-1,1]<-(names(PoolB[i]))
  mod<-tidy((glm(scale(PoolB$loco)~scale(PoolB[,i])+as.factor(PoolB$Lse))))# standardised coefficients
  
  PTb[i-1,2]<-(mod$estimate[2])
  
  PTb[i-1,3]<-(mod$p.value[2])
}

PTc<-cbind.data.frame(PTa,PTb)
setwd(home)
write.csv(x = PTc,file = "Table4 Aggregated.csv")

summary(lm(PoolA$loco~PoolA$STANDUP+as.factor(PoolA$Lse)))
summary(lm(PoolA$loco~PoolA$LAYINGCOUNTER+as.factor(PoolA$Lse)))
cor.test(PoolA$STANDUP,PoolA$LAYINGCOUNTER)
summary(lm(PoolB$loco~PoolB$ACTIVITY+as.factor(PoolB$Lse)))
summary(lm(PoolB$loco~PoolB$STANDINGINDEX+as.factor(PoolB$Lse)))
cor.test(PoolB$STANDINGINDEX,PoolB$ACTIVITY) # weak association

sum(PoolA$loco==0)
sum(PoolA$loco==1)
sum(PoolA$loco==2)
sum(PoolA$loco==3)

sum(PoolB$loco==0)
sum(PoolB$loco==1)
sum(PoolB$loco==2)
sum(PoolB$loco==3)

#plot activity locomotion  score



#issues with legend due to need to reshape data?
names(PoolA)
chartDF<-PoolA[,c(16,1,11)]
chartDF[chartDF$Lse==1,1]<-'1.a'
chartDF[chartDF$Lse==3,1]<-'2'
chartDF[chartDF$Lse==4,1]<-'3.a'
chartDF[chartDF$Lse==6,1]<-'4.a'


ggplot(chartDF,aes(x=factor(chartDF$loco), y=(chartDF$LAYINGCOUNTER)))+ 
  geom_boxplot()+
  geom_jitter(aes(color=factor(chartDF$Lse)),size=3,width = 0.2,height = 0.15)+
  labs(x = "Mobility Score",y='Laying Counter',color='Trial',title='Mobility and laying counter')+ theme(panel.background = element_blank())

#Parity

PoolA<-rbind.data.frame(SumDataList[[1]],SumDataList[[3]],
                        SumDataList[[4]],# 3.a
                        SumDataList[[6]]) 
                      
names(PoolA)
PoolA<-PoolA[,c(1:16,21)]
names(ParityData)[1]<-'Cow.ID'

PoolC<-left_join(PoolA,ParityData,'Cow.ID')
PoolC[(PoolC$Cow.ID==8308),15:19]
PoolD<-PoolC[-c(29,48),-c(17:20)]

PoolE<-left_join(PoolB,ParityData,'Cow.ID')
PoolE[(PoolE$Cow.ID==8308),15:19]
PoolE<-PoolE[-c(29,49)-c(17:20)]

names(PoolE)

summary(lm(PoolD$loco~PoolD$LactYr+PoolD$Lse))


CorMat <- rcorr(as.matrix(PoolD),type = 'spearman')

#r
DFCor<-rownames_to_column(as.data.frame(CorMat[1]))
CorList[[8]]<-as.data.frame(DFCor[,c(1:2,18)])

#n - values
DFCor<-rownames_to_column(as.data.frame(CorMat[2]))
numList[[8]]<-as.data.frame(DFCor[,c(1:2,18)])

#p-values
DFCor<-rownames_to_column(as.data.frame(CorMat[3]))
pList[[8]]<-as.data.frame(DFCor[,c(1:2,18)])



summary(PoolD$LactYr)

JE<-PoolD[PoolD$Lse==1,]
HF17<-PoolD[PoolD$Lse==3,]
HF18<-PoolD[PoolD$Lse==4,]
Comm<-PoolD[PoolD$Lse==6,]

JE<-PoolD[PoolE$Lse==2,]
HF17<-PoolD[PoolE$Lse==3,]
HF18<-PoolD[PoolE$Lse==5,]
Comm<-PoolD[PoolE$Lse==7,]

sum(na.omit(JE$LactYr==1))
sum(na.omit(JE$LactYr==2))
sum(na.omit(JE$LactYr==3))
sum(na.omit(JE$LactYr==4))
sum(na.omit(JE$LactYr==5))
sum(na.omit(JE$LactYr==6))
sum(na.omit(JE$LactYr==7))

sum(na.omit(HF17$LactYr==1))
sum(na.omit(HF17$LactYr==2))
sum(na.omit(HF17$LactYr==3))
sum(na.omit(HF17$LactYr==4))
sum(na.omit(HF17$LactYr==5))
sum(na.omit(HF17$LactYr==6))
sum(na.omit(HF17$LactYr==7))


sum(na.omit(HF18$LactYr==1))
sum(na.omit(HF18$LactYr==2))
sum(na.omit(HF18$LactYr==3))
sum(na.omit(HF18$LactYr==4))
sum(na.omit(HF18$LactYr==5))
sum(na.omit(HF18$LactYr==6))
sum(na.omit(HF18$LactYr==7))

sum(na.omit(Comm$LactYr==1))
sum(na.omit(Comm$LactYr==2))
sum(na.omit(Comm$LactYr==3))
sum(na.omit(Comm$LactYr==4))


summary(JE$LactYr)
summary(HF17$LactYr)
summary(HF18$LactYr)
summary(Comm$LactYr)