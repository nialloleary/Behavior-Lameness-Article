
#Run Scripts 1 & 2 first before this one
JerseyB <- as.data.frame(SumDataList[[2]]) 
JerseyB <- JerseyB[-4,] # Cow wasn't scored first time
SumDataList[[8]] <- SumDataList[[1]]-JerseyB  #Jersey change
SumDataList[[8]]$Lse<-8
SumDataList[[9]] <- (
  as.data.frame(SumDataList[4])-as.data.frame(SumDataList[5])
)  #BW2018 Change
SumDataList[[9]]$Lse<-9
SumDataList[[10]] <- as.data.frame(SumDataList[6])-as.data.frame(SumDataList[7]) #Commercial farm change
SumDataList[[10]]$Lse<-10
#Change correlations ----

for (locomotion_scoring_event in 8:10) {
  #Duplication of code
  CorMat <- rcorr(x = as.matrix(SumDataList[[locomotion_scoring_event]]),type = 'spearman')
  
  DFCor <- rownames_to_column(as.data.frame(CorMat[1])) #r
  CorList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
  
  DFCor <- rownames_to_column(as.data.frame(CorMat[2])) #n - values
  numList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
  
  DFCor <- rownames_to_column(as.data.frame(CorMat[3])) #p-values
  pList[[locomotion_scoring_event]] <- as.data.frame(DFCor[,1:2])
}




{
  CorListA<-as.data.frame(CorList[1])
  for (i in 2:10){
    CorListA<-left_join(CorListA,as.data.frame(CorList[[i]]),"rowname")
    colnames(CorListA)[ncol(CorListA)]<-ncol(CorListA)-1
    }
  
  numListA<-as.data.frame(numList[1]) # N per sample
  for (i in 2:10){numListA<-left_join(numListA,as.data.frame(numList[[i]]),"rowname")
  
  colnames(numListA)[ncol(numListA)]<-ncol(numListA)-1 } #number the columns
  
  #P-value summary table ----
  pListA<-as.data.frame(pList[1])
  
  for (i in 2:10){pListA<-left_join(pListA,as.data.frame(pList[[i]]),"rowname")
  
  colnames(pListA)[ncol(pListA)]<-ncol(pListA)-1 }
  
  CorListA$AverageAbsoluteCorrelation <- rowMeans(CorListA[,2:8])
  
  CorListA[nrow(CorListA)+1,1]<-'n'
  CorListA[nrow(CorListA),2:8]<- sapply(X = numListA[2:8],FUN = max)
  #Round & Name  
  
  is.num <- sapply(CorListA, is.numeric)
  
  CorListB<-cbind(CorListA[,1],as.data.frame(lapply(CorListA[is.num],round,2)))
  
  #Blank values of negligible size
  #CorListC<-replace(CorListB,CorListB<0.2 & CorListB > -0.2,NA)
  CorListC<-CorListB
  
  colnames(CorListC)<-c("Variable", "JerseysA","JerseysB", 
                        "BW17", "BW18A","BW18B","FarmA", "FarmB", "Jersey Change","BW18 Change", "Farm Change","Average Cor","Av Change Cor")
  
  plstSig<-pListA
  plstSig[pListA>0.2]<-NA
  plstSig$rowname<- pListA$rowname
  
  #Write Behaviour correlation Table
  
}# Outermost
setwd(home)
write.csv(x = CorListC,file = "ChangeCor.csv")
write.csv(x = plstSig,file = "ChangePvals.csv")
#P Values - Manually add in stars for the few that are significant.

PoolCh<-rbind.data.frame(SumDataList[[8]],SumDataList[[9]],
                 SumDataList[[10]])

PTCh<- data.frame(matrix(ncol = 3, nrow = 14))
colnames(PTCh)<-c('var','Estimate','pval')
names(PoolCh)
library(broom);
for (i in 2:15){
  PTCh[i-1,1]<-(names(PoolCh[i]))
  mod<-tidy((glm(scale(PoolCh$loco)~scale(PoolCh[,i])+as.factor(PoolCh$Lse))))# standardised coefficients
  
  PTCh[i-1,2]<-(mod$estimate[2])
  
  PTCh[i-1,3]<-(mod$p.value[2])
}

