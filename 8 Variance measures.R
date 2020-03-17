CorList<-vector(mode ="list" ,(nrow(Meta)+3))
numList<-vector(mode ="list" ,(nrow(Meta)+3))
pList<-vector(mode ="list" ,(nrow(Meta)+3))
MRatioList<-vector(mode ="list" ,(nrow(Meta)+3))
VRatioList<-vector(mode ="list" ,(nrow(Meta)+3))
datelist<-c('02','13','17','10','13','17','20')
startlist<- c('35','35','20','20','20','20','20')
stoplist<-c('44','44','29','29','29','29','29')

#Loop start----

for (lse in 1:nrow(Meta)) { # Load selected locomotion scoring events
  print(c("Locomotion Scoring Event",lse))
  setwd(home) 
  setwd(as.character(Meta$Folder[[lse]]))
  setwd('./1Hourly')
  inde<-dir() # Index of file names 
  inde2<-substr(inde,start = startlist[[lse]],stop= stoplist[[lse]]) # Pedometer Serial numbers
  Results3<-cbind.data.frame(inde,inde2)
  colnames(Results3)<-c("inde","UNITID")
  #Excluded Records----
  # If you want to exclude 12 &13 you exclude 12 twice (13 goes to 12th position). if 12 and 14, 12 and 13 (14 goes to 13).
  #names(Meta)      
  for (m in 5:11){ 
    Results3<-Results3[paste0(Meta[lse,m])!=Results3$UNITID,]
  }
  
  #Locomotion Scores and Reference table----
  setwd("../")
  Ref1<-read.csv(file = 'PedRef.csv' ,sep = ",",header = T)
  Score<-read.csv(file =  'Score.csv'  ,sep = ",",header = T)# 
  NUM <-as.numeric(paste0(Meta$Loco_Index[[lse]])) # Column with relevant loco score for this 
  Score$loco2<-Score[,NUM] 
  Ref<- left_join(Score,Ref1,paste0(Meta$JoinBy[[lse]]))
  Results2<-left_join(Results3,Ref,"UNITID")#
  Results2$inc<-is.na(Results2$loco2) 
  Results<- Results2 %>% filter(inc== F) # don't load
  Results<-Results[,-(ncol(Results))] #remove inc
  Results$loco<-Results$loco2
  Results<-Results[,c(1,2,ncol(Results))] 
  
  ## 1 hour data-----
  setwd('./1Hourly')
  
  
  #load first & Initialise
  RWconvert <- fread(input = paste(Results[1,1]),sep2 = ";", header=T)
  #Append the rest
  for (i in 2:nrow(Results)) { # loads all the day records
    feat <- fread(input = paste(Results[i,1]),sep2 = ";",  header=T)
    RWconvert<-rbind.data.frame(RWconvert, feat,fill=TRUE)
  }
  
  # Pull out characters for date
  RWconvert$Date<-substr(x = RWconvert$WATCHSTART,start = 1,stop = 2)
  RWconvert <- RWconvert %>% filter(Date==paste(datelist[[lse]])) #Chosen day
  
  RWconvert$Hour<-as.numeric(substr(x = RWconvert$WATCHSTART,start = 12,stop = 13))
  MODDF<- RWconvert %>% group_by(UNITID)%>% summarise_all(var)
  
  MODDF<-left_join(Results,MODDF,'UNITID')
  names(MODDF)
  MODDF<-MODDF[,c(-1,-2,-4:-5,-11:-13,-26,-31,-32,-33)]
  SumDataList[[locomotion_scoring_event]]<-MODDF
  SumDataList[[lse]]<-MODDF

  
  #### Correlation within trial ----
  # rcorr creates a list of 3 with 1 - Correlation matrix r, 2 n and 3 p values
  
  CorMat <- rcorr(as.matrix(MODDF),type = 'spearman')
  
  #r
  DFCor<-rownames_to_column(as.data.frame(CorMat[1]))
  CorList[[lse]]<-as.data.frame(DFCor[,1:2])
  
  #n - values
  DFCor<-rownames_to_column(as.data.frame(CorMat[2]))
  numList[[lse]]<-as.data.frame(DFCor[,1:2])
  
  #p-values
  DFCor<-rownames_to_column(as.data.frame(CorMat[3]))
  pList[[lse]]<-as.data.frame(DFCor[,1:2])
  
  print("LSE")
  print(lse)
}


{
  CorListA<-as.data.frame(CorList[1])
  for (i in 2:7){
    CorListA<-left_join(CorListA,as.data.frame(CorList[[i]]),"rowname")
    colnames(CorListA)[ncol(CorListA)]<-ncol(CorListA)-1 }
  
  numListA<-as.data.frame(numList[1]) # N per sample
  for (i in 2:7){numListA<-left_join(numListA,as.data.frame(numList[[i]]),"rowname")
  
  colnames(numListA)[ncol(numListA)]<-ncol(numListA)-1 } #number the columns
  
  #P-value summary table ----
  pListA<-as.data.frame(pList[1])
  
  for (i in 2:7){pListA<-left_join(pListA,as.data.frame(pList[[i]]),"rowname")
  
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
  
  #colnames(CorListC)<-c("Variable", "JerseysA","JerseysB",                         "BW17", "BW18A","BW18B","FarmA", "FarmB", "Jersey Change","BW18 Change", "Farm Change","Average Cor","Av Change Cor")
  
  plstSig<-pListA
  plstSig[pListA>0.3]<-NA
  plstSig$rowname<- pListA$rowname
  
  #Write Behaviour correlation Table
  
}# Outermost
setwd(home)
write.csv(x = CorListC,file = "Table4.csv")
write.csv(x = plstSig,file = "Table4Pvals.csv")
#P Values - Manually add in stars for the few that are significant.

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
