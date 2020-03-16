#Start----
{
library(readr)
setwd(home) 
ParityData <- read_csv("ParityData.csv")
names(ParityData)
ParityData<-ParityData[,c(1,2,5)]

#Loop start----
for (lse in 1:7) { # 7 scoring events
   print(c("Locomotion Scoring Event",lse))
  setwd(home) 
  setwd(as.character(Meta$Folder[[lse]]))
  setwd('./24Hourly')
  inde<-dir() # Index of file names 
  inde2<-substr(inde,start = 35,stop=44 ) # Pedometer Serial numbers
  Results3<-cbind.data.frame(inde,inde2)
  colnames(Results3)<-c("inde","UNITID")
  
  #Excluded Records----

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
  Results<-Results[,c(1,2,3,5,ncol(Results))] 
  
  #Relabel cow ID
  if   (names(Results)[4]=="Cow.ID"){
    names(Results)[4]<-'Cow.id'
  }
  if (names(Results)[3]=="FreezeBrand"){
    names(Results)[3]<-'Cow.id'
  }
  if (names(Results)[3]=="Jumbo"){
    names(Results)[3]<-'Cow.id'
  }
  if (names(Results)[3]=="Cow"){
    names(Results)[3]<-'Cow.id'
  }
  
  if(sum(lse==(1:5))>0){
  Results<-left_join(Results,ParityData,'Cow.id')
  }
  
  #duplicates from joins (1 cow in 2 trials) removed
  if(lse==3){
    Results<-Results[-c(14),]
  }
  if(lse==4){
    Results<-Results[-c(16),]
  }
  if(lse==5){
    Results<-Results[-c(16),]
  }
  
  
  {    ## 24 hour data-----
    setwd('./24Hourly')
    inde
    #load first & Initialise
    RWconvert <- fread(input = paste(Results[1,1]),sep2 = ";", header=T)
    #Append the rest
    for (i in 2:nrow(Results)) { # loads all the day records
      feat <- fread(input = paste(Results[i,1]),sep2 = ";",  header=T)
      RWconvert<-rbind.data.frame(RWconvert, feat)}
    
    RWconvert<-RWconvert %>% filter(WATCHSTART==paste(Meta$WATCHSTART[[lse]])) # relevant day
    RWconvert[RWconvert==0]<-NA
    
    #Variable Select----
    RWconvert<-RWconvert[,c(1,4:8,12:20)] 
    #assign to a list
    MODDF1<-left_join(Results,RWconvert,'UNITID')  
 # colnames(MODDF)[3] <- "CowID"

    

   # MODDF<-MODDF[,c(-1:-3,-5:-7,-9:-13)]# cow number
 MODDF<-MODDF1[,c('loco',"LAYTIME","STANDTIME","WALKTIME","STANDUP","LAYDOWN","ACTIVITY","LAYINGINDEX","STANDINGINDEX",   "WALKINGINDEX", "LAYINGCOUNTER", "STANDINGCOUNTER",  "WALKINGCOUNTER", "LIMBEVENTS",    "STRIDES")]

 SumDataList[[lse]]<-MODDF
 SumDataList[[lse]]$Lse<-lse
 SumDataList[[lse]]$c0<-sum(Results$loco==0)
 SumDataList[[lse]]$c1<-sum(Results$loco==1)
 SumDataList[[lse]]$c2<-sum(Results$loco==2)
 SumDataList[[lse]]$c3<-sum(Results$loco==3) 
 SumDataList[[lse]]$Cow.ID<-Results$Cow.id

 
    
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
}
}# start

#table 1 - numbers of each cow by mobility score
nrow(SumDataList[[1]])
SumDataList[[1]]$c0
SumDataList[[1]]$c1
SumDataList[[1]]$c2
SumDataList[[1]]$c3

nrow(SumDataList[[2]])
SumDataList[[2]]$c0
SumDataList[[2]]$c1
SumDataList[[2]]$c2
SumDataList[[2]]$c3

nrow(SumDataList[[3]])
SumDataList[[3]]$c0
SumDataList[[3]]$c1
SumDataList[[3]] $c2
SumDataList[[3]]$c3

nrow(SumDataList[[4]])
SumDataList[[4]]$c0
SumDataList[[4]]$c1
SumDataList[[4]]$c2
SumDataList[[4]]$c3

nrow(SumDataList[[5]])
SumDataList[[5]]$c0
SumDataList[[5]]$c1
SumDataList[[5]]$c2
SumDataList[[5]]$c3

nrow(SumDataList[[6]])
SumDataList[[6]]$c0
SumDataList[[6]]$c1
SumDataList[[6]]$c2
SumDataList[[6]]$c3

nrow(SumDataList[[7]])
SumDataList[[7]]$c0
SumDataList[[7]]$c1
SumDataList[[7]]$c2
SumDataList[[7]]$c3

