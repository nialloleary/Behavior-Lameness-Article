
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

colnames(CorListC)<-c("Variable", "JerseysA","JerseysB", 
"BW17", "BW18A","BW18B","FarmA", "FarmB", "Jersey Change","BW18 Change", "Farm Change","Average Cor","Av Change Cor")

plstSig<-pListA
plstSig[pListA>0.2]<-NA
plstSig$rowname<- pListA$rowname

#Write Behaviour correlation Table

}# Outermost
setwd(home)
write.csv(x = CorListC,file = "Table4.csv")
write.csv(x = plstSig,file = "Table4Pvals.csv")
#P Values - Manually add in stars for the few that are significant.
