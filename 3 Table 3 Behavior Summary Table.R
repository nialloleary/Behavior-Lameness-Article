#Combine key columns from the lists created above into summary table
#Want the mean and var of 24 hour summary of each variable
mean.sd <- function(x) c(mean =mean(x,na.rm = T), sd = sd(x,na.rm = T))  
names(SumDataList[[1]][,c(4,8:21)])
names(SumDataList[[6]][,c(4:18)])
      
SummaryTable2<-as.data.frame(t(sapply(X = SumDataList[[1]][,c(4,8:21)],FUN = mean.sd)))
for (i in 2:5){
  SummaryTable2<-cbind.data.frame(SummaryTable2,
                      as.data.frame(t(sapply(X = SumDataList[[i]][,c(4,8:21)],FUN = mean.sd))))}
  for (i in 6:7){
    SummaryTable2<-cbind.data.frame(SummaryTable2,
                                    as.data.frame(t(sapply(X = SumDataList[[i]][,c(4:18)],FUN = mean.sd))))  
  
  }

SummaryTable2<-round(SummaryTable2,digits = 1)
SummaryTable2<-rownames_to_column(SummaryTable2)
colnames(SummaryTable2)<-c("Variable","1.a Jerseys Mean","1.a Jerseys Var", "1.b Jerseys Mean","1.b Jerseys Var","2 BW17 Mean","2 BW17Var", "3.a BW18a Mean","3.a BW18a Var","3.b BW18b Mean","3.b BW18b Var","4.a Farm Mean","4.a FarmaVar", "4.b Farm Mean", "4.b Farm Var")

SummaryTable2<-SummaryTable2[order(SummaryTable2$Variable,decreasing = F),]

#Write Behaviour correlation Table
setwd('../')
setwd('../')
# Outermost

write.csv(x = SummaryTable2,file = "Table2_24hr_Summary.csv")


