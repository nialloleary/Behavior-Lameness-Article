#Data available from https://1drv.ms/u/s!ArQRQHPfDmtggaRXMwHHQhvUbyI-Tw?e=wAyfXN

home<- "C:/Users/olearyn2/OneDrive/Behavior Paper data" # where data is locally

setwd(home)

{
   library(dplyr); library(data.table);library(tibble);library("Hmisc")
  
  #Meta Table ----
  #This table contains the meta data for locomotion scoring events required to run the script. This facilitate same script being applied to each data set. Only these variables should change between locmotion scoring events. The Selected row corresponds to the locomotion scoring event (lse). In the script, then the item from that row is called. 
  
  MVars<-c( #Meta variables 
    "Folder",  "WATCHSTART","Feature path","Loco_Index","ExclCow","Excl2","Excl3","ExclCosws4","ExclCows5","ExclCows6","ExclCows7",'JoinBy', "LSE")
  
  #Jersey trial - First scoring (a) (LSE1)
  ja<-c( './Jerseys', '02.06.2017 00:00',  "Loco0106",3,40,40,40,40,40,40,40, 'Ped')
  
  #LSE2
  jb<-c('./Jerseys','13.06.2017 00:00',   "Loco1506",4,40,40,40,40,40,40,40, 'Ped')
  
  #Dairygold 2017(Black & white herd)     LSE 3
   BW17<-c('./DGHF2017', '17.06.2017 00:00',  "LocoScore150617",4,
          40,40,40,40,40,40,40, 'UNITID') 
  
  #DairyGold 2018 (Black & white herd) LSE 4
  BW18a<-c( './DGHF2018', '10.08.2018 00:00', "Loco080818",2,
            'SN00017FFD', #  Accelerometer failed to record
          'SN00018E33', # data doesn't contain walking
           'SN00018D41',  # Faulty data doesn't contain walking 
            'SN00018E33', # Faulty doesn't contain walking
            'SN00018DD5',# Faulty doesn't contain walking
          40,40, 'Ped'  )
  #Lse 5
  BW18b<-c('./DGHF2018','13.08.2018 00:00', "Loco130818", 4,
           'SN00017FFD', #  Accelerometer failed to record
           'SN00018E33', # data doesn't contain walking
           'SN00018D41',  # Faulty data doesn't contain walking 
           'SN00018E33', # Faulty doesn't contain walking
           'SN00018DD5',# Faulty doesn't contain walking
           40,40, 'Ped'   )
  
  #Commerical farm 
  Farma <-c( './Commercial_Farm','17.08.2018 00:00', "Loco160818",2, 
             'SN00017FFD', #  Accelerometer failed to record
             'SN00018D41',#  Accelerometer failed to record
              'SN00018DD5', # Low walking recorded
             NA, # 
             25,25,
             25, 'Ped')# out by a day SN00018D79
  
  Farmb <-c('./Commercial_Farm', '20.08.2018 00:00',"Loco200818",4,  
            'SN00017FFD', #  Accelerometer failed to record
            'SN00018D41',#  Accelerometer failed to record
            'SN00018DD5', # Low walking recorded
            25,#
            25,25, 25, 'Ped')# out by a day SN00018D79
  
  #LSE selection (Locomotion scoring event)
  Meta<-rbind.data.frame(ja, jb,BW17, BW18a,BW18b,Farma, Farmb)
  Meta$LSE<-c("1.aJerseys", "1.bJerseys","2.BW17", "3.aBW18a","3.bBW18b","4.aFarma", "4.bFarmb")
  colnames(Meta)<-MVars
  
  #Initialise lists to store results from each cohort
  numList<-vector(mode ="list" ,(nrow(Meta)+3))
  varList<-vector(mode ="list" ,(nrow(Meta)+3))
  SumDataList<-vector(mode ="list" ,(nrow(Meta)+3))
  CorList<-vector(mode ="list" ,(nrow(Meta)+3))
  pList<-vector(mode ="list" ,(nrow(Meta)+3))
}

#Power analysis
library(pwr)
pwr.r.test(n =15 ,r = ,sig.level = 0.1,power =0.8,alternative = 'greater' )
pwr.r.test(n =16 ,r = ,sig.level = 0.1,power =0.8,alternative = 'greater' )
pwr.r.test(n = ,r =0.2 ,sig.level = 0.05,power =0.8,alternative = 'greater' )

#degrees of freedom in linear model = n-K
#k= parameters including intercept
#
pwr.f2.test(u = 4, # number of model coefficents 
            v = (63-4-1), #n-u-1
            f2 = ,sig.level = 0.1,power = 0.8  )


#https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html