## Task 10*: Multiply annual flow volume results (Task 7) with connectivity adjustment (Task 8)
#join datasets

source('DryWeatherLoadingCalcs/1projectsetup.R')

inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')

values <- list()  
values[['DailyCFandPond_trib2']] <- paste0(outPath, 'DailyCFandPond_trib2.rds')
DailyCFandPond_trib2 <-  readRDS(values[["DailyCFandPond_trib2"]])  
 
#includes all outfalls, including those without delineated tributaries

values <- list()  
values[['DryDaysYear']] <- paste0(outPath, 'DryDaysYear.rds')
DryDaysYear <-  readRDS(values[["DryDaysYear"]]) %>%
  filter(!is.na('Facility Identifier')) %>%
  filter(MonitoringYear=='MY2023-24')  

DryDaysYear[DryDaysYear == ""] <- NA

DryDaysYear <- DryDaysYear %>%
  filter(!is.na('Facility Identifier'))

values <- list()  
values[['Connectivity']] <- paste0(outPath, 'Connectivity.rds')
Connectivity <-  readRDS(values[["Connectivity"]])   #includes all outfalls, including those without delineated tributaries
  

file_url<- paste0(inPath, "MajorOutfalls_2024.csv")
MO<-read.csv(file_url) %>%
  select('Facility.Identifier')


#Calculate total annual flow by jurisdiction and monitoring year - should only be for those with a result in DischargePCF3
AnnualFlow <- full_join(DailyCFandPond_trib2, Connectivity, by=c('Facility Identifier'='Facility.Identifier')) %>%
  unique() %>%
  left_join(., DryDaysYear, by=c('Facility Identifier'= 'FACILITYID')) %>%
  group_by(`Facility Identifier`, JURISDICTI3) %>%
  mutate(Qadj_Qall=avgCnx*dry_days*DischargePCF3) 

saveRDS(AnnualFlow, file = paste0(outPath, 'AnnualFlow.rds'))
write_csv(AnnualFlow, path = paste0(outPath, 'AnnualFlow.csv'))


#Find annual flow volume by jurisdiction
#Remove outfalls after volumes determined

file_url<- paste0(wd, "/juris_area_summary.csv")
SOCWMA_Cities<-read.csv(file_url) %>%
  select('jurisdicti', 'acres_juris_soc') %>%
  filter(!is.na(acres_juris_soc)) %>%
  filter(!is.na(jurisdicti))

AnnualFlow_Juriscon<-AnnualFlow %>% #remove outfalls upstream of most downstream outfall closest to receiving water
  as_tibble() %>%
  ungroup() %>%
  filter('Facility Identifier' != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter('Facility Identifier' != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter('Facility Identifier' != "L05-049-2" & 'Facility Identifier' != "L05-049-1" & 'Facility Identifier' != "L05-489-7" & 'Facility Identifier' !="L05-489-3" & 'Facility Identifier' !="L05-489-4") %>%  #Horno Basin
  filter('Facility Identifier' != "J03-9234-8" & 'Facility Identifier' != "J03-9234-6" & 'Facility Identifier' !="J03-9234-5" & 'Facility Identifier' !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter('Facility Identifier' != "L03-141-1" & 'Facility Identifier' != "L03-141-3" & 'Facility Identifier' != "L03-141-2" & 'Facility Identifier' != "L03-172-2" & 'Facility Identifier' != "L03-172-3" & 'Facility Identifier' != "L03-073-3" & 'Facility Identifier' != "L03-073-4" & 'Facility Identifier' != "L03-073-5" & 'Facility Identifier' != "L03-074-2" & 'Facility Identifier' != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter('Facility Identifier' != "L04-136-1u (L04P07)" | 'Facility Identifier' != "L04-136-1(L04P07)") %>%
  filter('Facility Identifier' != "J03-9199-2" & 'Facility Identifier' != "J03-9190-1" & 'Facility Identifier' != "J03-9199-1") %>%
  filter('Facility Identifier' != "K01-12156-6"   & 'Facility Identifier' != "K01-12156-4") %>% #Salt Creek
  filter('Facility Identifier' != "M02-052-3" & 'Facility Identifier' != "M02-052-4" & 'Facility Identifier' != "M02-032-1" & 'Facility Identifier' != "M02-085-1 (M02P06)" & 'Facility Identifier' != "M02-085-2" & 'Facility Identifier' != "M02-013-1" & 'Facility Identifier' != "M02-086-1" & 'Facility Identifier' != "M02-015-1" & 'Facility Identifier' != "M02-028-2 (M02P08)" & 'Facility Identifier' != 'M02-061-7' & 'Facility Identifier' != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter('Facility Identifier' != "M01-008-1" & 'Facility Identifier' != "M01-060-3" & 'Facility Identifier' != "M01-124-4") %>%
  filter('Facility Identifier' != "M00.1-070-6" & 'Facility Identifier' != "M00.1-070-4" & 'Facility Identifier' != "M00.1-070-3" & 'Facility Identifier' != "M00.1-070-2" & 'Facility Identifier' != 'M00.1-070-1' & 'Facility Identifier' !=  "M00.1-071-1 (M00S04)" & 'Facility Identifier' !=  "M00.1-071-4 (M00S04)" & 'Facility Identifier' !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter('Facility Identifier' != "I01-11503-3"  & 'Facility Identifier' != "I01-11503-4" & 'Facility Identifier' != "I01-11502-1" & 'Facility Identifier' != "I01-11216-3" & 'Facility Identifier' != "I01-11216-2 (I02P12)" & 'Facility Identifier' != "I01-11216-1 (I02P13)" & 'Facility Identifier' != "I01-11216-4 (I02P14)" & 'Facility Identifier' != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter('Facility Identifier' != "L01-613-1" & 'Facility Identifier' != "L01-728-7 (L01S03)")  %>%
  filter('Facility Identifier' != "L01-613-1" & 'Facility Identifier' != "L03-240-1 (L03P14)")  %>%
  select(JURISDICTI3, Qadj_Qall) %>%
  full_join(., SOCWMA_Cities, by=c('JURISDICTI3'='jurisdicti')) %>% 
  unique() %>%
  filter(!is.na(Qadj_Qall)) %>%
  group_by(JURISDICTI3) %>%
  mutate(QTot=sum(Qadj_Qall)) %>% 
  mutate(DischargeJ_af = QTot/43560) %>%
  mutate(QperArea_cfperacre=QTot/acres_juris_soc) %>%
  select(JURISDICTI3, DischargeJ_af, QperArea_cfperacre) %>%
  unique() %>%
  ungroup() %>%
  filter(!is.na(QperArea_cfperacre)) 

saveRDS(AnnualFlow_Juriscon, file = paste0(outPath, 'AnnualFlow_Juriscon.rds'))  
write_csv(AnnualFlow_Juriscon, path = paste0(outPath, 'AnnualFlow_Juriscon.csv'))


#Determine Annual flow by jurisdiction for unsampled outfalls (not in Appendix M, and also ponded outfalls in Appendix M)

#find outfalls with flow measurements but not sampled

file_url<- paste0(inPath, "MajorOutfalls_2024.csv")
MO<-read.csv(file_url) %>%
  select('Facility.Identifier') 

MO$Facility.Identifier[MO$Facility.Identifier == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 


values <- list()  
values[['AnnualFlow']] <- paste0(outPath, 'AnnualFlow.rds')
AnnualFlow <-  readRDS(values[["AnnualFlow"]]) 

Trib <- arc.open('SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint_Tributary') %>% 
  arc.select() %>% 
  as_tibble()

MOp<- arc.open('SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint') %>%
  arc.select() %>%
  tibble::as_tibble() %>%
  select('FACILITYID','SAMPLEDRY')

MOp$FACILITYID[MOp$FACILITYID == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 

MO <- left_join(MO, MOp, by=c('Facility.Identifier'='FACILITYID'))

AnnualFlow_USa <- AnnualFlow %>%
  right_join(., MO, by=c('Facility Identifier' = 'Facility.Identifier')) %>%
  filter(is.na(SAMPLEDRY)|SAMPLEDRY==2) %>%
  #filter(Q_DailyavgCF >'-0.99') %>%
  select('Facility Identifier', JURISDICTI3, Qadj_Qall) %>%
  filter(!is.na(JURISDICTI3))
   
saveRDS(AnnualFlow_USa, file = paste0(outPath, 'AnnualFlow_USa.rds'))
write_csv(AnnualFlow_USa, path = paste0(outPath, 'AnnualFlow_USa.csv'))  #put in connectivity by hand in csv#save csv as AnnualFlow_USa; use same connectivity as previous years if no data; if no observation, use 0.77


#outfalls sampled but without any flow measurements         
AnnualFlow_USb <- AnnualFlow %>%
  right_join(., MO, by=c('Facility Identifier' = 'Facility.Identifier')) %>%
  filter(SAMPLEDRY =='1' & (Q4calcsall=='-0.99')) %>%
  select('Facility Identifier', JURISDICTI3, Qadj_Qall) %>%   
  unique() 


#AnnualFlow_US <- bind_rows(AnnualFlow_USa, AnnualFlow_USb)   #flow from unsampled outfalls, both measured flow and estimates at ponded outfalls
AnnualFlow_US <- AnnualFlow_USa

#AnnualFlow_USj <- AnnualFlow_US %>%
  #select(JURISDICTI3, QJur_US) %>%
  #unique() %>%
  #group_by(JURISDICTI3) %>%
  #mutate(QJur_US=sum(QJur_US)) %>%
  #unique() 

#AnnualFlow_US <-AnnualFlow_US %>%
  #select(-QJur_US) %>%
  #left_join(.,AnnualFlow_USj)

saveRDS(AnnualFlow_US,  paste0(outPath,'AnnualFlow_US.rds'))
write_csv(AnnualFlow_US, path = paste0(outPath, 'AnnualFlow_US.csv'))


#by jurisdiction
values <- list()  
values[['AnnualFlow_US']] <- paste0(outPath, 'AnnualFlow_US.rds')

AnnualFlow_US_j <- AnnualFlow_US %>%
  filter('Facility Identifier' != "M01-008-1" & 'Facility Identifier' != "M01-060-3" & 'Facility Identifier' != "M01-124-4") %>%
  filter('Facility Identifier' != "M00.1-070-6" & 'Facility Identifier' != "M00.1-070-4" & 'Facility Identifier' != "M00.1-070-3" & 'Facility Identifier' != "M00.1-070-2" & 'Facility Identifier' != 'M00.1-070-1' & 'Facility Identifier' !=  "M00.1-071-1 (M00S04)" & 'Facility Identifier' !=  "M00.1-071-4 (M00S04)" & 'Facility Identifier' !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter('Facility Identifier' != "I01-11503-3"  & 'Facility Identifier' != "I01-11503-4" & 'Facility Identifier' != "I01-11502-1" & 'Facility Identifier' != "I01-11216-3" & 'Facility Identifier' != "I01-11216-2 (I02P12)" & 'Facility Identifier' != "I01-11216-1 (I02P13)" & 'Facility Identifier' != "I01-11216-4 (I02P14)" & 'Facility Identifier' != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter('Facility Identifier' != "L01-613-1" & 'Facility Identifier' != "L01-728-7 (L01S03)")  %>%
  filter('Facility Identifier' != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter('Facility Identifier' != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter('Facility Identifier' != "L05-049-2" & 'Facility Identifier' != "L05-489-7" & 'Facility Identifier' !="L05-489-4" & 'Facility Identifier' !="L05-489-3") %>%  #Horno Basin
  filter('Facility Identifier' != "J03-9234-8" & 'Facility Identifier' != "J03-9234-6" & 'Facility Identifier' !="J03-9234-5" & 'Facility Identifier' !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter('Facility Identifier' != "L03-141-1" & 'Facility Identifier' != "L03-141-3" & 'Facility Identifier' != "L03-141-2" & 'Facility Identifier' != "L03-172-2" & 'Facility Identifier' != "L03-172-3" & 'Facility Identifier' != "L03-073-3" & 'Facility Identifier' != "L03-073-4" & 'Facility Identifier' != "L03-073-5" & 'Facility Identifier' != "L03-074-2" & 'Facility Identifier' != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter('Facility Identifier' != "L04-136-1u (L04P07)") %>%
  filter('Facility Identifier' != "J03-9199-2" & 'Facility Identifier' != "J03-9190-1" & 'Facility Identifier' != "J03-9199-1") %>%
  filter('Facility Identifier' != "K01-12156-6"   & 'Facility Identifier' != "K01-12156-4") %>% #Salt Creek
  filter('Facility Identifier' != "M02-052-3" & 'Facility Identifier' != "M02-052-4" & 'Facility Identifier' != "M02-013-1" & 'Facility Identifier' != "M02-086-1" & 'Facility Identifier' != "M02-015-1" & 'Facility Identifier' != "M02-028-2 (M02P08)" & 'Facility Identifier' != 'M02-061-7' & 'Facility Identifier' != 'M02-102-1') %>% #egunda Deshecha Channel

unique() %>%
  group_by(JURISDICTI3) %>%
  mutate(QJur_US=sum(Qadj_Qall)) %>%

unique() 

  saveRDS(AnnualFlow_US_j, paste0(outPath, 'AnnualFlow_US_j.rds'))
write_csv(AnnualFlow_US_j, path = paste0(outPath, 'AnnualFlow_US_j.csv'))

#Dataset with flow from sampled outfalls with flow
AnnualFlow_s <- AnnualFlow %>%
  right_join(., MO, by=c('Facility Identifier' = 'Facility.Identifier')) %>%
  filter(SAMPLEDRY =='1'& Q4calcsall != '-0.99') %>%
  select('Facility Identifier', JURISDICTI3, Qadj_Qall) %>%
  unique() 
  

saveRDS(AnnualFlow_s, paste0(outPath, 'AnnualFlow_s.rds'))
write_csv(AnnualFlow_s, paste0(outPath, 'AnnualFlow_s.csv'))

#combine sampled and unsampled for all outfalls and total jurisdictional flow volume

AnnualFlow_all <- bind_rows(AnnualFlow_s, AnnualFlow_US)
saveRDS(AnnualFlow_all, paste0(outPath, 'AnnualFlow_all.rds'))
write_csv(AnnualFlow_all, paste0(outPath, 'AnnualFlow_all.csv'))


#by jurisdiction

values <- list()  
values[['AnnualFlow_s']] <- paste0(outPath, 'AnnualFlow_s.rds')
AnnualFlow_S_j <-  readRDS(values[["AnnualFlow_s"]]) %>%
  as_tibble() %>%
  filter('Facility Identifier' != "L05-049-1" & 'Facility Identifier' != "M02-032-1" & 'Facility Identifier' != "M02-085-1 (M02P06)" & 'Facility Identifier' != "M02-085-2"
         & 'Facility Identifier' !="L05-489-3") %>% 
  select(c('Facility Identifier', JURISDICTI3, Qadj_Qall)) %>%
  group_by(JURISDICTI3) %>%
  mutate(QJur_S=sum(Qadj_Qall)) %>%
  ungroup() %>%
  unique() 
  
values <- list() 
values[['AnnualFlow_US_j']] <- paste0(outPath, 'AnnualFlow_US_j.rds')
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]]) %>%
  select(c('Facility Identifier', JURISDICTI3, Qadj_Qall, QJur_US))

AnnualFlow_J2<-full_join(AnnualFlow_S_j, AnnualFlow_US_j, by=c('JURISDICTI3', 'Facility Identifier', 'Qadj_Qall')) %>%
  select('Facility Identifier', JURISDICTI3, Qadj_Qall, QJur_US,QJur_S) %>%
  unique()

saveRDS(AnnualFlow_J2, paste0(outPath, 'AnnualFlow_J2.rds'))
write_csv(AnnualFlow_J2, paste0(outPath, 'AnnualFlow_J2.csv'))

AnnualFlow_J2short<-full_join(AnnualFlow_S_j, AnnualFlow_US_j, by=c('JURISDICTI3', 'Facility Identifier', 'Qadj_Qall')) %>%
  rowwise() %>%
  mutate(DischargeJ_cf = sum(QJur_S, QJur_US, na.rm=TRUE)) %>%
  mutate(DischargeJ_af = DischargeJ_cf/43560) %>%   #convert to acre-ft; 1 acre-feet = 43560 cf
  unique() %>%
  select(JURISDICTI3, DischargeJ_cf, DischargeJ_af) %>%
unique() %>%
  group_by(JURISDICTI3) %>%
  mutate(DischargeJ_cf = sum(DischargeJ_cf)) %>%
  mutate(DischargeJ_af = sum(DischargeJ_af)) %>%
  unique() %>%
  filter(!is.na(JURISDICTI3))

saveRDS(AnnualFlow_J2short, paste0(outPath, 'AnnualFlow_J2short.rds'))
write_csv(AnnualFlow_J2short, paste0(outPath, 'AnnualFlow_J2short.csv'))

#for print output (Jurisdictions, cnx)

values <- list()  
values[['Connectivity']] <- paste0(outPath,'Connectivity.rds')
Connectivity <-  readRDS(values[["Connectivity"]])


values[['AnnualFlow_all']] <- paste0(outPath, 'AnnualFlow_all.rds')
AnnualFlow_all <-  readRDS(values[["AnnualFlow_all"]]) 

values[['AnnualCF2b']] <- paste0(outPath, 'AnnualCF2b.rds')
AnnualCF2b <-  readRDS(values[["AnnualCF2b"]]) %>%
  select('Facility Identifier', Jurisdiction, JURISDICTI3, AnnualCF) %>%
  unique()

file_url<- paste0(inPath, "MajorOutfalls_2024.csv")
MO<-read.csv(file_url) %>%
  select('Facility.Identifier', 'Jurisdiction') 


MO[,2] = toupper(MO[,2])

MO$Facility.Identifier[MO$Facility.Identifier == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 

values <- list() 
values[['AnnualFlow_J2']] <- paste0(outPath, 'AnnualFlow_J2.rds')
AnnualFlow_J2 <-  readRDS(values[["AnnualFlow_J2"]]) %>%
  select('Facility Identifier', JURISDICTI3, QJur_US, QJur_S)


AnnualFlow_all_print <- AnnualFlow_all %>%
    full_join(., Connectivity, by=c('Facility Identifier' = 'Facility.Identifier')) %>%
  full_join(., AnnualFlow_J2short) %>%
  full_join(., MO, by=c('Facility Identifier' = 'Facility.Identifier')) %>%
  full_join(., AnnualCF2b, by=c('Facility Identifier', 'Jurisdiction' = 'Jurisdiction', 'JURISDICTI3')) %>%
  full_join(., AnnualFlow_J2, by=c('Facility Identifier', 'JURISDICTI3')) %>%
  select(Jurisdiction, 'Facility Identifier', JURISDICTI3, AnnualCF, avgCnx, Qadj_Qall, QJur_S, QJur_US) %>%
  filter(!is.na(JURISDICTI3))

write_csv(AnnualFlow_all_print, paste0(outPath, '/AnnualFlow_all_print.csv'))
