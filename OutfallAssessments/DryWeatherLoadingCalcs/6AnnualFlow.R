
source('DryWeatherLoadingCalcs/1projectsetup.R')
inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/ProvisionalFlow/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')

#convert to total daily discharge


values <- list()  
values[['MedianQ_contCalcs']] <- paste0(outPath, 'MedianQ_contCalcs.rds')
MedianQ_contCalcs <-  readRDS(values[["MedianQ_contCalcs"]]) %>%
  mutate(Qcf2024 = Qcont4calc2024*60*60*24) %>%
  mutate(Qcfp = Qcont4calcPrior*60*60*24)


values <- list() 
values[['allInst']] <- paste0(outPath, 'allInst.rds')
allInst <-  readRDS(values[["allInst"]]) %>%
  mutate(QInscf = Q4calcs*60*60*24)

  colnames(allInst)[colnames(allInst) == "StationCode"] <- "FACILITYID"


#combine file with instant flow and continuous flow measurements and create a column with flow measurement for calcs; outfalls with no flow measurements ever (ponded or not visited) are NA

DailyAvgCF <- full_join(allInst, MedianQ_contCalcs, by=c("Facility Identifier" ="FACILITYID")) %>%  #create a column for all continuous flow data for 2023, use instead of instantaneous for calcs
  mutate(Qboth = mean(c(Qcfp, QInscf))) %>%
  mutate(Q4calcsall=coalesce(Qcf2024, Qboth)) %>%
  mutate(Q4calcsall=coalesce(Q4calcsall, Qcfp)) %>%
  mutate(Q4calcsall=coalesce(Q4calcsall, QInscf))
str(DailyAvgCF)


saveRDS(DailyAvgCF, file = paste0(outPath, 'DailyAvgCF.rds'))
write_csv(DailyAvgCF, path = paste0(outPath, 'DailyAvgCF.csv')) 


## Combine Daily flow file and no flow files to estimate flow at outfalls without flow measurements


values <- list()  
values[['JURISinTRIBS2']] <- paste0(outPath, 'JURISinTRIBS2.rds')
JURISinTRIBS2 <-  readRDS(values[["JURISinTRIBS2"]]) %>%
  select(FACILITYID, JURISDICTI3, Jurisdiction, PERCENTAGE, area_acres, Acres) %>%
  unique()

JURISinTRIBS2$FACILITYID[JURISinTRIBS2$FACILITYID == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)"

DailyCFandPond_trib <- full_join(DailyAvgCF, JURISinTRIBS2,  by=c('Facility Identifier' = 'FACILITYID', 'Jurisdiction' = 'Jurisdiction')) %>%
  filter(!is.na(JURISDICTI3))  #filters out non major outfalls

saveRDS(DailyCFandPond_trib, file = paste0(outPath, 'DailyCFandPond_trib.rds'))
        
write_csv(DailyCFandPond_trib, path = paste0(outPath, 'DailyCFandPond_trib.csv'))


values <- list()  
values[['DailyCFandPond_trib']] <- paste0(outPath, '/DailyCFandPond_trib.rds')
DailyCFandPond_trib <-  readRDS(values[["DailyCFandPond_trib"]]) 
#%>%
  #filter(JURISDICTI3 != c(IRVINE, 'NEWPORT BEACH'))

library(DescTools)



DailyCFandPond_trib2a <-  DailyCFandPond_trib %>%    #estimate flow at ponded outfalls and outfalls not visited from average daily (instead of annual)
 # filter(JURISDICTI3 != "IRVINE") %>%
 # filter(JURISDICTI3 != 'NEWPORT BEACH') %>%
  #mutate(DischargePCF2= PERCENTAGE*Q4calcsall/100)
  mutate(DischargePCF2= PERCENTAGE*Q4calcsall/100, Q4calcsall) %>%
  filter(DischargePCF2 > '0'| is.na(Q4calcsall)) %>%  #need to filter away the zeros to run the geomean
  replace_na(list(Q4calcsall=-0.99)) %>% 
  group_by(JURISDICTI3) %>%  #use geomean instead of log mean (July 27, 2021; 2019-20 Annual Report used arithmetic average)
  mutate(DischargePCF3 = ifelse(Q4calcsall < 0 & is.na(DischargePCF2) , geoMean(DischargePCF2, na.rm=TRUE), DischargePCF2)) 

#add back in the zeros
Zeros <-  DailyCFandPond_trib %>%    #estimate flow at ponded outfalls and outfalls not visited from average daily (instead of annual)
  #filter(JURISDICTI3 != "IRVINE") %>%
  #filter(JURISDICTI3 != 'NEWPORT BEACH') %>%
  #mutate(DischargePCF2= PERCENTAGE*Q4calcsall/100)
  mutate(DischargePCF2= PERCENTAGE*Q4calcsall/100, Q4calcsall) %>%
  filter(DischargePCF2 == '0')

DailyCFandPond_trib2all <- bind_rows(DailyCFandPond_trib2a, Zeros) %>%
  mutate(DischargePCF3 = coalesce(DischargePCF3, DischargePCF2)) %>%
  filter(JURISDICTI3 != "IRVINE")  %>%
  filter(JURISDICTI3 != "NEWPORT BEACH")

saveRDS(DailyCFandPond_trib2all, file = paste0(outPath,'DailyCFandPond_trib2.rds'))

write_csv(DailyCFandPond_trib2all, path = paste0(outPath,'DailyCFandPond_trib2.csv'))


#Annual Flow volume - join in dry_days_file
values <- list()  
values[['DryDaysYear']] <- paste0(outPath, 'DryDaysYear.rds')
DryDaysYearGroup <-  readRDS(values[["DryDaysYear"]]) %>%
  filter(!is.na(FACILITYID)) %>%
  filter(MonitoringYear=='MY2023-24')

values <- list()  
values[['DailyCFandPond_trib2']] <- paste0(outPath, 'DailyCFandPond_trib2.rds')
DailyCFandPond_trib2 <-  readRDS(values[["DailyCFandPond_trib2"]])


AnnualCF2b <- DailyCFandPond_trib2 %>%
  left_join(.,DryDaysYearGroup, by=c('Facility Identifier'="FACILITYID")) %>%  
  filter(MonitoringYear=='MY2023-24') %>%
  group_by(`Facility Identifier`, JURISDICTI3) %>%
  #mutate(AnnualCFb=mean(DischargePCF3)) %>%
  #mutate(AnnualCF = dry_days*AnnualCFb)
  mutate(AnnualCF = dry_days*DischargePCF3)

saveRDS(AnnualCF2b, file = paste0(outPath, 'AnnualCF2b.rds'))
write_csv(AnnualCF2b, path = paste0(outPath,'AnnualCF2b.csv'))


#Find annual flow volume by jurisdiction
#Remove outfalls after volumes determined
AnnualCF2 <- AnnualCF2b %>%  #remove outfalls upstream of most downstream outfall closest to receiving water
  filter('Facility Identifier' != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter('Facility Identifier' != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter('Facility Identifier' != "L05-049-2" & 'Facility Identifier' != "L05-049-1" & 'Facility Identifier' != "L05-489-7" & 'Facility Identifier' !="L05-489-3" & 'Facility Identifier' !="L05-489-4") %>%  #Horno Basin
  filter('Facility Identifier' != "J03-9234-8" & 'Facility Identifier' != "J03-9234-6" & 'Facility Identifier' !="J03-9234-5" & 'Facility Identifier' !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter('Facility Identifier' != "L03-141-1" & 'Facility Identifier' != "L03-141-3" & 'Facility Identifier' != "L03-141-2" & 'Facility Identifier' != "L03-172-2" & 'Facility Identifier' != "L03-172-3" & 'Facility Identifier' != "L03-073-3" & 'Facility Identifier' != "L03-073-4" & 'Facility Identifier' != "L03-073-5" & 'Facility Identifier' != "L03-074-2" & 'Facility Identifier' != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter('Facility Identifier' != "L04-136-1 (L04P07)") %>%
  filter('Facility Identifier' != "J03-9199-2" & 'Facility Identifier' != "J03-9190-1" & 'Facility Identifier' != "J03-9199-1") %>%
  filter('Facility Identifier' != "K01-12156-6"   & 'Facility Identifier' != "K01-12156-4") %>% #Salt Creek
  filter('Facility Identifier' != "M02-052-3" & 'Facility Identifier' != "M02-052-4" & 'Facility Identifier' != "M02-032-1" & 'Facility Identifier' != "M02-085-1 (M02P06)" & 'Facility Identifier' != "M02-085-2" & 'Facility Identifier' != "M02-013-1" & 'Facility Identifier' != "M02-086-1" & 'Facility Identifier' != "M02-015-1" & 'Facility Identifier' != "M02-028-2 (M02P08)" & 'Facility Identifier' != 'M02-061-7' & 'Facility Identifier' != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter('Facility Identifier' != "M01-008-1" & 'Facility Identifier' != "M01-060-3" & 'Facility Identifier' != "M01-124-4") %>%
  filter('Facility Identifier' != "M00.1-070-6" & 'Facility Identifier' != "M00.1-070-4" & 'Facility Identifier' != "M00.1-070-3" & 'Facility Identifier' != "M00.1-070-2" & 'Facility Identifier' != 'M00.1-070-1' & 'Facility Identifier' !=  "M00.1-071-1 (M00S04)" & 'Facility Identifier' !=  "M00.1-071-4 (M00S04)" & 'Facility Identifier' !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter('Facility Identifier' != "I01-11503-3"  & 'Facility Identifier' != "I01-11503-4" & 'Facility Identifier' != "I01-11502-1" & 'Facility Identifier' != "I01-11216-3" & 'Facility Identifier' != "I01-11216-2 (I02P12)" & 'Facility Identifier' != "I01-11216-1 (I02P13)" & 'Facility Identifier' != "I01-11216-4 (I02P14)" & 'Facility Identifier' != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter('Facility Identifier' != "L01-613-1" & 'Facility Identifier' != "L01-728-7 (L01S03)")

saveRDS(AnnualCF2, file = paste0(outPath, 'AnnualCF2.rds'))
write_csv(AnnualCF2, path = paste0(outPath, 'AnnualCF2.csv'))

#make unique values for JURISDICTI3 and ANNUAL CF. 

values <- list()  
values[['AnnualCF2']] <- paste0(outPath, 'AnnualCF2.rds')
AnnualCF2 <-  readRDS(values[["AnnualCF2"]])

file_url<- paste0(wd, "/juris_area_summary.csv")
SOCWMA_Cities<-read.csv(file_url) %>%
  select('jurisdicti', 'acres_juris_soc') %>%
  filter(!is.na(acres_juris_soc)) %>%
  filter(!is.na(jurisdicti))



AnnualCF_J  <- AnnualCF2 %>%
  as_tibble() %>%
  ungroup() %>%
  select('JURISDICTI3', 'AnnualCF') %>%
  full_join(., SOCWMA_Cities, by=c('JURISDICTI3'='jurisdicti')) %>%
  unique() %>%
  group_by(JURISDICTI3) %>%
  mutate(DischargeJ_cf=sum(AnnualCF)) %>%
  mutate(DischargeJ_af=DischargeJ_cf/43560) %>%  #convert to acre-ft; 1 acre-feet = 43560 cf
  select('JURISDICTI3','DischargeJ_cf', 'DischargeJ_af',  'acres_juris_soc') %>%
  mutate(QperArea_cfperacre=DischargeJ_cf/acres_juris_soc) %>%
  unique() %>%
  ungroup() %>%
  filter(!is.na(acres_juris_soc)) %>%

  filter(!is.na(DischargeJ_cf)) 
  
  
#discharge per cubic acre without connectivity adjustment 
saveRDS(AnnualCF_J, file = paste0(outPath, 'AnnualCF_J.rds'))
write_csv(AnnualCF_J, file = paste0(outPath, 'AnnualCF_J.csv'))


AnnualCF_J_QC  <- DischargePointTrib_flow %>%
  filter(!is.na(AreaJ)) %>%
  #remove dry outfalls
  select('JURISDICTI3', 'MonitoringYear', 'AnnualCF', 'AreaJ') %>%
  unique() 

##STOP## old way to save below

saveRDS(AnnualCF_J_QC, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.rds'))
write_csv(AnnualCF_J_QC, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.csv'))
