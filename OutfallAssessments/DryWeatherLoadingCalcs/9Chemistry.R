## Import chemistry data
source('DryWeatherLoadingCalcs/1projectsetup.R')

inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')


values <- list()  
values[['JURISinTRIBS2']] <- paste0(outPath, 'JURISinTRIBS2.rds')
JURISinTRIBS2 <-readRDS(values[["JURISinTRIBS2"]])  


#2023-24
Chemdata2024R1and2 <- "C:/Users/givens/Box/OC Watersheds/Environmental Monitoring/WQIP/Monitoring and Assessment Program Implementation/MS4 Outfall Monitoring/Outfall Field Screening Follow-ups/DryWeatherDataNALsAssessment2024_draft.xlsx"

Chemdata2024R1and2 <- read_excel(Chemdata2024R1and2, skip = 1, 'Horizon Data') 
#format date
Chemdata2024R1and2 <- Chemdata2024R1and2 %>%
  separate('Collect Date', into=c("date", "time"), sep = " ") %>%
  mutate(Date = as.Date(date, format = '%m/%d/%Y')) %>%
  select(-`Entry.Set`, -Station, -`Sample ID`, -time, -`Field pH`, -`Field Temperature`, -`Field Dissolved Oxygen`, -`Field Turbidity - NTU`, -`Turbidity - NTU`, -'Temperature', `Field Specific Conductivity`)

str(Chemdata2024R1and2)

names(Chemdata2024R1and2)[names(Chemdata2024R1and2)=='Column1'] <- 'Station'


#use 1/2 the detection limit
OutfallChem2024 <- Chemdata2024R1and2 %>%
  filter(!is.na(Station2)) %>%
  filter('Total Coliform - CFU/100 mL'!= "NA") %>%
  #filter(`Sample Type` == 'Total') %>%
  gather(Parameter, Value, `2,4'-DDD - ng/L`:`Zinc - ug/L`) %>% 
  separate(Parameter, c('Parameter', 'Units'), sep = ' - ') %>%
  separate(Value, c('Qualifier', 'Result'), "(?<=[<|>|>=]) ?(?=[0-9])") %>%
  mutate(
    Result = as.numeric(ifelse(is.na(Result), Qualifier, Result)),
    Qualifier = ifelse(grepl('>|<=|>=|<', Qualifier), Qualifier, NA),
    Result = ifelse(!(Parameter %in% c('Fecal coliforms', 'E.Coli', 'Total Coliform', 'Enterococcus')),
                    ifelse(grepl('<', Qualifier),
                           Result / 2,
                           Result),
                    Result)
  ) %>%
  filter(!is.na(Result))


saveRDS(OutfallChem2024, paste0(outPath, 'OutfallChem2024.rds'))
write_csv(OutfallChem2024, paste0(outPath, 'OutfallChem2024.csv'))

values <- list()  
values[['OutfallChem2024']] <- paste0(outPath, 'OutfallChem2024.rds')
OutfallChem2024 <-readRDS(values[["OutfallChem2024"]]) 


OutfallChem2024_avg <- OutfallChem2024 %>%  
  group_by(Station2, `Sample Type`, Parameter) %>%
  #filter(!is.na(Result)) %>%
  mutate(ResultAvg=mean(Result, na.rm=TRUE)) %>%  #find average for each jurisdiction for each monitoring year
  ungroup() %>%
  unique() 


OutfallChem2024_samp <- OutfallChem2024_avg %>%
  select(Station2, `Sample Type`, Parameter, Qualifier, ResultAvg, Units) %>%
  ungroup() %>%
  unique() 

saveRDS(OutfallChem2024_avg, paste0(outPath, 'OutfallChem2024_avg.rds'))
write_csv(OutfallChem2024_avg, paste0(outPath, 'OutfallChem2024_avg.csv'))

saveRDS(OutfallChem2024_samp, paste0(outPath, 'OutfallChem2024_samp.rds'))
write_csv(OutfallChem2024_samp, paste0(outPath, 'OutfallChem2024_samp.csv'))


#Find average conc by jurisdiction (use in loading calcs for unsampled outfalls)

#join jurisdiciton

values <- list()  
values[['JURISinTRIBS2']] <- paste0(outPath, 'JURISinTRIBS2.rds')
JURISinTRIBS2 <-readRDS(values[["JURISinTRIBS2"]]) 

values[['OutfallChem2024_samp']] <- paste0(outPath, 'OutfallChem2024_samp.rds')
OutfallChem2024_j <-  readRDS(values[["OutfallChem2024_samp"]]) %>%
  
  full_join(., JURISinTRIBS2, by=c('Station2' = 'FACILITYID')) %>%
  select(Station2,`Sample Type` , Parameter, Units, Qualifier, ResultAvg, Jurisdiction, JURISDICTI3) %>%
  group_by(JURISDICTI3, Parameter) %>%
  mutate(ResultJuris=mean(ResultAvg)) %>%
  ungroup() %>%
  summarise(JURISDICTI3, Jurisdiction,`Sample Type` , Parameter, ResultJuris, Units) %>%
  select(-Jurisdiction) %>%
  unique() %>%
  filter(!is.na(JURISDICTI3))

saveRDS(OutfallChem2024_j, paste0(outPath, 'OutfallChem2024_j.rds'))
write_csv(OutfallChem2024_j, paste0(outPath, 'OutfallChem2024_j.csv'))

## Task 12*: Multiply adjusted annual flow volume results (Task 9) with chemistry data (Task 10)
values <- list()  

values[['OutfallChem2024_samp']] <- paste0(outPath, 'OutfallChem2024_samp.rds')
OutfallChem2024_samp <-  readRDS(values[["OutfallChem2024_samp"]])
values[['OutfallChem2024_j']] <- paste0(outPath, 'OutfallChem2024_j.rds')
OutfallChem2024_j <-  readRDS(values[["OutfallChem2024_j"]])


values[['AnnualFlow_US']] <- paste0(outPath, 'AnnualFlow_US.rds')
AnnualFlow_US <-  readRDS(values[["AnnualFlow_US"]]) 

values[['AnnualFlow_s']] <- paste0(outPath, 'AnnualFlow_s.rds')
AnnualFlow_s <-  readRDS(values[["AnnualFlow_s"]]) 

# select(FACILITYID, JURISDICTI3, ResultAvg, Qadj_Qall)

values[['AnnualFlow_US_j']] <- paste0(outPath, 'AnnualFlow_US_j.rds')
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]])


#sum by jurisdiction

#2024 sampled outfalls
Loads2024_samp <-left_join(OutfallChem2024_samp, AnnualFlow_s, by=c('Station2'='Facility Identifier')) %>%  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 28.3168 Liters in one cubic-ft
  filter(!is.na(ResultAvg)) %>% 
  #filter(MonitoringYear=='MY2021-22') %>%
  mutate(LoadQadj_Qall=ResultAvg*Qadj_Qall) %>%
  mutate(LoadsPoundsQS = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),
                                (LoadQadj_Qall)  * (28.3168) * 10,
                                ifelse(Units == 'mg/L',
                                       (LoadQadj_Qall) * (1/453592)  * (28.3168),
                                       ifelse(Units == 'ug/L',
                                              (LoadQadj_Qall) * (1/10^3) * (1/453592)  * (28.3168),
                                              ifelse(Units == 'ng/L',
                                                     (LoadQadj_Qall) * (1/10^6) * (1/453592) * (28.3168),
                                                     NA)
                                       )
                                )))

saveRDS(Loads2024_samp, paste0(outPath, 'Loads2024_samp.rds'))
write_csv(Loads2024_samp, paste0(outPath, 'Loads2024_samp.csv'))

#sum by jurisdiction
values <- list()  
values[['Loads2024_samp']] <- paste0(outPath, 'Loads2024_samp.rds')
Loads2024_j <-  readRDS(values[["Loads2024_samp"]]) %>%
  filter(!is.na(JURISDICTI3)) %>%
  filter(Parameter!="SpecificConductivity") %>%
  filter(Parameter!="Turbidity") %>%
  group_by(JURISDICTI3, `Sample Type` ,Parameter) %>%
  mutate(LoadsJurs=sum(LoadsPoundsQS, na.rm=TRUE)) %>%
  ungroup()

saveRDS(Loads2024_j, paste0(outPath, 'Loads2024_j.rds'))
write_csv(Loads2024_j, paste0(outPath, 'Loads2024_j.csv'))

#2024 Unsampled Outfalls
values <- list() 
values[['AnnualFlow_US_j']] <- paste0(outPath, 'AnnualFlow_US_j.rds')
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]]) %>%
  select(JURISDICTI3, QJur_US) %>%
  unique()
  
values <- list()
values[['OutfallChem2024_j']] <- paste0(outPath, 'OutfallChem2024_j.rds')
OutfallChem2024_j <-  readRDS(values[["OutfallChem2024_j"]]) %>%
  as.tibble()%>%
  as.data.frame()


values[['AnnualFlow_US']] <- paste0(outPath, 'AnnualFlow_US.rds')
AnnualFlow_US <-  readRDS(values[["AnnualFlow_US"]]) 
  

Loads2024Uns<-full_join(AnnualFlow_US_j, OutfallChem2024_j,  by=c('JURISDICTI3'='JURISDICTI3')) %>%  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 28.3168 Liters in one cubic-ft
  mutate(LoadQUns=ResultJuris*QJur_US) %>% #flow with connectivity adjustment
  mutate(LoadsPoundsQUns = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),
                                  (LoadQUns)  * (28.3168) * 10,
                                  ifelse(Units == 'mg/L',
                                         (LoadQUns) * (1/453592)  * (28.3168),
                                         ifelse(Units == 'ug/L',
                                                (LoadQUns) * (1/10^3) * (1/453592)  * (28.3168),
                                                ifelse(Units == 'ng/L',
                                                       (LoadQUns) * (1/10^6) * (1/453592) * (28.3168),
                                                       NA)
                                         )
                                  )))
saveRDS(Loads2024Uns, paste0(outPath, 'Loads2024Uns.rds'))
write_csv(Loads2024Uns, paste0(outPath, 'Loads2024Uns.csv'))

Loads2024Uns_j <-  Loads2024Uns %>%
  filter(!is.na(JURISDICTI3)) %>%
  filter(Parameter!="SpecificConductivity") %>%
  filter(Parameter!="Turbidity") %>%
  filter(Parameter!="Chloride") %>%
  unique() %>%
  select(JURISDICTI3,`Sample Type` , Parameter, Units, LoadsPoundsQUns) %>%
  unique() %>%
  group_by(JURISDICTI3,`Sample Type` , Parameter) %>%
  mutate(LoadQUns_j=sum(LoadsPoundsQUns)) %>%
  
  ungroup()

saveRDS(Loads2024Uns_j, paste0(outPath, 'Loads2024Uns_j.rds'))
write_csv(Loads2024Uns_j, paste0(outPath, 'Loads2024Uns_j.csv'))

#Combine loads from sampled and unsampled outfalls

#2024
values <- list() 
values[['Loads2024_j']] <- paste0(outPath, 'Loads2024_j.rds')
Loads2024_j <-  readRDS(values[["Loads2024_j"]]) %>%
  select(JURISDICTI3,`Sample Type`, Parameter, Units, LoadsJurs, LoadsPoundsQS) %>%
  group_by(JURISDICTI3) %>%
  distinct() %>%
  filter(!is.na(JURISDICTI3))

values <- list()
values[['Loads2024Uns_j']] <- paste0(outPath, 'Loads2024Uns_j.rds')
Loads2024Uns_j <-  readRDS(values[["Loads2024Uns_j"]]) %>%
  select(c('JURISDICTI3',`Sample Type` , 'Parameter', 'Units',  'LoadsPoundsQUns', 'LoadQUns_j')) %>%
  filter(Parameter!='SpecificConductivity'|Parameter!='Turbidity') %>%
  filter(!is.na(LoadQUns_j))

JurisLoads2024<-full_join(Loads2024_j, Loads2024Uns_j, by=c('JURISDICTI3','Sample Type', 'Parameter', 'Units')) 
JurisLoads2024$LoadsPoundsQUns[is.na(JurisLoads2024$LoadsPoundsQUns)] <- 0 #necessary to ensure Laguna Woods is included

JurisLoads2024 <- JurisLoads2024 %>%  
  group_by(JURISDICTI3, Parameter,`Sample Type`) %>%
  mutate(TotalLoadPounds=LoadsPoundsQUns+LoadsJurs) %>%
  filter(Parameter!="SpecificConductivity"|Parameter!="Turbidity") %>%
  ungroup() %>%
  unique() %>%
  select(JURISDICTI3, `Sample Type`, Parameter, LoadsJurs, LoadsPoundsQUns, TotalLoadPounds) %>%
  unique()

saveRDS(JurisLoads2024, paste0(outPath,'JurisLoads2024.rds'))
write_csv(JurisLoads2024, paste0(outPath, 'JurisLoads2024.csv'))

#NALS Parameters
values <- list() 
values[['JurisLoads2024']] <- paste0(outPath, 'JurisLoads2024.rds')
JurisLoads2024 <-  readRDS(values[["JurisLoads2024"]])


#2024
JurisLoads2024NALs <- JurisLoads2024 %>%
  filter(Parameter=='Fecal coliforms'|Parameter=='Enterococcus'|Parameter=='Nitrate+Nitrite Nitrogen'|Parameter=='Total Kjeldahl Nitrogen'|Parameter=='Phosphorus as PO4'|Parameter=='Total Suspended Solids'|Parameter=='MBAS'|Parameter=='Iron'|Parameter=='Manganese'|Parameter=='Cadmium'|Parameter=='Chromium'|Parameter=='Copper'|Parameter=='Lead'|Parameter=='Nickel'|Parameter=='Silver'|Parameter=='Zinc') %>%
  select(c('JURISDICTI3', `Sample Type` ,'Parameter','LoadsJurs' , 'LoadsPoundsQUns','TotalLoadPounds')) %>%
  unique() 

saveRDS(JurisLoads2024NALs, paste(outPath, 'JurisLoads2024NALsg.rds'))
write_csv(JurisLoads2024NALs, paste0(outPath, 'JurisLoads2024NALsg.csv'))


