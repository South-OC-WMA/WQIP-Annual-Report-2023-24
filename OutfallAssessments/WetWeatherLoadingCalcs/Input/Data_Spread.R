library(data.table)
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
#library(xlsx)
library(openxlsx)

wd <- 'C:/Users/givens/Box/OC Environmental Resources/OC Watersheds/Monitoring and Assessment/'

inPath <- paste0(wd,'Data Requests/KKelly/Outfall Data/')
#inPath <- paste0(wd,'Water Quality Data/Intermediate Data Products/Data/OUTPUT/Wet Weather Outfall Data')
outPath <- paste0(wd, 'Water Quality Data/Intermediate Data Products/R - Data Workup/')

list.files(inPath)

#"

#Wet Weather Chemistry Outfalls (2 ways to import)

#url_Outfall_WW <- 'C:/Users/givens/Box/OC Environmental Resources/OC Watersheds/Monitoring and Assessment/Data Requests/KKelly/Outfall Data/WW_Outfall_22-23.xlsx'  
#OutfallWW <- read_excel(url_Outfall_WW)

#Wet Weather Chemistry Outfalls

OutfallWW <- read.csv(paste0(inPath,'WW Outfall data_ 23-24.csv'))
#OutfallWW <- read_excel(paste0(inPath,'WW Outfall data_23-24.xlsx'))
#OutfallWW <- read.xlsx(paste0(inPath,"WW Outfall data_23-24.xlsx"))

OutfallWW<- OutfallWW%>%
  mutate(Result = ifelse(!is.na(Qualifier), paste0(Qualifier, Result), Result))
str(OutfallWW)
OutfallWW_spread <- OutfallWW %>%
  #select(!...1) %>%
  select(Entry.Set,LogNumber,Date,Station,Sample.Type,Filtered,Parameter,Result, Composite.Begin, Composite.End, Num.Sample) %>%
  filter(Station != 'LANTu1')

#Each row of output must be identified by a unique combination of keys.
#Keys are shared for 6 rows:
 # * 664, 665
#* 666, 667
#* 668, 669

#OutfallWW_spread <-  OutfallWW_spread[-1309, ] #NA2800 for Iron
#OutfallWW_spread <-  OutfallWW_spread[-1217, ] #NA110 for Manganese
#OutfallWW_spread <-  OutfallWW_spread[-1319, ] #NA0.710 for Selenium
 
OutfallWW_spread <- OutfallWW_spread %>%

spread(Parameter,Result)

write.xlsx(OutfallWW_spread, paste0(outPath, 'SDR_WQIP_Outfalls_Wet_202324.xlsx'), sheetName = 'Outfalls_WW', showNA = FALSE)



#Dry Weather Chemistry Outfalls
OutfallDW <- read.csv(paste0(outPath,'SDR_WQIP_Outfalls_DryWeather.csv'))

OutfallDW<- OutfallDW%>%
  mutate(Result = ifelse(!is.na(Qualifier), paste0(Qualifier, Result), Result))

OutfallDW_spread <- OutfallDW %>%
  select(Entry.Set,LogNumber,Date,Station,Sample.Type,Filtered,Parameter,Result)%>%
  spread(Parameter,Result)

write.xlsx(OutfallDW_spread, paste0(outPath, 'SDR_WQIP_Outfalls_Dry.xlsx'), sheetName = 'Outfalls_DW', showNA = FALSE)



#Wet Weather Field Data Outfalls
OutfallWWF <- read_excel(paste0(inPath,'WW Outfall data_ 23-24_FLD.xlsx'))

OutfallWWF_spread <- OutfallWWF %>%
  select(hsn,collect_date,collection_site,analyte_name,result)%>%
  spread(analyte_name,result) %>%
  select(hsn, collection_site, collect_date, 'Dissolved Oxygen', pH,'Specific Conductivity', Temperature, Turbidity )

write.xlsx(OutfallWWF_spread, paste0(outPath, 'SDR_WQIP_Outfalls_Wet_FLD_202324.xlsx'), sheetName = 'Outfalls_WW_FLD_202324', showNA = FALSE)



#Dry Weather Field Data Outfalls
OutfallDWF <- read.csv(paste0(outPath,'SDR_WQIP_DW_Field.csv'))

OutfallDWF_spread <- OutfallDWF %>%
  select(project_seq,hsn,collect_date,collection_site,sample_type_desc,analyte_name,result)%>%
  spread(analyte_name,result)

write.xlsx(OutfallDWF_spread, paste0(outPath, 'SDR_WQIP_Outfalls_Dry_FLD.xlsx'), sheetName = 'Outfalls_DW_FLD', showNA = FALSE)

