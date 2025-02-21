#Bring in files from HYCSV and modify files to use in loading calcs
library(dplyr)
library(tidyr)
library(tidyverse)


getwd()
setwd("C:/Users/givens/Box/WQIP-Annual-Report-2023-24/OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/")

wd<-"C:/Users/givens/Box/WQIP-Annual-Report-2023-24/OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/"

inPath <- paste0(wd,'HachFL900_2024_raw/')
outPath <- paste0(wd, 'HachFL900_2024_provisional/')



MS4_dry <-read.csv(paste0(inPath, 'MS4_dry.CSV'))

names(MS4_dry)[names(MS4_dry) == 'Time'] <- 'Inst Time'

MS4_dry<-MS4_dry[-(1:2),]

J03_9216_2 <- MS4_dry %>%
  select(c(1,4))

names(J03_9216_2)[names(J03_9216_2) == 'J03_9216_2'] <- 'Flow (cfs)'


L05_049_1 <- MS4_dry %>%
  select(c(1,6))

names(L05_049_1)[names(L05_049_1) == 'L05_049_1'] <- 'Flow (cfs)'

L02_166_2 <- MS4_dry %>%
  select(c(1,7))

names(L02_166_2)[names(L02_166_2) == 'L02_166_2'] <- 'Flow (cfs)'

write.csv(J03_9216_2, paste0(outPath, 'J03_9216_2.csv'))
write.csv(L05_049_1, paste0(outPath, 'L05_049_1.csv'))
write.csv(L02_166_2, paste0(outPath, 'L02_166_2.csv'))


