
### LIBRARIES ##########################################################################################################

library(tidyverse)
library(feather)
library(mondate)
library(survival)
library(survminer)
library(factorMerger)

### LOADING DATA #######################################################################################################

raw_data <- feather('data/raw_data_surv.feather')
raw_data <- as.data.frame(raw_data)

### PREPROCESSING ######################################################################################################

preprocessed_data <- raw_data %>%
  filter(YearOfStartingOfTheBusiness == 2011) %>%
  mutate_at(c('MainAddressCounty',
              'MainAddressVoivodeship',
              'CorrespondenceAddressVoivodeship',
              'CorrespondenceAddressCounty'), toupper) %>%
  mutate_at(c('StartingDateOfTheBusiness', 'DateOfTermination'), as.Date) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(NIP = as.character(NIP),
         Terminated = as.integer(!is.na(DateOfTermination)),
         DurInMonths = as.integer(
           mondate(
             if_else(is.na(DateOfTermination), Sys.Date(), DateOfTermination))
           - mondate(StartingDateOfTheBusiness)),
         MainAddressVoivodeship = fct_lump(MainAddressVoivodeship, n = 16)) %>%
  filter(DurInMonths > 0)

save(preprocessed_data, file = 'results/100_PreprocessedData.Rdata')
