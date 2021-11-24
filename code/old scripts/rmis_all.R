#this script runs rmis_base which adds locations to associated rmis data by recovery code 
setwd("~/Documents/GitHub/rmis/scripts_gs")
source("rmis_base.R")

#region assignment for rmis
source("assigning_regions.R")

#then we parce snoutbase data and assign it to rmis data and replace some rmis data with more accurate lat and long recovery locations
setwd("~/Documents/GitHub/Chinook_Bycatch/scripts")
source("snoutbase_rmis_join.R")

#AFTER RUNNING ALL OF THIS DAT IS THE FILE THAT HAS COMPLETE DATA SET WITH UPDATED LAT AND LONGS FROM SNOUTBASE AND REGIONS ASSIGNED

newdat = dplyr::filter(dat, recovery_year >= 1980) %>% 
  dplyr::group_by(fishery) %>% 
  dplyr::mutate(n = n()) %>% 
  dplyr::filter(n > 1000) %>% 
  dplyr::select(-n) %>% 
  dplyr::group_by(shelton) %>% 
  dplyr::mutate(n = n()) %>% 
  dplyr::filter(n > 1000) %>% 
  dplyr::select(-n)
newdat$shelton = as.factor(as.character(newdat$shelton))
newdat$shelton_fishery_stock = paste0(newdat$shelton, newdat$fishery,newdat$fram_stock)
newdat = dplyr::filter(newdat, recovery_month%in%c(5:10))

newdat = dplyr::filter(newdat, !is.na(length))
newdat = group_by(newdat, shelton_fishery_stock) %>% 
  mutate(n = n()) %>% 
  dplyr::filter(n >= 50) %>% 
  dplyr::select(-n)

# summarize means
ms = dplyr::group_by(newdat, brood_year, age, fishery, shelton, fram_stock) %>%
  dplyr::summarize(eff = sum(estimated_number, na.rm=T),
    wm = sum(estimated_number*length, na.rm=T)/eff) %>% 
  dplyr::filter(!is.na(shelton))


ms$fishery_shelton = paste0(ms$fishery, ms$fram_stock)
ms$fishery = as.factor(ms$fishery)
ms$fram_shelton = paste0(ms$fram_stock, ms$shelton)
ms$fram_fishery = paste0(ms$fram_stock, ms$fishery)

ms = dplyr::group_by(ms, fram_fishery) %>% 
  dplyr::mutate(n = n()) %>% 
  dplyr::filter(n >= 100) %>% 
  dplyr::group_by(fram_shelton) %>%
  dplyr::mutate(n = n()) %>% 
  dplyr::filter(n >= 100)

fit = mgcv::gam(log(wm) ~ s(brood_year,by=as.factor(age)) + 
    as.factor(age) + as.factor(fishery)*as.factor(age) + 
    fishery_shelton + 
    as.factor(age), 
  data=dplyr::filter(ms, fram_stock==35))
