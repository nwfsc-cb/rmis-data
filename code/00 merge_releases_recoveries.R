library(here)
library(tidyverse)
#this script joins releases and recoveries
#Creates a file that has all release/recovery and locations when applicable

#################################################################################################################################################################
# STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################
#create dat_focal which is release and recoveries combined
#load recoveries

recover = readRDS("data/chinook/recoveries_1973.rds")
recover = dplyr::select(recover, species,
  tag_code, recovery_id, 
  recovery_date, fishery, gear, sex, length, length_type, length_code,
  recovery_location_code, recovery_location_name, sampling_site, sample_type,
  estimation_level, estimated_number, detection_method)
for(y in 1974:2022) {
  #  names change slightly in 2015,
  temp = readRDS(paste0("data/chinook/recoveries_",y,".rds"))
  if("number_cwt_estimated" %in% names(temp)) { # naming changed ~ 2020
    temp <- dplyr::rename(temp, estimated_number = number_cwt_estimated)
  } 
  temp = dplyr::select(temp, species, tag_code, recovery_id, recovery_date, fishery, gear, 
    sex, length, length_type, length_code,
    recovery_location_code, recovery_location_name,  sampling_site, sample_type,
    estimation_level, estimated_number, detection_method)
  recover = rbind(recover, temp)
}

## OLE THINKS WE SHOULDN'T DROP ENTRIES WITHOUT ESTIMATED NUMBERS. 
## THEY CAN BE FILTERED OUT LATER FOR SOME PURPOSES but should be kept for others.
recover = recover %>% filter( tag_code != "") #%>%
  #filter(!is.na(estimated_number))

# if you are OLE, write just recover to file for use in other scripts.
all_chinook <- list(recover=recover,date=date())
save(file="all_chinook.RData",all_chinook)

#load release data
#release = read.csv("/Users/ole.shelton/Github/rmis-data/data/chinook/all_releases.csv", header=T, stringsAsFactors = FALSE) 
release = readRDS("data/chinook/all_releases_oct2023.rds") 

release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
  release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
  non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
  stock_location_name, release_location_state, release_location_rmis_region, 
  release_location_rmis_basin, avg_length) %>% 
  dplyr::rename(tag_code = tag_code_or_release_id)

#left_join combines the two data frames into dat
dat = left_join(recover, release) 
##dat should have all releases and recoveries
saveRDS(dat, "data/joined_releases_recoveries.rds")
# pull in location data from RMIS
locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description","rmis_region","rmis_basin")]
locations = rename(locations, recovery_location_code = location_code,
  recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
dat2 = left_join(dat, locations)

dat = rename(dat, recovery_rmis_region=rmis_region, recovery_rmis_basin=rmis_basin)

saveRDS(dat, "data/joined_releases_recoveries_locations.rds")
