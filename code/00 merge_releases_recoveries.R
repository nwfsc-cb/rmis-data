
library(tidyverse)
#this script joins releases and recoveries
#Creates a file that has all release/recovery and locations when applicable

#################################################################################################################################################################
# STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################
#create dat_focal which is release and recoveries combined
#load recoveries

recover = read.csv("data/chinook/recoveries_1973.csv", stringsAsFactors = FALSE)
recover = dplyr::select(recover, species,
  tag_code, recovery_id, 
  recovery_date, fishery, gear, sex, length, length_type, length_code,
  recovery_location_code, recovery_location_name, 
  estimation_level, estimated_number, detection_method)
for(y in 1974:2017) {
  #  names change slightly in 2015,
  temp = read.csv(paste0("data/chinook/recoveries_",y,".csv"), 
    stringsAsFactors = FALSE)
  temp = dplyr::select(temp, species, tag_code, recovery_id, recovery_date, fishery, gear, 
    sex, length, length_type, length_code,
    recovery_location_code, recovery_location_name, estimation_level, 
    estimated_number, detection_method)
  recover = rbind(recover, temp)
}

recover = dplyr::filter(recover, !is.na(estimated_number)) %>% 
  filter(tag_code != "")

#load release data
release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 
release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
  release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
  non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
  stock_location_name, release_location_state, release_location_rmis_region, 
  release_location_rmis_basin, avg_length) %>% 
  dplyr::rename(tag_code = tag_code_or_release_id)

#left_join combines the two data frames into dat
dat = left_join(recover, release) 
##dat should have all releases and recoveries

# pull in location data from RMIS
locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description","rmis_region","rmis_basin")]
locations = rename(locations, recovery_location_code = location_code,
  recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
dat = left_join(dat, locations)

dat = rename(dat, recovery_rmis_region=rmis_region, recovery_rmis_basin=rmis_basin)

saveRDS(dat, "data/joined_releases_recoveries_locations.rds")