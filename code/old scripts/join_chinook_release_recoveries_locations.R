library(dplyr)
library(ggplot2)

# Load release data
release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 
release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
  release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
  non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
  stock_location_name, release_location_state, release_location_rmis_region, 
  release_location_rmis_basin) %>% 
  dplyr::rename(tag_code = tag_code_or_release_id)

# Sum up the total releases, drop unneccessary columns

release = dplyr::mutate(release, 
                        cwt_1st_mark_count = ifelse(is.na(cwt_1st_mark_count), 0, cwt_1st_mark_count),
                        cwt_2nd_mark_count = ifelse(is.na(cwt_2nd_mark_count), 0, cwt_2nd_mark_count),
                        non_cwt_1st_mark_count = ifelse(is.na(non_cwt_1st_mark_count), 0, non_cwt_1st_mark_count),
                        non_cwt_2nd_mark_count = ifelse(is.na(non_cwt_2nd_mark_count), 0, non_cwt_2nd_mark_count),
                        total_release = cwt_1st_mark_count + cwt_2nd_mark_count + non_cwt_1st_mark_count + non_cwt_2nd_mark_count)

#fix zeros that are added
temp.tag <- release$tag_code
temp.tag[which(nchar(temp.tag)==5)] <- paste("0",temp.tag[nchar(temp.tag)==5],sep="")
release$tag_code <- temp.tag

# pull out release year
release$release_year = substr(release$first_release_date, 1, 4)
release = dplyr::select(release, -first_release_date)

# recoveries
recover = read.csv("data/chinook/recoveries_1973.csv", stringsAsFactors = FALSE)
recover = dplyr::select(recover, tag_code, recovery_id, recovery_date, fishery, gear, 
  recovery_location_code, recovery_location_name, estimated_number, length)
for(y in 1974:2016) {
  #  names change slightly in 2015,
  temp = read.csv(paste0("data/chinook/recoveries_",y,".csv"), 
    stringsAsFactors = FALSE)
  temp = dplyr::select(temp, tag_code, recovery_id, recovery_date, fishery, gear, 
    recovery_location_code, recovery_location_name, estimated_number, length)
  recover = rbind(recover, temp)
}

#fix zeros that are added
temp.tag <- recover$tag_code
temp.tag[which(nchar(temp.tag)==5)] <- paste("0",temp.tag[which(nchar(temp.tag)==5)],sep="")
recover$tag_code <- temp.tag

recover = dplyr::filter(recover, !is.na(estimated_number)) %>% 
  filter(tag_code != "")

# Join by tag code
release = dplyr::rename(release, tag_code=tag_code)
dat = left_join(recover, release) # join by tag code

# pull in location data from RMIS
locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description","rmis_region","rmis_basin")]
locations = rename(locations, recovery_location_code = location_code,
  recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
dat = left_join(dat, locations)

dat = rename(dat, recovery_rmis_region=rmis_region, recovery_rmis_basin=rmis_basin)

# Code to group recoveries by region
run_names = c("Spr","Sum","Fa","Win","Hyb","Land","LateFa","URB-LateFa")
dat$stock_coarse = paste0(dat$release_location_rmis_region, "-",run_names[dat$run])
dat$recovery_year = as.numeric(substr(dat$recovery_date,1,4))
dat$recovery_month = as.numeric(substr(dat$recovery_date,5,6))
recovery_period=data.frame("recovery_month"=1:12, 
  "recovery_period"=c(1,1,2,2,3,3,4,4,5,5,6,6))
dat = left_join(dat, recovery_period)

saveRDS(dat,file="releases_recoveries.Rds")

###################################################
# Example analysis:
# identify salish sea regions
dat$recovery_coarse = NA
dat$recovery_coarse[which(dat$recovery_rmis_region %in% c("HOOD","JUAN","SPS","MPS","NPS","SKAG","NOWA","FRTH","GST","JNST"))] = "Salish Sea"
salish = dplyr::filter(dat, recovery_coarse=="Salish Sea") %>% 
  dplyr::filter(recovery_year > 2005) %>% 
  group_by(recovery_year, recovery_period, stock_coarse) %>% 
  summarize(n = sum(estimated_number, na.rm=T)) %>% 
  group_by(recovery_year, recovery_period) %>% 
  mutate(p = n/sum(n))

mean_salish = group_by(salish, recovery_period, stock_coarse) %>% 
  summarize(mean_p = mean(p, na.rm=T), mean_n = mean(n, na.rm=T)) %>% 
  arrange(recovery_period, -mean_p) %>%
  dplyr::rename(p = mean_p)

aql <- reshape2::melt(mean_salish[,c("recovery_period","stock_coarse","p")], id.vars = c("recovery_period","stock_coarse"))
aqw <- reshape2::dcast(aql, recovery_period ~ variable + stock_coarse)
write.csv(aqw,"output/mean_proportions_SalishSea.csv",row.names = FALSE)
# At this point, 'dat' is the complete release-recovery dataset for all years, coastwide

###################################################
# Example analysis:
# filtering
# location codes: 2(BC), 3 (WA), 5 (OR), 6 (CA), etc.
dat = mutate(dat, recovery_state = substr(recovery_location_code, 1,2)) %>%
  mutate(recovery_year = substr(recovery_date, 1,4),
    recovery_month = substr(recovery_date, 5,6))