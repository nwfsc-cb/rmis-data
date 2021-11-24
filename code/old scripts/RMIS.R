library(tidyverse)
library(here)
#this script creates a file that has all release/recovery info assigned into regions 

#################################################################################################################################################################
# STEP 1: Join Release, Recoveries, and Location information.  ------ Dont need to do step one, Eric did that in Script "00 join release and recovery.R"
#Eric has already created this file and it includes the descriptions 
rmisdat <- readRDS("data/joined_releases_recoveries_locations.rds") 
rmisdat <- readRDS(rmisdat)

##then filter out stocks that we are not interested in...
#load focal species data that Ole created, this file just has the stocks we are interested in
focal = read.csv("data/focal_spring_chinook_releases_summary.csv", header=T, stringsAsFactors = FALSE)%>%
  select(stock_location_code)
#combine dat and focal by stock_code_location, filters out stocks we are not interested in
dat_focal=semi_join(rmisdat, focal)

#now to sum total releases across rows using coded wire tag columns
dat_focal$total_release=rowSums(dat_focal[,c('cwt_1st_mark_count', 'cwt_2nd_mark_count', 'non_cwt_1st_mark_count', 'non_cwt_2nd_mark_count')], na.rm=TRUE)
#drop unnecessary columns after summing total releases, you only care about total releases of tagged fish
dat_focal=dplyr::select(dat_focal, -cwt_1st_mark_count, -cwt_2nd_mark_count,
                        -non_cwt_1st_mark_count, -non_cwt_2nd_mark_count)
#dat_focal now has all releases and recoveries, sum of CWT as total releases

############################################################################################################################################################
############################################################################################################################################################

#take out freshwater recoveries from dat_recovery
dat_recovery= dat_focal %>% 
  separate(recovery_location_code, into = c("state_code", "rest_of_rec_code"), sep = 1, remove = FALSE)%>%
  separate(rest_of_rec_code, into = c("marine_fw", "rest_of_rec_code"), sep = 1) %>%
  filter(marine_fw == 'M') %>%
  #add names to fishery gear in dat_recovery_state to make it more interprable
  #create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
  #new variable created is called 'gear_type'
  mutate(fishery_type = as.vector(cut(fishery, breaks=c(-Inf,19,29,39,49,59,69,79,89,99,Inf), 
                                      labels=c("troll",
                                               "net_seine",
                                               "aboriginal",
                                               "sport",
                                               "escapement",
                                               "test_fishery",
                                               "juv_sampling",
                                               "high_seas",
                                               "misc",
                                               "high_seas")))) %>%
  #rename state code #s to state names (ie 1-> AK) used info from locaiton.txt
  # mutate(recovery_state = recode_factor(state_code, '1' = "AK",
  #                                              '2' = "BC",
  #                                              '3' = "WA",
  #                                              '4'="ID",
  #                                              '5'="OR",
  #                                              '6'="CA",
  #                                              '7'="HS")) %>%
  
  mutate(fishery_name = recode_factor(fishery, '80' = "Hake Trawl At Sea (CA OR WA)",
                                      '800' = "Hake Trawl Shoreside (OR WA)",
                                      '802' = "Rockfish Trawl (CA OR WA)",
                                      '803'="Groundfish Trawl (CA OR WA)",
                                      '804'="Sablefish Fixed Gear (CA OR WA)",
                                      '805'="Nearshore Groundfish (CA OR)",
                                      '81'="Groundfish Observer (Gulf AK)",
                                      '812'="Rockfish Fishery (Gulf of AK)",
                                      '82'="Groundfish Observer (Bering Sea)",
                                      '83'="Foreign Research Vessels",
                                      '84'="Foreign Mothership",
                                      '85'="Ocean Trawl By-catch",
                                      '87'="Squid Gillnet By-catch", 
                                      '88'="Juv Sampling",
                                      '89'="Other",
                                      '40' = "Ocean Sport",
                                      '41' = "Charter Sport",
                                      '42' = "Private Sport",
                                      '43'= "Jetty Sport",
                                      '44'="Columbia River Sport",
                                      '45'="Estuary Sport",
                                      '48'="Terminal Sport",
                                      '20' = "Ocean Gillnet",
                                      '21' = "Columbia Gillnet",
                                      '22' = "Coastal Gillnet",
                                      '23'= "Mixed Net and Seine",
                                      '24'="FW Net",
                                      '25'="Commercial Seine",
                                      '26'="Terminal Seine",
                                      '27'="FW Seine",
                                      '28'="Other Net",
                                      '29'="Other Seine",
                                      '10' = "Ocean Troll (non-treaty)",
                                      '11' = "Ocean Troll- Day Boat",
                                      '12' = "Ocean Troll- Trip",
                                      '13'= "Ocean Troll- Freezer Boat",
                                      '14'="Ocean Troll- Ice Boat",
                                      '15'="Treaty Troll",
                                      '16'="Terminal Troll",
                                      '17'="Non-treaty/treaty Troll",
                                      '18'="Aboriginal Troll",
                                      '19'="Other Troll")) %>%
  separate(recovery_date, into = c("rec_year", "rest_of_rec_date"), sep = 4) %>%
  separate(rest_of_rec_date, into = c("rec_month", "rec_day"), sep= 2) %>%
  mutate(rec_year = as.numeric(rec_year))

#################################################################################################################################################################
#STEP 2: Assign recoveries into regions 
################################################################################################################################################################
#codes updated jan 2020 to use the NMFS stat areas in AK 
rec_codes_lookup = read.csv("data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv", stringsAsFactors = FALSE) %>%
  mutate(recovery_location_code =location_code) %>%
  mutate(region = Rec.area.Sullaway.Shelton.NMFSstat) %>% 
  select(c(recovery_location_code, region)) %>% #change so that RMIS has same column labels 
  distinct(recovery_location_code, .keep_all = TRUE) #remove duplicates 

df_recovery <- left_join(dat_recovery, rec_codes_lookup, by = "recovery_location_code")

#na is a list of recovery codes that did not match up to Oles look up table - went and added it back in to the lookup data base
na<- df_recovery %>% filter(is.na(region)) #all the recoveries that do not match up with a recovery location code in look up table // will want to use this later for when add in regions
# check <- df_recovery %>% filter(region == "HSEA") %>%
#   filter(!fishery_name == "Hake Trawl At Sea (CA OR WA)")
na_id <- as.data.frame(unique(na$recovery_location_code)) %>%
  rename(recovery_location_code = 'unique(na$recovery_location_code)') %>%
  left_join(location) #list of unique codes that are not in look up table (includes with and without lat long)
# write.csv(na_id, "data/missing_codes.csv")

#################################################################################################################################################################
#STEP 4: SNOUTBASE - DATA PARCE AND JOIN INTO RMIS 
################################################################################################################################################################
#SNOUTBASE- Use to match lat and long into to RMIS Highseas to get better recovery information so the recoveries dont snap to a grid
setwd("~/Documents/GitHub/Chinook_Bycatch/data/ASHOP/snoutbase")
snout = read.csv("A-SHOP_Snoutbase_122118.csv", stringsAsFactors = FALSE) %>%
  mutate(tag_code = str_pad(CWTCode, width= 6, pad = "0", side="left")) %>% #add leading zero that r removes
  filter(!CWTCode == "-" & !CWTCode == " ") %>% #remove blanks in snout data set
  separate(Date, c("rec_month", "rec_day", "delete"), sep= '/') %>%
  mutate(rec_month = as.numeric(rec_month)) %>% 
  mutate(rec_day = as.numeric(rec_day)) %>% 
  mutate(rec_year= as.numeric(Year)) %>%
  mutate(Long= as.numeric(Long)) %>%
  mutate(Long = ifelse(rec_year == 2009, Long * -1, Long)) %>%#Fixes this --> ALL RECOVERIES FROM 2009 ARE PROBABLY MISSING A (-) IN LONGITUDE
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code") )

#is there fish in snoutbase not in rmis? #ANSWER = NO ALL SNOUTBASE FISH ARE IN RMIS
#get unique list of tag codes for each
#          rmis_u <- as.data.frame(unique(dat_short$tag_code))
#          snout_u <-as.data.frame(unique(snout$tag_code))

#          colnames(snout_u)[which(names(snout_u) == "unique(snout$tag_code)")] <- "name"
#          colnames(rmis_u)[which(names(rmis_u) == "unique(dat_short$tag_code)")] <- "name"

#         b <- left_join(snout_u, rmis_u)


#now match lat and long into to RMIS Highseas to get better recovery information so they recoveries dont snap to a grid

#snoutbase is just for highseas fisheries South of AK
hs_dat <- df_recovery %>% 
  filter((fishery_name %in% c("Hake Trawl At Sea (CA OR WA)","Ocean Trawl By-catch","Groundfish Trawl (CA OR WA)", "Rockfish Trawl (CA OR WA)", "Nearshore Groundfish (CA OR)", "Hake Trawl Shoreside (OR WA)") & rec_year > 2005)) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
  mutate(rec_month = as.numeric(rec_month)) %>% 
  mutate(rec_day = as.numeric(rec_day)) %>% 
  mutate(rec_year= as.numeric(rec_year)) %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove = FALSE )

all_dat <- left_join(hs_dat, snout, by =  "id") %>% #combine so that rmis lat and longs can be replaced by snout 
  filter(!is.na(Year)) %>%
  mutate(Lat = as.numeric(Lat)) %>%
  mutate(Long = as.numeric(Long)) %>%
  #add an updated region to these based on lat long
  mutate(region = as.vector(cut(Lat, breaks=c(Inf,46.63611,45.76666,44.015, 42.6725, 42, 38.958333, -Inf), 
                                labels=c(  
                                  "NCA",
                                  "MEN",
                                  "SOR",
                                  "COR",
                                  "NOR",
                                  "COL",
                                  "WAC"))))%>% 
  select(id,rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type, fishery_name, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level, brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
         stock_location_code, total_release, detection_method, recovery_description)

df_recovery$Haul <- NA
df_recovery$Lat <- NA
df_recovery$Long <- NA
df_recovery$Ln <- NA
df_recovery$Wt <- NA
df_recovery$Sex <- NA

dat_everything <- df_recovery %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove =FALSE ) %>%
  filter(!(fishery_name %in% c("Hake Trawl At Sea (CA OR WA)","Ocean Trawl By-catch","Groundfish Trawl (CA OR WA)", "Rockfish Trawl (CA OR WA)", "Nearshore Groundfish (CA OR)", "Hake Trawl Shoreside (OR WA)") & rec_year > 2005)) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
  select(id, rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type,fishery_name, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level,  brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
         stock_location_code, total_release,detection_method, recovery_description) %>%
  rbind(all_dat, deparse.level = 1, make.row.names = TRUE)  %>%
  mutate(Latitude = case_when(Lat > 0 ~ Lat,       
                              TRUE ~ latitude)) %>%  
  mutate( Longitude = case_when(Lat > 0 ~ Long, 
                                TRUE ~longitude)) %>%
  select(-c(longitude, Long, latitude, Lat)) #now this should have all snout and rmis data with combined lat and longs

#################################################################################################################################################################
# SAVE TIDY DATA FILES 
################################################################################################################################################################
#
#  write.csv(dat_everything, "data/rmis_release_recovery.csv" )