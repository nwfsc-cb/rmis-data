library(tidyverse)
library(here)
#this script joins releases and recoveries
#Creates a file that has all release/recovery and locations when applicable

#################################################################################################################################################################
                                                                  # STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################
#create dat_focal which is release and recoveries combined
#load recoveries

rmisdat<- load("/Users/ole.shelton/Github/Orca_Salmon_DATA/Recoveries/_All Chinook/all_chinook 2020-07-10.RData")

dat_recovery <- all_chinook$recover

#add names to fishery gear in dat_recovery_state to make it more interprable
#create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
#new variable created is called 'gear_type'
#take out freshwater recoveries from dat_recovery
dat_recovery= dat_recovery %>%
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
  filter(fishery_type == "high_seas") %>%
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
                                      '89'="Other")) %>%
  separate(recovery_date, into = c("rec_year", "rest_of_rec_date"), sep = 4) %>%
  separate(rest_of_rec_date, into = c("rec_month", "rec_day"), sep= 2) %>%
  mutate(rec_year = as.numeric(rec_year))

#################################################################################################################################################################
      #STEP 2: Assign recoveries into regions 
################################################################################################################################################################

# pull in location data from RMIS
locations = read.csv("/Users/ole.shelton/Github/rmis-data/data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
                   recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
#fix locations file before left joining then may not experience as many duplicates 
locations <- locations %>%
  filter(!is.na(latitude))

locations <- locations %>% 
  distinct(recovery_location_code, .keep_all = TRUE)   #took out duplicate recovery codes, keep.all = keeps the first row of values for each recovery code
#i think this is ok because alot of the duplicate data were just off my a 0.001 degree in the lat and long, not huge differences 
df_recovery <- left_join(dat_recovery, locations)

### Cull recoveries to only include the US west coast
df_recovery1 <- df_recovery %>% filter(latitude<=49)



#df_recovery <- left_join(dat_recovery, all_rec_codes_locations, by = "recovery_location_code")
#na is a list of recovery codes that did not match up to Oles look up table
#229 recovery location codes did not match up
#na<- everything1 %>% filter(is.na(region)) #all the recoveries that do not match up with a recovery location code in look up table // will want to use this later for when add in regions
#recovery_location_code <- unique(na$recovery_location_code)
#recovery_location_code <- as.data.frame(recovery_location_code)
#recovery_location_code$id <- 1
#c<- left_join(locations, recovery_location_code) #list of unique codes that are not in look up table (includes with and without lat long)
#id<- c %>% filter(id == 1) #list of unique codes that are not in look up table (includes with and without lat long)

#z <- id %>% filter(is.na(latitude)) # list of recovery codes that are not in lookup tanble AND that dont have lat and long - need to look up in RMIS data base?
#lat_long <- id %>% filter(!is.na(latitude)) #list of recovery codes that are not in table but DO have lat long- will use for convex hull stuff
#lat_long <- lat_long %>%
#  dplyr::select(-c(id))
#x <- left_join(dat_recovery, z) 
#v <- x %>% filter(!is.na(id)) #list of # of recoveries that dont have lat long in recovery codes

#################################################################################################################################################################
#STEP 4: SNOUTBASE - DATA PARCE AND JOIN INTO RMIS 
################################################################################################################################################################
#SNOUTBASE- Use to match lat and long into to RMIS Highseas to get better recovery information so the recoveries dont snap to a grid
setwd("/Users/ole.shelton/Github/Orca_Salmon_DATA/Recoveries/ASHOP/snoutbase")
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
  filter((fishery_name %in% c("Hake Trawl At Sea (CA OR WA)",
                              "Ocean Trawl By-catch",
                              "Groundfish Trawl (CA OR WA)", 
                              "Rockfish Trawl (CA OR WA)", 
                              "Nearshore Groundfish (CA OR)", 
                              "Hake Trawl Shoreside (OR WA)") & rec_year > 2005)) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
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
  dplyr::select(id,rec_year, rec_month, rec_day, tag_code, recovery_id,region,
        fishery, fishery_type, fishery_name, gear, latitude, longitude, recovery_location_name,
        Lat, Long, estimated_number, estimation_level, detection_method, recovery_description)

df_recovery$Haul <- NA
df_recovery$Lat  <- NA
df_recovery$Long <- NA
df_recovery$Ln   <- NA
df_recovery$Wt   <- NA
df_recovery$Sex  <- NA

dat_everything <- df_recovery %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove =FALSE ) %>%
  filter(!(fishery_name %in% 
             c("Hake Trawl At Sea (CA OR WA)",
               "Ocean Trawl By-catch",
               "Groundfish Trawl (CA OR WA)",
               "Rockfish Trawl (CA OR WA)",
               "Nearshore Groundfish (CA OR)",
               "Hake Trawl Shoreside (OR WA)") & rec_year > 2005)) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
  dplyr::select(id,rec_year, rec_month, rec_day, tag_code, recovery_id,region,
                fishery, fishery_type, fishery_name, gear, latitude, longitude, recovery_location_name,
                Lat, Long, estimated_number, estimation_level, detection_method, recovery_description) %>%
  rbind(all_dat, deparse.level = 1, make.row.names = TRUE)  %>%
  mutate(Latitude = case_when(Lat > 0 ~ Lat,       
                              TRUE ~ latitude)) %>%  
  mutate( Longitude = case_when(Lat > 0 ~ Long, 
                                TRUE ~longitude)) %>%
  dplyr::select(-c(longitude, Long, latitude, Lat)) #now this should have all snout and rmis data with combined lat and longs
