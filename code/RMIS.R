library(tidyverse)
library(here)
#this script joins releases and recoveries
#Creates a file that has all release/recovery and locations when applicable

#################################################################################################################################################################
                                                                  # STEP 1: Join Release, Recoveries, and Location information. 
################################################################################################################################################################
#create dat_focal which is release and recoveries combined
#load recoveries

#   recover = read.csv("data/chinook/recoveries_1973.csv", stringsAsFactors = FALSE)
#   recover = dplyr::select(recover, species,
#     tag_code, recovery_id, 
#     recovery_date, fishery, gear, sex, length, length_type, length_code,
#     recovery_location_code, recovery_location_name, 
#     estimation_level, estimated_number, detection_method, recovery_description)
#   for(y in 1974:2017) {
#     #  names change slightly in 2015,
#     temp = read.csv(paste0("data/chinook/recoveries_",y,".csv"), 
#                     stringsAsFactors = FALSE)
#     temp = dplyr::select(temp, species, tag_code, recovery_id, recovery_date, fishery, gear, 
#       sex, length, length_type, length_code,
#       recovery_location_code, recovery_location_name, estimation_level, 
#       estimated_number, detection_method, recovery_description)
#     recover = rbind(recover, temp)
#   }
#   
#   recover = dplyr::filter(recover, !is.na(estimated_number)) %>% 
#     filter(tag_code != "")
#   
#   #load release data
#   release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 
#   release = dplyr::select(release, tag_code_or_release_id, run, brood_year, first_release_date,
#                           release_location_code, stock_location_code, cwt_1st_mark_count, cwt_2nd_mark_count,
#                           non_cwt_1st_mark_count, non_cwt_2nd_mark_count, release_location_name,
#                           stock_location_name, release_location_state, release_location_rmis_region, 
#                           release_location_rmis_basin) %>% 
#     dplyr::rename(tag_code = tag_code_or_release_id)
# 
# #left_join combines the two data frames into dat
# dat = left_join(recover, release) 
# ##dat should have all releases and recoveries

#Eric has already created this file and it includes the descriptions - Now just need to get focal to match in. 
rmisdat<- here::here("data","joined_releases_recoveries_locations.rds") %>%
             readRDS(rmisdat)

##then filter out stocks that we are not interested in...
#load focal species data that Ole created, this file just has the stocks we are interested in
focal = read.csv("data/focal_spring_chinook_releases_summary.csv", header=T, stringsAsFactors = FALSE) 
#focal now just has one column, stock_location code
focal=dplyr::select(focal, stock_location_code)
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
  separate(recovery_location_code, into = c("state_code", "rest_of_rec_code"), sep = 1, remove = FALSE)

dat_recovery= dat_recovery %>%
  separate(rest_of_rec_code, into = c("marine_fw", "rest_of_rec_code"), sep = 1) %>%
  filter(marine_fw == 'M')
#add names to fishery gear in dat_recovery_state to make it more interprable
#create categories by gear based on Github/RMIS/RMIS Documentation/PSC_V41_Specification 
#new variable created is called 'gear_type'
dat_recovery$fishery_type <-  as.vector(cut(dat_recovery$fishery, breaks=c(-Inf,19,29,39,49,59,69,79,89,99,Inf), 
                                            labels=c("troll",
                                                     "net_seine",
                                                     "aboriginal",
                                                     "sport",
                                                     "escapement",
                                                     "test_fishery",
                                                     "juv_sampling",
                                                     "high_seas",
                                                     "misc",
                                                     "high_seas")))



#rename state code #s to state names (ie 1-> AK) used info from locaiton.txt
dat_recovery$recovery_state <- recode_factor(dat_recovery$state_code, '1' = "AK",
                                             '2' = "BC",
                                             '3' = "WA",
                                             '4'="ID",
                                             '5'="OR",
                                             '6'="CA",
                                             '7'="HS")
#Split WA and OR so Columbia river is included 
#new column is called release_loc_domain
#codes from Chap 13 in PSC_V41, RMIS documentation, using region codes
dat_recovery$release_loc_domain <- dat_recovery$release_location_rmis_region %>%
  recode(CECR= "COL", CRGN= "COL", LOCR= "COL", UPCR= "COL", SNAK= "COL") %>%
  recode(SEAK= "AK", CNAK= "AK", NOAK= "AK", CEAK= "AK", WEAK= "AK", AKGN= "AK") %>%
  recode(FRTH= "BC", NASK= "BC", GST= "BC", WCVI= "BC", JNST= "BC", COBC= "BC", QCI= "BC", TRAN= "BC", BCGN= "BC") %>%
  recode(GRAY= "WA", HOOD= "WA", JUAN= "WA", MPS= "WA", NOWA= "WA", NWC= "WA", SKAG= "WA", SPS= "WA", NPS= "WA", WILP= "WA", WAGN= "WA") %>%
  recode(NOOR= "OR", SOOR= "OR", ORGN= "OR") %>%
  recode(NOCA= "CA", CECA= "CA",SOCA= "CA",KLTR= "CA", SAFA= "CA", SJOA= "CA", CAGN= "CA") %>%
  recode(LOYR="YR", UPYR= "YR", YRGN= "YR") %>%
  recode(ALSR= "TR", CHIL= "TR", STUN= "TR", TAWH= "TR", TRGN= "TR") 
#BC has a lot of regions that are left blanks, so had to tell it to put BC in those blank areas
dat_recovery$release_loc_domain[dat_recovery$release_loc_domain == ""] <- NA
dat_recovery$release_loc_domain <- dat_recovery$release_loc_domain %>%
  recode('NA' = "BC")

dat_recovery$fishery_name <- recode_factor(dat_recovery$fishery, '80' = "Hake Trawl At Sea (CA OR WA)",
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
                                           '19'="Other Troll")

dat_recovery <- dat_recovery %>% 
  separate(recovery_date, into = c("rec_year", "rest_of_rec_date"), sep = 4) 

dat_recovery <- dat_recovery %>%
  separate(rest_of_rec_date, into = c("rec_month", "rec_day"), sep= 2)
dat_recovery$rec_year <- as.numeric(dat_recovery$rec_year)

#
#################################################################################################################################################################
                                                    #STEP 2: Assign recoveries into regions 
################################################################################################################################################################

rec_codes_lookup = read.csv("data/recovery codes-wietkamp+shelton 12-2018 two PUSO.csv", stringsAsFactors = FALSE)

rec_codes_lookup$recovery_location_code <- rec_codes_lookup$location_code
rec_codes_lookup <- rec_codes_lookup%>% 
  select(recovery_location_code, Rec.area.Sullaway.Shelton)

rec_codes_lookup$region <- rec_codes_lookup$Rec.area.Sullaway.Shelton
rec_codes_lookup <- rec_codes_lookup %>% select(-c(Rec.area.Sullaway.Shelton)) 

# pull in location data from RMIS
locations = read.csv("locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
                   recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
#fix locations file before left joining then may not experience as many duplicates 
locations <- locations %>%
  filter(!is.na(latitude))

locations <- locations %>% 
  distinct(recovery_location_code, .keep_all = TRUE)   #took out duplicate recovery codes, keep.all = keeps the first row of values for each recovery code
#i think this is ok because alot of the duplicate data were just off my a 0.001 degree in the lat and long, not huge differences 
all_rec_codes_locations <- left_join(rec_codes_lookup ,locations)

df_recovery <- left_join(dat_recovery, all_rec_codes_locations, by = "recovery_location_code")
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

setwd("~/Documents/GitHub/Chinook_Bycatch/data/ASHOP/snoutbase")
snout = read.csv("A-SHOP_Snoutbase_122118.csv", stringsAsFactors = FALSE)
snout$tag_code<- str_pad(snout$CWTCode, width= 6, pad = "0", side="left") #add leading zero that r removes

snout <- snout %>% 
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


#this script matches lat and long into to RMIS Highseas to get better recovery information so they recoveries dont snap to a grid

#use dat from rmis_base file. 
#dat_region = read.csv("dat_region.csv", stringsAsFactors = FALSE)

#keep just highseas fisheries
hs_dat <- df_recovery %>% 
  filter(fishery_type == "high_seas") %>%
  filter(rec_year > 2005) %>% #snoutbase starts in 2005
  mutate(rec_month = as.numeric(rec_month)) %>% 
  mutate(rec_day = as.numeric(rec_day)) %>% 
  mutate(rec_year= as.numeric(rec_year)) %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove = FALSE )

all_dat <- left_join(hs_dat, snout, by =  "id") #combine so that rmis lat and longs can be replaced by snout 

all_dat1 <- all_dat %>%
  filter(!is.na(Year)) %>%
mutate(Lat = as.numeric(Lat)) %>%
mutate(Long = as.numeric(Long))
#add spatial bounds to these based on lat long
all_dat1$region<-  as.vector(cut(all_dat1$Lat, breaks=c(Inf,46.63611,45.76666,44.015, 42.6725, 42, 38.958333, -Inf), 
                                 labels=c(  
                                   "NCA",
                                   "MEN",
                                   "SOR",
                                   "COR",
                                   "NOR",
                                   "COL",
                                   "WAC")))

all_dat2 <- all_dat1 %>% 
  select(id,rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level, brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
         release_loc_domain, stock_location_code, total_release, detection_method )
 
df_recovery$Haul <- NA
df_recovery$Lat <- NA
df_recovery$Long <- NA
df_recovery$Ln <- NA
df_recovery$Wt <- NA
df_recovery$Sex <- NA

dat_region_filter <- df_recovery %>%
  unite("id", c("rec_year", "rec_month", "rec_day", "tag_code"), remove =FALSE ) %>%
  filter(!fishery_type == "high_seas" | !rec_year > 2005) %>% #get rid of the data that is in hs_dat so then they can be bound later and duplicates wont be counted
    select(id, rec_year, rec_month, rec_day, tag_code, recovery_id, fishery, fishery_type, gear, region, latitude, longitude, Lat, Long, region, recovery_location_code, recovery_location_name, estimated_number, estimation_level,  brood_year, first_release_date, release_location_code, release_location_name, release_location_state, release_location_rmis_basin,
         release_loc_domain, stock_location_code, total_release,detection_method)

everything <- rbind(dat_region_filter, all_dat2, deparse.level = 1, make.row.names = TRUE)  

# conditionally replace latitude and longitude with NA if there is a value in the snoutbase lat and long
everything1 <- everything %>%
 mutate(Latitude = case_when(Lat > 0 ~ Lat,       
                             TRUE ~ latitude)) %>%  
 mutate( Longitude = case_when(Lat > 0 ~ Long, 
                               TRUE ~longitude)) %>%
  dplyr::select(-c(longitude, Long, latitude, Lat)) #now this should have all snout and rmis data with combined lat and longs

dat_everything <- everything1 

#################################################################################################################################################################
                                                                  # SAVE TIDY DATA FILES 
################################################################################################################################################################

#
write.csv(dat_everything, "rmis_parced.csv" )
#getwd()

#  #CHECK TO MAKE SURE REGION ASSIGNMENTS PLOT OK
# plot <- dat_everything %>%
#   filter(!Latitude == 0)
# 
# h <-  p_north_am +
#   geom_point(data = plot, mapping = aes(x = Longitude, y = Latitude, color = region)) +
#   scale_alpha(guide = 'none')+
#   facet_wrap(~fishery_type) +
#  
#   theme_bw() 
# h
# unique(dat_everything$fishery_type)
