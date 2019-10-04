library(dplyr)
library(tidyr)
#this script joins releases and recoveries
#Creates a file that has all release/recovery and locations when applicable

#################################################################################################################################################################
################################################################################################################################################################
#create dat_focal which is release and recoveries combined
#load recoveries
setwd("~/Documents/GitHub/rmis")

recover = read.csv("data/chinook/recoveries_1973.csv", stringsAsFactors = FALSE)
recover = dplyr::select(recover, tag_code, recovery_id, recovery_date, fishery, gear, 
                        recovery_location_code, recovery_location_name, estimated_number)
for(y in 1974:2016) {
  #  names change slightly in 2015,
  temp = read.csv(paste0("data/chinook/recoveries_",y,".csv"), 
                  stringsAsFactors = FALSE)
  temp = dplyr::select(temp, tag_code, recovery_id, recovery_date, fishery, gear, 
                       recovery_location_code, recovery_location_name, estimated_number)
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
                        release_location_rmis_basin) %>% 
  dplyr::rename(tag_code = tag_code_or_release_id)

#left_join combines the two data frames into dat
dat = left_join(recover, release) 
##dat should have all releases and recoveries 1973-2016

##then filter out stocks that we are not interested in...
#load focal species data that Ole created, this file just has the stocks we are interested in
focal = read.csv("data/focal_spring_chinook_releases_summary.csv", header=T, stringsAsFactors = FALSE) 
#focal now just has one column, stock_location code
focal=dplyr::select(focal, stock_location_code)
#combine dat and focal by stock_code_location, filters out stocks we are not interested in
dat_focal=semi_join(dat, focal)

#now to sum total releases across rows using coded wire tag columns
dat_focal$total_release=rowSums(dat_focal[,c('cwt_1st_mark_count', 'cwt_2nd_mark_count', 'non_cwt_1st_mark_count', 'non_cwt_2nd_mark_count')], na.rm=TRUE)
#drop unnecessary columns after summing total releases, you only care about total releases of tagged fish
dat_focal=dplyr::select(dat_focal, -cwt_1st_mark_count, -cwt_2nd_mark_count,
                -non_cwt_1st_mark_count, -non_cwt_2nd_mark_count)
#dat_focal now has all releases and recoveries, sum of CWT as total releases

############################################################################################################################################################
############################################################################################################################################################

#take out freshwater recoveries from dat_recovery_state
    dat_recovery= dat_focal %>% 
      separate(recovery_location_code, into = c("state_code", "rest_of_rec_code"), sep = 1)
  
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
    
    #recode to have seasons
  #  dat_recovery$rec_season <- dat_recovery$rec_month %>%
   #   recode(
    #  '11'= "winter",
     # '12'="winter",
      #'1'="winter",
      #'2'="winter",
      #'3'="winter",
      #'4'="spring",
      #'5'="spring",
      #'6'="summer",
      #'7'="summer",
      #'8'="fall",
      #'9'="fall",
      #'10'="fall")
    
    #dat_recovery has state, fishery gear, and just Marine fish and domain
   # write.csv(dat_recovery, "dat_recovery.csv")
    
###############################################################################################################################################################
###############################################################################################################################################################
   
    #create dat used for spatial plotting and used for future work. 
    #joins locations.txt with dat_recovery
    
    # pull in location data from RMIS
    locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
    locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
    locations = rename(locations, recovery_location_code = location_code,
                       recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
    #fix locations file before left joining then may not experience as many duplicates 
    locations <- locations %>%
      filter(!is.na(latitude))
    
    locations <- locations %>% 
      distinct(recovery_location_code, .keep_all = TRUE)   #took out duplicate recovery codes, keep.all = keeps the first row of values for each recovery code
    #i think this is ok because alot of the duplicate data were just off my a 0.001 degree in the lat and long, not huge differences 
    
   dat_recovery <- dat_recovery %>% 
    unite("recovery_location_code", c(state_code, marine_fw, rest_of_rec_code), sep = "", remove = TRUE)
    dat = left_join(dat_recovery, locations, by="recovery_location_code")

   # write.csv(dat, "rmis_data.csv")
    
    