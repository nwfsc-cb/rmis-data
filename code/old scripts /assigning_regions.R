setwd("~/Documents/GitHub/rmis/data")
df = read.csv("recovery codes-wietkamp+shelton 12-2018 two PUSO.csv", stringsAsFactors = FALSE)

#dat is file from rmis_base

library(dplyr)

df$recovery_location_code <- df$location_code
df <- df%>% 
  select(recovery_location_code, Rec.area.Sullaway.Shelton)

df$region <- df$Rec.area.Sullaway.Shelton
df <- df %>% select(-c(Rec.area.Sullaway.Shelton)) 
#df = version of lookup table with necessary columns

#___________________________________________________________________________________________________________________________________________________
#join DF recovery with the weitkamp updated codes 
dat_region <- left_join(dat, df)
    #this join only adds 109 rows, small amount of duplicates going to ignore

#write.csv(dat_region, "dat_region.csv") #SAVE DAT REGION FOR FURTHER USE       THIS FILE IS GOOD TO GO. NOW NEEDS SNOUTBASE CORRECTIONS 

#___________________________________________________________________________________________________________________

#FIGURING OUT WHERE WE ARE MISSING INFO ON LOCATION CODES
#  df_recovery <- left_join(dat_recovery, df, by = "recovery_location_code")
#na is a list of recovery codes that did not match up to Oles look up table
#331 recovery location codes did not match up
          na<- df_recovery %>% filter(is.na(`Rec area.Shelton3`)) #all the recoveries that do not match up with a recovery location code in look up table // will want to use this later for when add in regions
          recovery_location_code <- unique(na$recovery_location_code)
          recovery_location_code <- as.data.frame(recovery_location_code)
          recovery_location_code$id <- 1
          c<- left_join(locations, recovery_location_code) #list of unique codes that are not in look up table (includes with and without lat long)
          id<- c %>% filter(id == 1) #list of unique codes that are not in look up table (includes with and without lat long)
          
          z <- id %>% filter(is.na(latitude)) # list of recovery codes that are not in lookup tanble AND that dont have lat and long - need to look up in RMIS data base?
          lat_long <- id %>% filter(!is.na(latitude)) #list of recovery codes that are not in table but DO have lat long- will use for convex hull stuff
          lat_long <- lat_long %>%
            dplyr::select(-c(id))
          x <- left_join(dat_recovery, z) 
          v <- x %>% filter(!is.na(id)) #list of # of recoveries that dont have lat long in recovery codes
#write_csv(lat_long,"missing_rec_code_lat_long.csv")




