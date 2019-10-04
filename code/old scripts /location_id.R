#how much data/what kinds of data is lost between dat recovery and dat loc

# loop to get dat recovery dfs then dat recoery loc dfs then subtract both dfs

library(ggplot2)
library(dplyr)
dat_recovery = read.csv("dat_recovery.csv", stringsAsFactors = FALSE)
dat_recovery_loc = read.csv("dat_recovery_loc.csv", stringsAsFactors = FALSE)

# loop to get dfs from dat_recovery
  #create list of fishery types to create list of data frames
col.filters <- unique(dat_recovery$fishery_type) 

lapply(seq_along(col.filters), function(x) {
  filter(dat_recovery, fishery_type == col.filters[x])
}
) -> df_list

names(df_list) <- col.filters

list2env(df_list, .GlobalEnv)

#set up loop
  #tell R what each DF is
df1=list()
df2=list()
df3=list()
df4=list()

  #create list of years so each DF is same length
year_list <- as.data.frame(unique(dat_recovery$rec_year))
names(year_list)[names(year_list) == "unique(dat_recovery$rec_year)"] <- "rec_year"

#for loop to get recovery counts for each fishery per year
for(i in 1: length(df_list)) 
{
  df = as.data.frame(df_list[[i]])
  df1[[i]] <- df %>%  
    group_by(rec_year, fishery_name) 
   df2[[i]] <-count(df1[[i]], fishery_name) 
   df3[[i]] <- df2[[i]] %>%
     spread(fishery_name, n)
    df3[[i]][is.na(df3[[i]])] <- 0
    df4[[i]]<-left_join(year_list, df3[[i]])
    df4[[i]][is.na(df4[[i]])] <- 0
}

#take lists into a single data frame
df_rec <- do.call("cbind", df4)
df_rec[is.na(df_rec)] <- 0
#df shows how many zeros there are in each category  
  #remove duplicate columns in df_rec
df_rec = subset(df_rec, select = -c(8, 16, 24, 26, 28, 30, 40))

#_______________________________________________________________________
          # loop to get dfs from dat_recovery_loc
col.filters <- unique(dat_recovery_loc$fishery_type) 

lapply(seq_along(col.filters), function(x) {
  filter(dat_recovery_loc, fishery_type == col.filters[x])
}
) -> df_list

names(df_list) <- col.filters

list2env(df_list, .GlobalEnv)

df1loc=list()
df2loc=list()
df3loc=list()
df4loc=list()

for(i in 1: length(df_list)) 
{
  df = as.data.frame(df_list[[i]])
  df1loc[[i]] <- df %>%  
    group_by(rec_year, fishery_name) 
  df2loc[[i]] <-count(df1loc[[i]], fishery_name) 
  df3loc[[i]] <- df2loc[[i]] %>%
    spread(fishery_name, n)
  df3loc[[i]][is.na(df3loc[[i]])] <- 0
  df4loc[[i]]<-left_join(year_list, df3loc[[i]])
  df4loc[[i]][is.na(df4loc[[i]])] <- 0
}

#take lists into a single data frame
  df_rec_loc <- do.call("cbind", df4loc)
  df_rec_loc[is.na(df_rec_loc)] <- 0
#df shows how many zeros there are in each category  
#remove duplicate columns in df_rec
  df_rec_loc = subset(df_rec_loc, select = -c(8, 16, 24, 26, 28, 30, 40))

  
#_______________________________________________________________________
  # subtract df_rec_loc and df_rec from each other

df_diff= df_rec - df_rec_loc


# pull in location data from RMIS
locations = read.csv("data/locations.txt", stringsAsFactors = FALSE)
locations = locations[,c("location_code","rmis_latitude","rmis_longitude", "description")]
locations = rename(locations, recovery_location_code = location_code,
                   recovery_description = description, latitude=rmis_latitude, longitude = rmis_longitude)
locations_fltr <- locations %>%
  filter(!is.na(latitude) | !is.na(longitude))

#this pulls out the unique recovery codes (no lat and longs associated, includes the ones that are duplicate)
u<- as.data.frame(unique(locations_fltr$recovery_location_code))

#this has all of the redundant recovery location codes and associated lat/longs
d<- locations_fltr %>% 
  group_by(recovery_location_code) %>%
  filter( n() > 1 )

#this filters redundant codes to show which recovery codes are the unique ones with out the duplicates and lat longs
e<- as.data.frame(unique(d$recovery_location_code))

#plot d onto map to see how close the points are to each other and if it matters

dat_recovery_loc = read.csv("dat_recovery_loc.csv", stringsAsFactors = FALSE)

#had to take the world map and parce it down by filtering out lats and longs I didnt want since subregions were not in data set
world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))

north_am_filter <- north_america %>%
  filter(!group== 1511, !group== 1518, !group==1515, !group==1508, !group==1502, !group==1509) %>%
  filter(!long > -115) %>%
  filter(!lat < 34) %>%
  filter(!lat > 62) %>%
  filter(!long < -173) 

#base map
p_north_am <- ggplot(data = north_am_filter) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3)
p_north_am

#NEED TO FIGURE OUT A BETTER WAY TO PLOT AND SEE IF CODES ARE CLOSE TO EACH OTHER OR NOT....RIGH TNOW TRIES TO PLOT 2000.


#plot<- p_north_am +  
#  geom_point(data = d, mapping = aes(x = longitude, y = latitude, color = recovery_location_code, alpha=0.05)) +
  #scale_alpha(guide = 'none')+
#       theme_bw() 
#plot







