library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(plyr)
library(dplyr)
library(maps)
library(mapdata)
                                  #DOSNT REALLY NEED THE FIRST PART OF THIS SCRIPT ANY MORE
#THIS SCRIPT WILL CREATE A TILE PLOT SHOWING COUNTS OF HOW MANY FISH APPEAR IN EACH STATISTICAL AREA PER YEAR AND SEASON
      #IDEALLY THIS WILL HELP ESTABLISH SPATIAL BOUNDS
#use blakes file: bycatch_location_points_with_stat_areas

gis <- dat_recovery_loc
gis$ID_NO <- 1:nrow(gis) 

gis<- gis %>% 
  dplyr:: select(ID_NO, rec_season) #JOIN SO YOU CAN HAVE REC_SEASON IN THERE TOO

df<-  inner_join(bycatch_location_points_with_statistical_area_attributes, gis, by = "ID_NO")

##__________________________________________________________________________________________________
         #PLOT recovery cts X bound X Year (Facet by fishery) 

df$REC_YEAR<- as.character(df$REC_YEAR)

df<- df %>%
  group_by(REC_YEAR, FISH_TYPE, REGISTRATI, REGISTRA_3) 

df_ct <-count(df,FISH_NAME) 

#recode factor
df_ct$Region <- recode_factor(df_ct$REGISTRA_3, 
'KOD' = "KOD",
'CIFW'= "KOD",
'NGSW'="KOD",
'CISW'="KOD", #combine cook inlet with kodiak
'PWSE' ="PWS", #combine all prince william sound 
'PWSF'="PWS",
'PWSI'="PWS",
'IBS'="YAK",
'EYKT' = "YAK",
'CSEO'="YAK",
'NSEI'= "YAK",
'NSEO' = "YAK",
'BSEA' ="BER",
'AISD' = "ALEUT",
'MSAPW' = "ALEUT",
'MSAPE'="PEN",
'LCHIG'="PEN",
'SSEI'= "NSEAK",
'SSEO'="SSEAK") #change IBS to WYAK, for W of Yakatat

#Make levels from NW to SE so they plot better
df_ct <- df_ct %>% 
  mutate(Region = factor(Region, levels = c("BER",
                                                                    "ALEUT",
                                                                    "PEN", 
                                                                    "KOD",
                                                                    "PWS",
                                                                    "YAK",
                                                                    "NSEAK",
                                                             "SSEAK"
                                                                    )))
df_ct <- df_ct %>%
  filter(!FISH_TYPE == "misc") %>%
  filter(!FISH_TYPE == "aboriginal")

df_ct$Region = with(df_ct, factor(Region, levels = rev(levels(Region)))) #reverse factor levels so it plots NE to SW

df_ct$Region <- as.factor(df_ct$Region)
 ggplot(df_ct) + geom_tile(aes(x=REC_YEAR, y= Region)) +
  theme_bw() +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~FISH_TYPE)

#___________________________________________________________________________
      #PLOT recovery cts X bound X Season (Facet by fishery) 

df<- df %>%
  group_by(rec_season, FISH_TYPE, REGISTRATI, REGISTRA_3) 

df_ct <-count(df,FISH_NAME) 
df_ct <- df_ct %>% filter(!is.na(rec_season))
df_ct$Region <- recode_factor(df_ct$REGISTRA_3, 
                              'KOD' = "KOD",
                              'CIFW'= "KOD",
                              'NGSW'="KOD",
                              'CISW'="KOD", #combine cook inlet with kodiak
                              'PWSE' ="PWS", #combine all prince william sound 
                              'PWSF'="PWS",
                              'PWSI'="PWS",
                              'IBS'="YAK",
                              'EYKT' = "YAK",
                              'CSEO'="YAK",
                              'NSEI'= "YAK",
                              'NSEO' = "YAK",
                              'BSEA' ="BER",
                              'AISD' = "ALEUT",
                              'MSAPW' = "ALEUT",
                              'MSAPE'="PEN",
                              'LCHIG'="PEN",
                              'SSEI'= "NSEAK",
                              'SSEO'="SSEAK") #change IBS to WYAK, for W of Yakatat

#Make levels from NW to SE so they plot better
df_ct <- df_ct %>% mutate(Region = factor(Region, levels = c("BER",
                                                             "ALEUT",
                                                             "PEN", 
                                                             "KOD",
                                                             "PWS",
                                                             "YAK",
                                                             "NSEAK",
                                                             "SSEAK"
)))


df_ct <- df_ct %>%
  filter(!FISH_TYPE == "misc") %>%
  filter(!FISH_TYPE == "aboriginal")
df_ct$Region = with(df_ct, factor(Region, levels = rev(levels(Region)))) #reverse factor levels so it plots NE to SW
df_ct$Region <- as.factor(df_ct$Region)
ggplot(df_ct) + geom_tile(aes(x=rec_season, y= Region), color= "white") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~FISH_TYPE)

#SUMMARIZE COUNTS IN HIGHSEAS FISHERY
df_summ <- df_ct %>%
  filter(FISH_TYPE == "high_seas") %>%
  group_by(FISH_TYPE, rec_season, Region) %>%
  summarise(ct=sum(n))

##__________________________________________________________________________________________________
            #PLOT Bounds and labels on Map 
#had to take the world map and parce it down by filtering out lats and longs I didnt want since subregions were not in data set
    #CALL BASE MAP
setwd("~/Documents/GitHub/rmis/scripts_gs")
source("base_map_script.R") #create base map for this script to continue to run

#load dataset called spatial_bounds_gs.xlsx in chinook_bycatch/maps
spatial_bounds_gs = read.csv("spatial_bounds_gs.csv", stringsAsFactors = FALSE)

df<- spatial_bounds_gs
#df<- df %>%
#  filter(state == 'ak')

p<- p_north_am +
  geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
                                            y = as.numeric(line.start.lat), 
                                            xend = as.numeric(line.end.lon), 
                                            yend = as.numeric(line.end.lat))) +
  geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  geom_text(data=df, colour="darkgreen",aes(x=label_long2, y= label_lat2, label= region), size=3)
p

pdf("regions_label_all.pdf", width=13, height=8.5); print(p); dev.off()

##__________________________________________________________________________________________________
#ALASKA ONLY

world <- map_data("world")
north_america <- subset(world, region %in% c("USA", "Canada"))
ak <- subset(north_america, subregion %in% c("Alaska"))

#filtered out some points in +longitude because map shape gets really small- likely not many recoveries out on W Aleutians
ak <- ak %>%
  filter(!long > 0)
 
#base map
a <- ggplot(data = ak) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3)+
  theme_bw()
a

#load dataset called spatial_bounds_gs.xlsx in chinook_bycatch/maps
spatial_bounds_gs = read.csv("spatial_bounds_gs.csv", stringsAsFactors = FALSE)

  df<- spatial_bounds_gs %>% filter(state == "ak") 

ak_base<- a +
  geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
                                               y = as.numeric(line.start.lat), 
                                               xend = as.numeric(line.end.lon), 
                                               yend = as.numeric(line.end.lat))) +
  #geom_text(data=df, colour="darkgreen",aes(x=label_long1, y= label_lat1, label= region), size=2)+ #smaller labels for CA, OR, and WA so they fit
  geom_text(data=df, colour="darkgreen",aes(x=label_long2, y= label_lat2, label= region), size=3)
ak_base

pdf("ak_regions_label.pdf", width=13, height=8.5); print(ak_base); dev.off()

#________________________________________________________________________________________________________________
#JUST AK X SEASON

ak_nolab<- a +
  geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
                                               y = as.numeric(line.start.lat), 
                                               xend = as.numeric(line.end.lon), 
                                               yend = as.numeric(line.end.lat))) 
ak_nolab

df <- dat_recovery_loc %>%
  #filter(fishery_type == 'high_seas')%>%
  #filter(recovery_state =="AK") %>%
  filter(!is.na(rec_season)) %>%
  filter(!longitude > -130) %>%
  filter(!latitude < 51) 

s <-  ak_nolab +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, color= fishery_type, alpha= 0.5))  +
  scale_alpha(guide = 'none')+
  #colScale+
  facet_wrap(~rec_season)+
  #ggtitle("High Seas Recovery X Time") +
  theme_bw() 
s

pdf("AK_Areas_all1.pdf", width=13, height=8.5); print(s); dev.off()

##################

 na <- ak_loc %>%  filter(is.na(region)) %>%
  filter(fishery_type == "high_seas")



#WHOLE WEST COAST PLOT ON MAP TO SEE IF REGIONS GOT ASSIGNED CORRECTLY
spatial_bounds_gs = read.csv("spatial_bounds_gs.csv", stringsAsFactors = FALSE)

df<- spatial_bounds_gs
p_nolab<- p_north_am +
  geom_segment(data = df, colour="orange", aes(x = as.numeric(line.start.lon), 
                                               y = as.numeric(line.start.lat), 
                                               xend = as.numeric(line.end.lon), 
                                               yend = as.numeric(line.end.lat))) 
  
p_nolab

dat_region_loc <- left_join(dat_recovery_loc, df, by = "recovery_location_code") #have to add region to dat loc so that you have lat longs just for checking
dat_region_loc$region <- dat_region_loc$Rec.area.Sullaway.Shelton
dat_region_loc <- dat_region_loc %>%
  select(-c(Rec.area.Sullaway.Shelton)) %>%
  filter(!is.na(rec_season))

w <-  p_nolab +
  geom_point(data = dat_region_loc, mapping = aes(x = longitude, y = latitude, color= region, alpha= 0.5))  +
  scale_alpha(guide = 'none')+
  #colScale+
  facet_wrap(~rec_season)+
  #ggtitle("High Seas Recovery X Time") +
  theme_bw() 
w



