library(dplyr)
library(ggplot2)

# Load release data
release = read.csv("data/chinook/all_releases.txt", header=T, stringsAsFactors = FALSE) 

# Pull out spring run Chinook (run == 1), summer Chinook (run == 2), and missing (run == is.na)
  release.spr <- release %>% filter(run==1 | run==2 |is.na(run)==T)

  # Cull releases with zero CWT releases.
  release.spr <- release.spr %>% 
                  mutate(cwt_1st_mark_count =if_else(is.na(cwt_1st_mark_count),0,as.numeric(cwt_1st_mark_count)),
                  cwt_2nd_mark_count =if_else(is.na(cwt_2nd_mark_count),0,as.numeric(cwt_2nd_mark_count))) %>%
                  mutate(total_cwt_rel = cwt_1st_mark_count + cwt_2nd_mark_count) %>% 
                  filter(total_cwt_rel >0 )
  
  ## Find the releases with NA run type and fix these....
  # Replace Alaska runs with run=1 (spring run) because there are no other run types in AK
  release.spr <- release.spr %>% mutate(.,run=ifelse(release_location_state=="AK",1,run)) 
  # Replace missing states with values derived from the hatchery and release locations
  release.spr <- release.spr %>% mutate(.,release_location_state=ifelse(is.na(release_location_state)==T & is.na(run)==T & substr(hatchery_location_code,1,1) == "3","WA",release_location_state))
  ## Cull wild releases because their run type is unclear in most cases.
  release.spr <- release.spr %>% filter(is.na(run)==F | (is.na(run)==T & rearing_type != "W"))
  # That leaves 49 entries that have no run type: dim(release.spr %>% filter(is.na(run)==T))
  ## Ole is ok dropping these 49 entries out of > 14,000.  Most of them are from Idaho (>40)
  release.spr <- release.spr %>% filter(is.na(run)==F)
  
  # summarize the releases by run, region, and year
    release.spr.summary <- release.spr %>% 
                            group_by(run,brood_year,release_location_state) %>% 
                            summarise(total_cwt_rel = sum(total_cwt_rel)) %>% as.data.frame()
    # Plot Total tagged release
    ggplot(release.spr.summary %>% filter(brood_year >1975, run==1)) +
        geom_line(aes(y=total_cwt_rel,x=brood_year)) +
        facet_wrap(~release_location_state)
                                   
###### Let's divide the releases into state specific chunks for easier manipulation.
    release.spr.ca <- release.spr %>% filter(release_location_state=="CA")
    release.spr.bc <- release.spr %>% filter(release_location_state=="BC")
    release.spr.ak <- release.spr %>% filter(release_location_state=="AK")
    
    # Make a Columbia area
    COL <- c("CECR","CRGN","LOCR","UPCR","SNAK")
    release.spr.col <- release.spr %>% filter(release_location_rmis_region %in% COL)
    
    # Make areas for oregon and washington
    release.spr.or  <- release.spr %>% filter(!release_location_rmis_region %in% COL) %>% filter(release_location_state=="OR")
    release.spr.wa  <- release.spr %>% filter(!release_location_rmis_region %in% COL) %>% filter(release_location_state=="WA")
############
        # Start with California
    rel.spr.ca.summ <- release.spr.ca %>% group_by(release_location_rmis_region,release_location_rmis_basin,brood_year,run) %>% 
          summarise(TOT=sum(total_cwt_rel)) %>% arrange(release_location_rmis_region,brood_year,run) %>% as.data.frame()
    
    ggplot(rel.spr.ca.summ) +
        geom_line(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
        geom_point(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
        facet_grid(release_location_rmis_region~.)
    ## This plot says a lot- Klamath-Trinity are constant and regular.  
    ## Others are sporadice and mostly absent from the early years.
    
    rel.spr.ca.summary   <- release.spr.ca %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,brood_year) %>% 
                      summarize(TOT =sum(total_cwt_rel)) %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name) %>% 
                      summarize(N.year = length(TOT), Total.rel = sum(TOT)) %>% as.data.frame()
        # So there are only three hatcheries of spring Chinook of note in California.
        ## This plot says a lot- Klamath-Trinity are constant and regular.  
        ## Others are sporadice and mostly absent from the early years.
    
    # COLUMBIA RIVER
    rel.spr.col.summ <- release.spr.col %>% group_by(release_location_rmis_region,release_location_rmis_basin,brood_year,run) %>% 
      summarise(TOT=sum(total_cwt_rel)) %>% arrange(release_location_rmis_region,brood_year,run) %>% as.data.frame()
    
    ggplot(rel.spr.col.summ) +
      geom_line(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      geom_point(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      facet_grid(release_location_rmis_region~run)
    
    rel.spr.col.summary   <- release.spr.col %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT)) %>% as.data.frame()
    
    rel.spr.col.summary2   <- release.spr.col %>% group_by(release_location_rmis_region, stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(release_location_rmis_region,stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT)) %>% arrange(run,stock_location_code) %>% as.data.frame()
    
    
    # OREGON
    rel.spr.or.summ <- release.spr.or %>% group_by(release_location_rmis_region,release_location_rmis_basin,brood_year,run) %>% 
      summarise(TOT=sum(total_cwt_rel)) %>% arrange(release_location_rmis_region,brood_year,run) %>% as.data.frame()
    
    ggplot(rel.spr.or.summ) +
      geom_line(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      geom_point(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      facet_grid(release_location_rmis_region~run)
    
    rel.spr.or.summary   <- release.spr.or %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT)) %>% as.data.frame()
    
    rel.spr.or.summary2   <- release.spr.or %>% group_by(stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT)) %>% as.data.frame()
    
    # WASHINGTON
    rel.spr.wa.summ <- release.spr.wa %>% group_by(release_location_rmis_region,release_location_rmis_basin,brood_year,run) %>% 
      summarise(TOT=sum(total_cwt_rel)) %>% arrange(release_location_rmis_region,brood_year,run) %>% as.data.frame()
    
    ggplot(rel.spr.wa.summ) +
      geom_line(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      geom_point(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      facet_grid(release_location_rmis_region~run)
    ## This plot says a lot- Klamath-Trinity are constant and regular.  
    ## Others are sporadice and mostly absent from the early years.
    
    rel.spr.wa.summary   <- release.spr.wa %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT))%>% arrange(run,stock_location_code) %>% as.data.frame()
    
    rel.spr.wa.summary2   <- release.spr.wa %>% group_by(stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT))%>% arrange(run,stock_location_code) %>% as.data.frame()

    # BRITISH COLUMBIA
    rel.spr.bc.summ <- release.spr.bc %>% group_by(release_location_rmis_region,release_location_rmis_basin,brood_year,run) %>% 
      summarise(TOT=sum(total_cwt_rel)) %>% arrange(release_location_rmis_region,brood_year,run) %>% as.data.frame()
    
    ggplot(rel.spr.bc.summ) +
      geom_line(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      geom_point(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      facet_grid(release_location_rmis_region~run)
    ## This plot says a lot- Klamath-Trinity are constant and regular.  
    ## Others are sporadice and mostly absent from the early years.
    
    rel.spr.bc.summary   <- release.spr.bc %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT))%>% arrange(run,stock_location_code) %>% as.data.frame()
    
    rel.spr.bc.summary2   <- release.spr.bc %>% group_by(stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT))%>% arrange(run,stock_location_code) %>% as.data.frame()
    
    # ALASKA
    rel.spr.ak.summ <- release.spr.ak %>% group_by(release_location_rmis_region,release_location_rmis_basin,brood_year,run) %>% 
      summarise(TOT=sum(total_cwt_rel)) %>% arrange(release_location_rmis_region,brood_year,run) %>% as.data.frame()
    
    ggplot(rel.spr.ak.summ) +
      geom_line(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      geom_point(aes(y=TOT,x=brood_year,color=release_location_rmis_basin)) +
      facet_grid(release_location_rmis_region~run)
    ## This plot says a lot- Klamath-Trinity are constant and regular.  
    ## Others are sporadice and mostly absent from the early years.
    
    rel.spr.ak.summary   <- release.spr.ak %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(hatchery_location_code,hatchery_location_name,stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT))%>% arrange(run,stock_location_code) %>% as.data.frame()
    
    rel.spr.ak.summary2   <- release.spr.ak %>% group_by(stock_location_code,stock_location_name,brood_year,run) %>% 
      summarize(TOT =sum(total_cwt_rel)) %>% group_by(stock_location_code,stock_location_name,run) %>% 
      summarize(N.year = length(TOT), Total.rel = sum(TOT))%>% arrange(run,stock_location_code) %>% as.data.frame()
    
#############################################################    
#############################################################    
#############################################################
    
    # Pick the results for stock location codes that are abundant an observed over many years.
        
        # Focus only on SPRING stocks for now (run == 1)
        # I picked stocks with > 1 million releases total or >10 years.
    
    
      # from CALIFORNIA (see rel.spr.ca.summary)
    STL.ca <- c("6FCSAFEA", # Feather R
                "6FCSABUT", # Butte Creek
                "6FCSAFEA FRFH", # Feather R
                "6FKTR") # Klamath - Trinity
      # from OREGON (see rel.spr.or.summary2)
    STL.or <- c("5F221    ROGUE", # Rogue R
                "5F222    34", # Trask R
                "5F222    47", # Nestucca
                "5F222    52", # Cole Rivers Hatch
                "5F222    55") # Umpqua R
      # from COLUMBIA (see rel.spr.col.summary2)
    STL.col <- c(
    "3F42001         S71",# Mixed Columbia
    "3F42001  260002 S",  # Cowlitz
    "3F42001  270002 S",  # Kalama 
    "3F42001  270168 S",  # Wind R
    "3F42001  290131 S02", # Little White Salmon NFH
    "3F42001  300002 S",  # Klickitat   
    "3F42001  330002 S",  # Lower Snake
    "3F42001  350009 H01",# Tucannon Captive
    "3F42001  350009 S",  # Tucannon River
    "3F42001  390002 S",  # Yakima R - Upr
    "3F42001  450474 S",  # LeavenWorth Hatchery
    "3F42001  450759 S",  # Chiwawa R
    "3F42001  460042 S",  # Entiat R
    "3F42001  480002 S",  # Methow R
    "3F42001  480374 S",  # Twisp R
    
    "4F1-17060201069-XUU", #Salmon R
    "4F1-17060210002-HUU", # Rapid R
    "4F1-17060303000-XUU", # Powell
    "4F1-17060304002-HUU", # Kooskia
    "4F1-17060305001-XUU", # S Fork Clearwater
    "4F1-17060306000-XUU", # Clearwater
    "4F1-17060306082-HUU", # Clearwater Mix
    "4F1-17060308001-HUU", #Dworshak
    "4FB-17060308001-HUU", # Dwor B
    
    "5F*335   85", # Rapid R Idaho
    "5F332    11", # Sandy R 
    "5F333    19", # Clackamas R Early.
    "5F333    21",  # SANTIAM R N FK  
    "5F333    22",  # MID WILLAMETTE R  
    "5F333    23",  # MCKENZIE HATCHERY  
    "5F333    24",  # SANTIAM R S FK
    "5F333    57",  # WILLAMETTE R   
    "5F334    102", # WARM SPRINGS R
    "5F334    200", # LOSTINE R ENDEMIC
    "5F334    50",  # HOOD R
    "5F334    66",  # DESCHUTES R
    "5F334    75",  # CARSON (WASH)
    "5F334    91",  # UMATILLA R
    "5F335    29",  # IMNAHA R AND TRIBS
    "5F335    80",  # GRANDE RONDE R UPR
    "5F335    81"   # LOOKINGGLASS CR
    )
    
    # These are summer runs of interest (same criteria)
    # c(
    #   "3F42001         S67", # Methow & Okanogan
    #   "3F42001  450030 S",  #     WENATCHEE R  45.0030
    #   "3F42001  470001 S",  #     WELLS DAM       (47)
    #   "3F42001  470001 S02",# WELLS HATCHERY
    #   "4F2-17060202002-XUU",# PAH CH-2   2     20   3701715
    #   "4F2-17060208033-XUU",# S FK SALMON   2     38  12213800
    #   "4F2-17060208044-XUU" # JOHNSON CRK
    # )  
      
     ### Washington (see rel.spr.wa.summary2 )  ###########################################  
    
    # Added a couple of close to 1 million stocks (quilcene)
    
    STL.wa <- c(  
    "3F10107  010120 S",# NOOKSACK R   01.0120   
    "3F10107  010246 S",# NOOKSACK -SF 01.0246
    "3F10107  010406 S",# KENDALL CR   01.0406   
    "3F10208  030176 S",# SKAGIT R     03.0176  
    "3F10511  100031 S",# WHITE R      10.0031   
    "3F10806  180018 S",# DUNGENESS R  18.0018   
    "3F21703  200096 S02")# SOL DUC R   20.0096   
    
    ## A couple of lower abundance possibilities.'
    # c("3F10208  031421 S", # CLARK CR     03.1421   1      5    539797
    # "3F10208  031421 S02", #    MARBLEMOUNT HATCH   1      1    486900
    # "3F10412  170012 S") # BIG QUILCENE 17.0012
    
     # Summer Runs that meet criteria
    # c(
    # "3F10208  030176 S",# SKAGIT R     03.0176   2     30   4838573
    # "3F10308  050001 S", # STILLAGUAMISH R   2     14   1505932
    # "3F10308  050135 S", # STILLAGUAMISH R -NF   2     20   3449004
    # "3F10308  070012 S02",# SKYKOMISH R  07.0012   2     19   9507426
    # "3F10308  070940 S", #  WALLACE R    07.0940
    #"3F21703  200096 S02" #  SOL DUC R   20.0096   2     15   2224023
    # )
    
    #### British Columbia. (see rel.spr.bc.summary2) ###########################################
    
    # Northern Coastal
    STL.bc <-
    c(
    "2FN  NASSS0120",#             S-Kincolith R   1     15    721668
    "2FN  NCSTS0064",#               S-Hirsch Cr   1     19   1163007
    "2FN  NCSTS0146",#               S-Kitimat R   1     16   1342486
    "2FN  NCSTS0166",#               S-Kildala R   1     12    585878
    "2FN  NCSTS1949",#            S-Kitimat R Up   1     10    502302
    "2FN  NCSTS1950",#           S-Kitimat R Low   1     15   1223614
    "2FN  SKNAS0478",#            S-Bulkley R Up   1     27   1409168
    
    # Yukon Canadian
    "2FN  YUKNS0959",#                 S-Yukon R   1     28   4633772
    "2FN  YUKNS1732",#               S-Takhini R   1     16    634360
    "2FN  YUKNS2318",#               S-Tatchun R   1     17    326749
    
    # Fraser and tributaries
    "2FS  LWFRS0152",#            S-Birkenhead R   1     17   1106796
    "2FS  TOMFS0132",#           S-Salmon R/TOMF   1     22   1508158
    
    "2FS  TOMMS0252",#             S-Bonaparte R   1     10    683768
    "2FS  TOMMS0253",#                S-Nicola R   1     35   4780733
    "2FS  TOMMS0254",#               S-Deadman R   1     13    393811
    "2FS  TOMMS1387",#             S-Coldwater R   1     13    660567
    
    "2FS  UPFRS0263",#              S-Bowron R   1      9   1174361
    "2FS  UPFRS0352",#            S-Cariboo R Up   1      9    922093
    "2FS  UPFRS1890"#                 S-Dome Cr   1     18    998639
    )
    ## BC Summer Runs.
    
    # c(
    # "2FN  CCSTS0079",#           S-Atnarko R Low   2     38   4669186
    # "2FN  CCSTS1940",#            S-Atnarko R Up   2     30   3493138
    # "2FN  QCI S0060",#                S-Yakoun R   2     15    768800
    # "2FN  RIVRS1964",#            S-Chuckwalla R   2     15    686178
    # "2FN  RIVRS2515",#              S-Kilbella R   2     10    295528
    # "2FN  SKNAS0053",#           S-Kitsumkalum R   2     21   2813637
    # "2FN  SKNAS0055",#                S-Babine R   2     18    750589
    # "2FN  SKNAS6449",#       S-Kitsum Abv Canyon   2     19   2375901
    # "2FN  SKNAS6501",#       S-Kitsum Bel Canyon   2     19   1655849
    # "2FS  GSMNS0216",#             S-Cheakamus R   2     14   1218507
    # "2FS  GSVIS0105",#             S-Puntledge R   2     46   4795599
    # "2FS  TOMFS0156",#                 S-Eagle R   2     11   2224221
    # "2FS  TOMFS0161",#           S-Shuswap R Low   2     35   7577002
    # "2FS  TOMFS1958",#        S-Shuswap R Middle   2     26   2688174
    # "2FS  UPFRS0155",#               S-Quesnel R   2     14   2761073
    # "2FS  UPFRS0158",#                S-Stuart R   2     10    730304
    # "2FS  UPFRS0272",#                S-Chilko R   2     11   1487652
    # "2MS  GSMNS2732",#              S-Porteau Cv   2     10   1609217
    # )
    
    ###### ALASKA ################################################
    
    # Alaska and particularly western alaska is stuck with very few tagged fish.  
    # Ole was more generous in including stocks here.
    
    STL.ak <- c(
      # Southeastern Alaska
    "1F1NE109 10",#       PORT ARMSTRONG   1     10    634104
    "1F1NE109 1010090999",#         LPW (UNUK R)   1     13    647297
    "1F1NE111 1710100",#        KING SALMON R   1     17    984855
    "1F1NE111 3210320",#        TAKU R 111-32   1     32    782286
    "1F1NE111 4099998",#             MACAULAY   1     11    760611
    "1F1NE112 1110110",#         HIDDEN FALLS   1     29   2200468
    "1F1NE115 3210250",#     CHILKAT R 115-32   1     23    605231
    "1F1NE115 3210250175",#             TAHINI R   1     12    568246
    "1F1NE115 3210250998",# BIG BOULDER CR115-32   1      5    216467
    "1F1NE115 34",#             BURRO CR   1     15    394502
    "1F1NW113 4110190999",#      SHELDON JACKSON   1     12    135600
    "1F1NW113 4110280",#             MEDVEJIE   1     26   3862988
    "1F1SE101 2510250",#            TAMGAS CR   1     27   1376051
    "1F1SE101 4510070",#           WHITMAN LK   1     32   5040103
    "1F1SE101 4710250",#         KETCHIKAN CR   1     30   1496842
    "1F1SE101 7110040",#   CHICKAMIN R 101-71   1     35   2554751
    "1F1SE101 7510300",#        UNUK R 101-75   1     41   5211979
    "1F1SE101 7510300999",#           CRIPPLE CR   1      4    389341
    "1F1SE101 9010100",#            NEETS BAY   1      6    594688
    "1F1SE106 4410310",#           CRYSTAL CR   1     34   6548548
    "1F1SE108 4010150",#     STIKINE R 108-40   1     21    619294
    "1F1SE108 4010150999",#     ANDREW CR 108-40   1     12    737885
    
    
    ## THESE ARE Prince William Sound and West.  (All of the releases!)
        # Prince William
    "1F2PW212 2010080",#      COPPER R 212-20   1      5    134113
    "1F2PW212 2010080257",# CHISTOCHINA R 212-20   1      1      6312
    "1F2PW212 2010080331",#            TONSINA R   1      3     58859
    "1F2PW212 2010080401",#            KLUTINA R   1      3     57640
    "1F2PW212 2010080461",#            GULKANA R   1      5     96558
    "1F2PW212 2010080571",# E FORK CHISTOCHINA R   1      2     47604
    "1F2PW223 40",#     WALLY NOERENBERG   1      4    128936
        # Cook Inlet
    "1F2UC244 2010090",#   NINILCHIK R 244-20   1     24   1642329
    "1F2UC244 2010100",#       DEEP CR 244-20   1      5     43209
    "1F2UC244 3010010",#              KENAI R   1      8    582357
    "1F2UC244 3010010076",#             KILLEY R   1      5     76016
    "1F2UC244 3010050",#            KASILOF R   1      2     70599
    "1F2UC244 3010050024",#    CROOKED CR 244-30   1     32   2827148
    "1F2UC247 4110200081",#             DESHKA R   1      4     83807
    "1F2UC247 4110200120",#     WILLOW CR 247-41   1     10    537265
    "1F2UC247 4110200250",#    MONTANA CR 247-41   1      1     21588
    "1F2UC247 4110200997",#     MOOSE CR 247-41   1      1       969
    "1F2UC247 4110200998",#  DECEPTION CR 247-41   1     21   3448964
    "1F2UC247 5010060",#              SHIP CR   1     11    527100
    "1M2LC241 1399998",#    HOMER (KASILOF R)   1      4    158527
    "1M2LC241 1399999",#   HOMER (CROOKED CR)   1      5    362760
        # Yukon
    "1F3YU334 4011000490",#             CLEAR CR   1      2     46988
    "1F3YU334 4011000998",#             SALCHA R   1      2     50168
        #Kodiak
    "1F4  004        999",# CHIGNIK+LK ROSE TEAD   1      1     22262
    "1F4CH271 1010310",#            CHIGNIK R   1      3     33468
    "1F4KD259 4110010" #          PASAGSHAK R   1      3     50949
    )

    
    STL.all <- c(
      STL.ca,
      STL.or,
      STL.col,
      STL.wa,
      STL.bc,
      STL.ak
    )
    
###################################################################    
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################    
###################################################################
###################################################################
###################################################################
    
    # Extract the releases involving the stock codes of interest
    rel.focal <- release.spr %>% filter(stock_location_code %in% STL.all, run == 1,brood_year>=1975)
    
    rel.focal.summary <- rel.focal %>% 
                          group_by(release_location_state,
                                  release_location_rmis_region,
                                  # hatchery_location_name,
                                  # hatchery_location_code,
                                  stock_location_name,
                                  stock_location_code) %>%
                          summarize(N.year=length(unique(brood_year)),first.year=min(brood_year),last.year=max(brood_year),
                                    total.release = sum(total_cwt_rel), avg.ann.release = sum(total_cwt_rel)/N.year)  %>% 
                          arrange(release_location_state,stock_location_code) %>%
                          as.data.frame()
    
    rel.focal.summary2 <- rel.focal %>% 
                            mutate(region=release_location_state,region=ifelse(release_location_rmis_region %in% COL,"COL",region)) %>%
                            group_by(region,
                                release_location_rmis_region,
                                # stock_location_name,
                                # stock_location_code,
                                brood_year) %>%
                            summarize(total.release = sum(total_cwt_rel))
    
    
    Focal.releases <- list(rel.focal, rel.focal.summary)
    
    releases.by.state.time <- ggplot(rel.focal.summary2) +
      geom_point(aes(y=total.release,x=brood_year,color=release_location_rmis_region)) +
      geom_line(aes(y=total.release,x=brood_year,color=release_location_rmis_region)) +
      facet_wrap(~region,scales = "free_y")
    
    pdf(file="data/Spr_Chinook_releases_by_state_time.pdf",width=13,height=8.5); print(releases.by.state.time); dev.off()
    save(Focal.releases,file="data/focal_spring_chinook_releases.RData")
    write.csv(rel.focal.summary,file="data/focal_spring_chinook_releases_summary.csv",row.names = F)
    
    