# calculate how much of the species ranges (clipped to forest 2018) intersects with soy-suitable forests and how the ZDC market share is in those areas/municiplaitues

library(tidyverse)
dir_iucn <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/IUCN/all_treatened_int_forest"
dir_trase <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"

# read data
cerrado <- read_csv(file.path(dir_iucn,"Cerrado_species2.csv"))
amazon <- read_csv(file.path(dir_iucn,"Amazon_species2.csv"))
trase <- read_rds(file.path (dir_trase, "MarketShare_annual_v1.rds"))
trase <- trase %>% select (YEAR,GEOCODE, GZDCtrader_share,soyMtrader_share, soy_ha)
#read GEE output
cerrado<- cerrado %>% select (-`system:index`, -`.geo`)
amazon<- amazon %>% select (-`system:index`, -`.geo`)
# classify ZDC market share categories
trase_C <- trase %>% filter(YEAR==2018) %>% mutate(GZDCtrader_shareClass = case_when(soy_ha==0 ~ "No soy",
                                                                                     GZDCtrader_share==0| is.na(GZDCtrader_share) ~"0",
                                                                                     GZDCtrader_share>0 & GZDCtrader_share<=25~ paste0(">0","\U2264", "25"),
                                                                                     GZDCtrader_share>25 & GZDCtrader_share<=50~ paste0(">25","\U2264", "50"),
                                                                                     GZDCtrader_share>50 & GZDCtrader_share<=75~ paste0(">50","\U2264", "75"),
                                                                                     GZDCtrader_share>75 & GZDCtrader_share<=100~ paste0(">75","\U2264", "100"))) %>% 
  mutate(GZDCtrader_shareClass=factor(GZDCtrader_shareClass,
                                      levels=c("No soy","0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))))#,
trase_A <- trase %>% filter(YEAR==2018) %>% mutate(soyMtrader_shareClass = case_when(soy_ha==0 ~ "No soy",
                                                                                     soyMtrader_share==0| is.na(soyMtrader_share) ~"0",
                                                                                     soyMtrader_share>0 & soyMtrader_share<=25~ paste0(">0","\U2264", "25"),
                                                                                     soyMtrader_share>25 & soyMtrader_share<=50~ paste0(">25","\U2264", "50"),
                                                                                     soyMtrader_share>50 & soyMtrader_share<=75~ paste0(">50","\U2264", "75"),
                                                                                     soyMtrader_share>75 & soyMtrader_share<=100~ paste0(">75","\U2264", "100"))) %>% 
  mutate(soyMtrader_shareClass=factor(soyMtrader_shareClass, 
                                      levels=c("No soy","0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))))#,

# delete municipalities without forest
cerrado <- cerrado %>% filter (forest > 0)
amazon <- amazon %>% filter (forest > 0)
s_c <- cerrado %>% distinct(BINOMIAL)
s_a <- amazon %>% distinct(BINOMIAL)

species <- s_c %>% rbind(s_a) %>% pull(BINOMIAL) %>% n_distinct() # 249 species in municipalities with forest 2018

#### Cerrado #### 
# number of species in the cerrado
cerrado %>% distinct(BINOMIAL) %>% nrow()
# 170
# how many only rely on soy suitable forest (at municipality scale)
cerrado %>% filter (suit_forest == 0) %>%  distinct(BINOMIAL) %>% nrow()
# 102
# how many species rely on soy suitable forest (at biome scale)
base <- cerrado %>% distinct(BINOMIAL) %>% nrow()
scenario <- cerrado %>% filter (suit_forest > 0) %>%  distinct(BINOMIAL) %>% nrow()
base-scenario
# 3

####
# how much % and area of the summed species range is affected and how much in ZDC categories (only soy-producing municipalities)
cerrado_trase <- cerrado %>% mutate(GEOCODE = as.character (CD_GEOC)) %>% left_join(trase_C, by =c("GEOCODE"="GEOCODE"))
# how much of the summed species area range is in soy_producing regions
cerrado_trase %>% mutate(soyP = case_when(soy_ha>0 ~ TRUE,
                                          TRUE ~ FALSE)) %>% group_by(GZDCtrader_shareClass) %>% summarise(sum=sum (suit_forest, na.rm=TRUE)) %>% mutate (sumF = sum(sum),
                                                                                                                                         perc_outside = (sum/sumF)*100)
cerrado_trase %>% mutate(soyP = case_when(soy_ha>0 ~ TRUE,
                                          TRUE ~ FALSE)) %>% group_by(GZDCtrader_shareClass) %>% summarise(sum=n_distinct(GEOCODE))%>% mutate (sumM = sum(sum),
                                                                                                                              perc_outside = (sum/sumM)*100)

# by ZDC category
species_cerrado <- cerrado_trase %>% 
  group_by(BINOMIAL, GZDCtrader_shareClass) %>% 
  summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE), soy_ha = sum(soy_ha)) 
tabulate <- species_cerrado %>% group_by (GZDCtrader_shareClass) %>% summarise(sum_forest= sum(forest), sum_suit_forest=sum(suit_forest), soy_ha = sum(soy_ha)) %>% 
  mutate(forestRange  =(sum_forest/sum (sum_forest))*100,suitforestRange  =(sum_suit_forest/sum (sum_suit_forest))*100, soy_ha =(soy_ha/sum(soy_ha))*100)
tabulate 

#####
# Including all municipalities
# percent area of the summed species range in soy-suitable forests (total all forests), including all municipalities independent of soy-production
species_cerrado <- cerrado %>% group_by(BINOMIAL) %>% summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE)) %>% 
  mutate (affected_perc = (suit_forest/forest)*100)
species_cerrado %>% select (affected_perc) %>% pull() %>% mean() # 50.21
species_cerrado %>% select (affected_perc) %>% pull() %>% sd() # 26.36

# area of the summed species range in soy-suitable forests (total all forests)
species_cerrado %>% select (forest, suit_forest) %>% summarise(forest=sum(forest), suit_forest=sum(suit_forest)) %>% mutate (perc= (suit_forest/forest)*100)



#### Amazon ####
# delete those without forest
amazon <- amazon %>% filter (forest > 0)

#### amazon #### 
# number of species in the amazon
amazon %>% distinct(BINOMIAL) %>% nrow()
# 170
# how many only rely on soy suitable forest (at municipality scale)
amazon %>% filter (suit_forest == 0) %>%  distinct(BINOMIAL) %>% nrow()
# 151
# how many only rely on soy suitable forest (at biome scale)
base <- amazon %>% distinct(BINOMIAL) %>% nrow()
scenario <- amazon %>% filter (suit_forest > 0) %>%  distinct(BINOMIAL) %>% nrow()
base-scenario
# 10
####
# how much % and area of the summed species range is affected and how much in ZDC categories (only soy-producing municipalities)
amazon_trase <- amazon %>% mutate(GEOCODE = as.character (CD_GEOC)) %>% left_join(trase_A, by =c("GEOCODE"="GEOCODE"))



# how much of the summed species area range is in soy_producing regions
amazon_trase %>% mutate(soyP = case_when(soy_ha>0 ~ TRUE,
                                          TRUE ~ FALSE)) %>% group_by(soyMtrader_shareClass) %>% summarise(sum=sum (suit_forest, na.rm=TRUE)) %>% mutate (sumF = sum(sum),
                                                                                                                                                          perc_outside = (sum/sumF)*100)
amazon_trase %>% mutate(soyP = case_when(soy_ha>0 ~ TRUE,
                                          TRUE ~ FALSE)) %>% group_by(soyMtrader_shareClass) %>% summarise(sum=n_distinct(GEOCODE))%>% mutate (sumM = sum(sum),
                                                                                                                                               perc_outside = (sum/sumM)*100)
# by ZDC category
species_amazon <- amazon_trase %>% 
  group_by(BINOMIAL, soyMtrader_shareClass) %>% 
  summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE), soy_ha = sum(soy_ha)) 
tabulate <- species_amazon %>% group_by (soyMtrader_shareClass) %>% summarise(sum_forest= sum(forest), sum_suit_forest=sum(suit_forest), soy_ha = sum(soy_ha)) %>% 
  mutate(forestRange  =(sum_forest/sum (sum_forest))*100,suitforestRange  =(sum_suit_forest/sum (sum_suit_forest))*100, soy_ha =(soy_ha/sum(soy_ha))*100)
tabulate

# Including all municipalities
# percent area of the summed species range in soy-suitable forests (total all forests), including all municipalities independent of soy-production
species_amazon <- amazon %>% group_by(BINOMIAL) %>% summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE)) %>% 
  mutate (affected_perc = (suit_forest/forest)*100)
species_amazon %>% select (affected_perc) %>% pull() %>% mean() # 27.11
species_amazon %>% select (affected_perc) %>% pull() %>% sd() # 22.57
# area of the summed species range in soy-suitable forests (total all forests)
species_amazon %>% select (forest, suit_forest) %>% summarise(forest=sum(forest), suit_forest=sum(suit_forest)) %>% mutate (perc= (suit_forest/forest)*100)


