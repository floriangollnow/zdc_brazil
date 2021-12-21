library(tidyverse)
dir_iucn <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/IUCN/all_treatened_int_forest"
dir_trase <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"

cerrado <- read_csv(file.path(dir_iucn,"Cerrado_species2.csv"))
amazon <- read_csv(file.path(dir_iucn,"Amazon_species2.csv"))
trase <- read_rds(file.path (dir_trase, "MarketShare_annual_v1.rds"))
trase <- trase %>% select (YEAR,GEOCODE, GZDCtrader_share,soyMtrader_share, soy_ha)

#cerrado <- cerrado %>% rename(forest = classification_2018, suit_forest= classification_2018_1)
cerrado<- cerrado %>% select (-`system:index`, -`.geo`)
#amazon <- amazon %>% rename(forest = classification_2018, suit_forest= classification_2018_1)
amazon<- amazon %>% select (-`system:index`, -`.geo`)
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

# delete those without forest
cerrado <- cerrado %>% filter (forest > 0)

#### Cerrado #### 
# number of species in the cerrado
cerrado %>% distinct(BINOMIAL) %>% nrow()
# 170
# how many only rely on soy suitable forest (at municipality scale)
cerrado %>% filter (suit_forest == 0) %>%  distinct(BINOMIAL) %>% nrow()
# 102
# how many only rely on soy suitable forest (at biome scale)
base <- cerrado %>% distinct(BINOMIAL) %>% nrow()
scenario <- cerrado %>% filter (suit_forest > 0) %>%  distinct(BINOMIAL) %>% nrow()
base-scenario
# 3
# how much % and area of the range is affected and how much in ZDC categories
cerrado_trase <- cerrado %>% mutate(GEOCODE = as.character (CD_GEOC)) %>% left_join(trase_C, by =c("GEOCODE"="GEOCODE"))

# percent
species_cerrado <- cerrado %>% group_by(BINOMIAL) %>% summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE)) %>% 
  mutate (affected_perc = (suit_forest/forest)*100)
species_cerrado %>% select (affected_perc) %>% pull() %>% mean() # 50.21
species_cerrado %>% select (affected_perc) %>% pull() %>% sd() # 26.36

# area
species_cerrado %>% select (forest, suit_forest) %>% summarise(forest=sum(forest), suit_forest=sum(suit_forest)) %>% mutate (perc= (suit_forest/forest)*100)
# by ZDC
species_cerrado <- cerrado_trase %>% 
  group_by(BINOMIAL, GZDCtrader_shareClass) %>% 
  summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE), soy_ha = sum(soy_ha)) 
tabulate <- species_cerrado %>% group_by (GZDCtrader_shareClass) %>% summarise(sum_forest= sum(forest), sum_suit_forest=sum(suit_forest), soy_ha = sum(soy_ha)) %>% 
  mutate(forestRange  =(sum_forest/sum (sum_forest))*100,suitforestRange  =(sum_suit_forest/sum (sum_suit_forest))*100, soy_ha =(soy_ha/sum(soy_ha))*100)
tabulate 
# gg_g_aptC <- ggplot()+
#   geom_bar(data=tabulate , aes (y=suitforestRange, x=GZDCtrader_shareClass),stat = "identity", position='stack')+
#   # scale_fill_brewer(palette = "Set2", labels=c( "without soy", "with soy"))+
#   labs(title = "Cerrado: Species range in soy-suitable forest", x="ZDC market share", y="% range", fill="Municipality")+
#   # geom_text(data=totalsdd, aes(x= soyMtrader_shareClass, y= (NPSUIT/1000)+2500),
#   #           label=paste0(format (round(totalsdd$NPSUIT/1000, 0), big.mark=","),"kha\n (", round(totalsdd$percentF, 0),"%)"),
#   #           size=3, show.legend = FALSE)+
#   scale_x_discrete(drop = FALSE)+
#   theme_minimal()
# gg_g_aptC

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
# how much of the range is affected and how much in ZDC categories
amazon_trase <- cerrado %>% mutate(GEOCODE = as.character (CD_GEOC)) %>% left_join(trase_A, by =c("GEOCODE"="GEOCODE"))

species_amazon <- amazon %>% group_by(BINOMIAL) %>% summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE)) %>% 
  mutate (affected_perc = (suit_forest/forest)*100)
species_amazon %>% select (affected_perc) %>% pull() %>% mean() # 27.11
species_amazon %>% select (affected_perc) %>% pull() %>% sd() # 22.57

# area
species_amazon %>% select (forest, suit_forest) %>% summarise(forest=sum(forest), suit_forest=sum(suit_forest)) %>% mutate (perc= (suit_forest/forest)*100)


species_amazon <- amazon_trase %>% 
  group_by(BINOMIAL, soyMtrader_shareClass) %>% 
  summarise(forest = sum (forest, na.rm=T), suit_forest= sum (suit_forest, na.rm=TRUE), soy_ha = sum(soy_ha)) 
tabulate <- species_amazon %>% group_by (soyMtrader_shareClass) %>% summarise(sum_forest= sum(forest), sum_suit_forest=sum(suit_forest), soy_ha = sum(soy_ha)) %>% 
  mutate(forestRange  =(sum_forest/sum (sum_forest))*100,suitforestRange  =(sum_suit_forest/sum (sum_suit_forest))*100, soy_ha =(soy_ha/sum(soy_ha))*100)
tabulate



