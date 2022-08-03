#map municipalities first ZDC treatment year - Figure S6-S9-S11

library(sf)
library(tidyverse)
library(rmapshaper)
library(RColorBrewer)

# directories
shape <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/Admin/IBGE/2015/br_municipios"
dir_plot <- "/Users/floriangollnow/Dropbox/ZDC_project/Draft/plot"
out_file <-"/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"


#read data
muni <- read_rds(file.path(shape, "BRMUE250GC_all_biomes_WGS84.rds"))
muni_s <- muni %>% select(-c(Area_I, Area_ha)) %>% ms_simplify()
MS_data <- read_rds( file.path(out_file, "SoyZDCs_05_v1.rds"))
muni_sd <- muni_s %>% left_join(MS_data %>% filter(YEAR==2005), by=c("CD_GEOCMU"="GEOCODE", "Biome_c", "Biome_lArea"))

muni_sd   <- muni_sd  %>% mutate (first_treat50 = factor(first_treat50 , levels=c(as.character(2006:2015), 0)))

#selected treatment for DiD
ggplot () +geom_sf(data= muni_sd %>% filter(Biome_c=="Amazônia"), aes(fill=first_treat50), size=0.2)+
  scale_fill_brewer(palette  = "Paired", name= "Year of SoyM\ntreatment",na.translate=FALSE, drop=F)+
  ggtitle("Selected soybean producing muncipalities 2005-2015")+
  theme_bw()
ggsave (filename = file.path(dir_plot, "SoyM_munis_2004.png"), width = 7 ,height =  5)

## define treatment for all 
MarketShareData <- read_rds(file.path (out_file, "MarketShare_annual_v1.rds")) #"MarketShare_annual_rates_allNew.rds"))
MarketShareData.t  <- MarketShareData %>% filter (YEAR>=2006, YEAR<=2015) 
## -> calc first treatments
MarketShareData.time <- MarketShareData.t   %>% mutate (soyMtrader_treat_50 = case_when (soyMtrader_share_50== 1 ~ 1,
                                                                                         TRUE ~ NA_real_),
                                                        GZDC_treat_50 = case_when (GZDCtrader_share >= 50 ~ 1,
                                                                                   TRUE ~ NA_real_)) %>% 
  group_by(GEOCODE) %>% arrange(GEOCODE, YEAR) %>%  # fill up and down
  fill(soyMtrader_treat_50, .direction="down") %>% 
  fill(GZDC_treat_50, .direction="down") %>% 
  mutate(soyMtrader_treat_50= case_when(is.na(soyMtrader_treat_50) ~ 0,
                                        TRUE~ soyMtrader_treat_50),
         GZDC_treat_50= case_when(is.na(GZDC_treat_50) ~ 0,
                                  TRUE~ GZDC_treat_50))
first50_allSoyM <- MarketShareData.time %>% filter (soyMtrader_treat_50==1)%>% group_by(GEOCODE) %>% summarise(first_treat50SoyM = min(YEAR))
first50_allGZDC <- MarketShareData.time %>% filter (GZDC_treat_50==1)%>% group_by(GEOCODE) %>% summarise(first_treat50GZDC = min(YEAR))
MarketShareData.time <- MarketShareData.time %>% left_join(first50_allSoyM) %>% left_join(first50_allGZDC)
MarketShareData.time  <- MarketShareData.time %>% replace_na(list(first_treat50SoyM=0, first_treat50GZDC=0))

# Amazon
MarketShareData_A <-  MarketShareData.time %>% filter (Biome_lArea=="Amazônia", YEAR>=2006, YEAR<=2015) 
muni_amazon <- muni_s %>% inner_join(MarketShareData_A %>% filter(YEAR==2006), by=c("CD_GEOCMU"="GEOCODE"))
muni_amazon  <- muni_amazon %>% mutate (first_treat50SoyM = factor(first_treat50SoyM , levels=c(as.character(2006:2015), 0)))

tya <- ggplot () +geom_sf(data= muni_amazon , aes(fill=first_treat50SoyM), size=0.2)+
  scale_fill_brewer(palette  = "Paired", name= "Year of SoyM\ntreatment",na.translate=FALSE, drop=F)+
  ggtitle("SoyM treatment muncipalities 2006-2015")+
  theme_bw()
ggsave (filename = file.path(dir_plot, "Amazon_munis.png"), width = 7 ,height =  5)

muni_amazon<-  muni_s %>% inner_join(MarketShareData_A %>% filter(YEAR<=2006) %>% group_by (GEOCODE) %>%  summarize(first_treat50GZDC= first(first_treat50GZDC),
                                                                                                                    def4soyMT5_ha=sum(def4soyMT5_ha)), 
                                     by=c("CD_GEOCMU"="GEOCODE"))

def_a <- ggplot () +geom_sf(data= muni_amazon , aes(fill=(def4soyMT5_ha)), size=0.2)+
  scale_fill_viridis_c(trans = "log", breaks=c(1,10,100,1000),"Soy-deforestation\nin ha")+
  #scale_fill_brewer(palette  = "Set3", name= "Year of SoyM\ntreatment",na.translate=FALSE)+
  ggtitle("Soy-deforestation 2006-2015")+
  theme_bw()
ggsave (filename = file.path(dir_plot, "Amazon_def.png"), width = 7 ,height =  5)

# map municiplaities cerrado treatment year Cerrado
MarketShareData_C <-  MarketShareData.time %>% filter (Biome_lArea=="Cerrado", YEAR>=2006, YEAR<=2015) 
muni_cerrado <- muni_s %>% inner_join(MarketShareData_C %>% filter(YEAR==2015), by=c("CD_GEOCMU"="GEOCODE"))
muni_cerrado  <- muni_cerrado %>% mutate (first_treat50GZDC = factor(first_treat50GZDC , levels=c(as.character(2006:2015), 0)))
trc <- ggplot () +geom_sf(data= muni_cerrado , aes(fill=first_treat50GZDC), size=0.2)+
  scale_fill_brewer(palette  = "Paired", name= "Year of ZDC\ntreatment",na.translate=FALSE,drop=F)+
  ggtitle("ZDC treatment 'scenario' muncipalities 2006-2015")+
  theme_bw()
ggsave (filename = file.path(dir_plot, "Cerrado_munis.png"), width = 7 ,height =  5)

muni_cerrado <-  muni_s %>% inner_join(MarketShareData_C %>% filter(YEAR<=2006) %>% group_by (GEOCODE) %>%  summarize(first_treat50GZDC= first(first_treat50GZDC),
                                                                                                                      def4soyMT5_ha=sum(def4soyMT5_ha)), 
                                       by=c("CD_GEOCMU"="GEOCODE"))
def_c <- ggplot () +geom_sf(data= muni_cerrado , aes(fill=(def4soyMT5_ha)), size=0.2)+
  scale_fill_viridis_c(trans = "log", breaks=c(1,10,100,1000), "Soy-deforestation\nin ha")+
  #scale_fill_brewer(palette  = "Set3", name= "Year of SoyM\ntreatment",na.translate=FALSE)+
  ggtitle("Soy-deforestation 2006-2015")+
  theme_bw()
ggsave (filename = file.path(dir_plot, "Cerrado_def.png"), width = 7 ,height =  5)
