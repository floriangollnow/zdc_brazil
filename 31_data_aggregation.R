# calculate ZDC market share and add deforestation and biome intersection (see script 4.2) to dataset for all years between 2002-2017
# based on 01_data_preparation and 4.2 biome

library(tidyverse)
library(sf)

# Directories
## trase data from  01_data_preparation.R
out <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"
## municipalities-biome shape
admin.dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/Admin/IBGE/2015/br_municipios"
## mapbiomas
dir.MapBiomas_v5 <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/GEE/MapBiomas_v5"
#IBGE
dir.SoyIBGE <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/IBGE_SoyAreaYieldMunicipio/area"

# Read data ----
## trase datase 
trase_25_f <- read_rds(file.path (out, "trase_25_cnpj_v01.rds"))
## municipalities, with biomes and states
ibgeMun <- read_rds(file.path(admin.dir, "BRMUE250GC_all_biomes_WGS84.rds"))
ibgeMun <- ibgeMun %>% rename (StateIBGE =State)
## soy area
soy.area <- read_rds(file.path(dir.SoyIBGE, "PlantedSoyAreaSidraR.rds" )) %>% mutate(Ano=as.double(Ano)) %>% arrange(`Município (Código)`, 'Ano')

##reduce trase to known Biome & municipalities
trase_25_u <- trase_25_f %>% filter(!str_detect (MUNICIPALITY, "UNKNOWN"))


# Prepare data for all municipalities and years ----
## TRASE only report on  municipalities that export soy at some point in time. Here I need all municipalities and all potential years 
full_lArea <- expand_grid (GEOCODE = unique (ibgeMun %>% as_tibble () %>% pull(CD_GEOCMU)), YEAR = 2000:2019) # build tibble with all GEOCODES and YEARS
full <- full_lArea  %>%  left_join(ibgeMun %>% as_tibble () %>% dplyr::select(CD_GEOCMU,Biome_c,StateIBGE, Biome_lArea ), by =c("GEOCODE"="CD_GEOCMU")) ## add BIOME (Biome_c: indicating Biomes Names and Biome combinations,Biome_lArea indicating Biomes Name of largest Biome intersection), STATE 

####################
# Municipality area ----
###################
#add Mun area in Ha
ibgeMun <- ibgeMun %>% mutate (Area_Ha = as.double(Area_ha)) %>% rename(area_ha = Area_Ha)
ibgeMun_area <- as_tibble(ibgeMun) %>% dplyr::select ("CD_GEOCMU", "area_ha")

####################
# Calculate SoyM market shares ----
###################
# first all Exports and second SoyM exports
## summing all municipal production per year and Geocode
MunSoy <- trase_25_u  %>% group_by(BIOME, STATE, GEOCODE, MUNICIPALITY, YEAR) %>% summarise(SOY_EQUIVALENT_TONNES  = sum (SOY_EQUIVALENT_TONNES ))%>% ungroup()
## summing all exports production per year and Geocode                                                                                            
MunSoyE <- trase_25_u%>% filter (EXPORTER!="DOMESTIC CONSUMPTION") %>% group_by(BIOME, STATE, GEOCODE, MUNICIPALITY, YEAR) %>% summarise(SOY_EQUIVALENT_TONNES_E  = sum (SOY_EQUIVALENT_TONNES ))%>% ungroup()

## summing all SoyM (export) production per year and Geocode    
MunSOYMSoy <- trase_25_u  %>% filter (SoyM==TRUE & YEAR>=2006) %>% group_by(BIOME, STATE, GEOCODE, MUNICIPALITY, YEAR) %>% 
  summarise(SOY_EQUIVALENT_TONNES_SM = sum (SOY_EQUIVALENT_TONNES, na.rm=TRUE)) %>%
  ungroup()
## summing all SoyM traders (export) production per year and Geocode    
MunSOYM_T_Soy <- trase_25_u  %>% filter (SoyM==TRUE) %>% group_by(BIOME, STATE, GEOCODE, MUNICIPALITY, YEAR) %>% 
  summarise(SOY_EQUIVALENT_TONNES_SM_TRADER = sum (SOY_EQUIVALENT_TONNES, na.rm=TRUE)) %>%
  ungroup()
## summing all Domestic production per year and Geocode 
MunSOYM_Domestic_Soy <- trase_25_u  %>% filter (EXPORTER=="DOMESTIC CONSUMPTION") %>% group_by(BIOME, STATE, GEOCODE, MUNICIPALITY, YEAR) %>% 
  summarise(SOY_EQUIVALENT_TONNES_DOMESTIC = sum (SOY_EQUIVALENT_TONNES, na.rm=TRUE)) %>%
  ungroup()
## summing all ZDC traders (export) production per year and Geocode 
MunGZDC_T_Soy <-trase_25_u  %>% filter (G_ZDC==TRUE) %>% group_by(BIOME, STATE, GEOCODE, MUNICIPALITY, YEAR) %>% 
  summarise(SOY_EQUIVALENT_TONNES_GZDC_TRADER = sum (SOY_EQUIVALENT_TONNES, na.rm=TRUE)) %>%
  ungroup()
## summing all ZDC (export) production per year and Geocode 
MunGZDC_Soy <-trase_25_u  %>% filter (G_ZDC_Y==TRUE) %>% group_by(BIOME, STATE, GEOCODE, MUNICIPALITY, YEAR) %>% 
  summarise(SOY_EQUIVALENT_TONNES_GZDC = sum (SOY_EQUIVALENT_TONNES, na.rm=TRUE)) %>%
  ungroup()
## combine above data sets and calculate Market shares 
MunSoyM_Share <- MunSoy %>% 
  left_join(MunSoyE, by = c("BIOME","STATE", "MUNICIPALITY", "GEOCODE" ,"YEAR")) %>% 
  left_join(MunSOYMSoy, by = c("BIOME","STATE", "MUNICIPALITY", "GEOCODE" ,"YEAR")) %>% 
  left_join(MunSOYM_T_Soy, by = c("BIOME","STATE", "MUNICIPALITY", "GEOCODE" ,"YEAR")) %>% 
  left_join(MunSOYM_Domestic_Soy, by = c("BIOME","STATE", "MUNICIPALITY", "GEOCODE" ,"YEAR")) %>% 
  left_join(MunGZDC_T_Soy, by = c("BIOME","STATE", "MUNICIPALITY", "GEOCODE" ,"YEAR")) %>% 
  left_join(MunGZDC_Soy, by = c("BIOME","STATE", "MUNICIPALITY", "GEOCODE" ,"YEAR")) %>% 
  replace_na(list(SOY_EQUIVALENT_TONNES_E=0, SOY_EQUIVALENT_TONNES_SM=0, SOY_EQUIVALENT_TONNES_SM_TRADER=0, SOY_EQUIVALENT_TONNES_DOMESTIC=0 ))%>% 
  mutate (soyM_share=(SOY_EQUIVALENT_TONNES_SM/SOY_EQUIVALENT_TONNES_E)*100,
          soyMtrader_share=(SOY_EQUIVALENT_TONNES_SM_TRADER/SOY_EQUIVALENT_TONNES_E)*100,
          soyDOMESTIC_share=(SOY_EQUIVALENT_TONNES_DOMESTIC/SOY_EQUIVALENT_TONNES)*100,
          GZDCtrader_share=(SOY_EQUIVALENT_TONNES_GZDC_TRADER/SOY_EQUIVALENT_TONNES_E)*100,
          GZDC_share=(SOY_EQUIVALENT_TONNES_GZDC/SOY_EQUIVALENT_TONNES_E)*100)


## print aggregated ZDC market share 2018 (BIOME as defined by Trase)
MunGZDC_T_Soy %>% filter (BIOME=="CERRADO" & YEAR==2018) %>% summarise(sum (SOY_EQUIVALENT_TONNES_GZDC_TRADER))/MunSoyE %>% filter (BIOME=="CERRADO" & YEAR==2018) %>% summarise(sum (SOY_EQUIVALENT_TONNES_E))                    
MunSOYM_T_Soy %>% filter (BIOME=="AMAZONIA" & YEAR==2018) %>% summarise(sum (SOY_EQUIVALENT_TONNES_SM_TRADER))/MunSoyE %>% filter (BIOME=="AMAZONIA" & YEAR==2018) %>% summarise(sum (SOY_EQUIVALENT_TONNES_E))                    

## combine with all municipality dataset ('full')
MunSOYMSoy_full <- full %>%  left_join(MunSoyM_Share, by= c("GEOCODE", "YEAR")) %>% left_join(ibgeMun_area, by= c("GEOCODE" = "CD_GEOCMU"))

MunSOYMSoy_full <- MunSOYMSoy_full %>%  replace_na(list(SOY_EQUIVALENT_TONNES_E=0, SOY_EQUIVALENT_TONNES_SM=0, SOY_EQUIVALENT_TONNES_SM_TRADER=0, SOY_EQUIVALENT_TONNES_DOMESTIC=0 ))

#################
# Add soy area ibge ----
#################
MunSOYMSoy_full_a <- MunSOYMSoy_full %>% left_join(soy.area %>% 
                                                     dplyr::select (-c(Município , `Unidade de Medida`)), by=c("GEOCODE"= "Município (Código)", "YEAR"="Ano")) %>% 
  rename (soyIBGE_ha = SoyArea_ha) %>% replace_na(list(soyIBGE_ha=0))
MunSOYMSoy_full_a <- MunSOYMSoy_full_a %>%  mutate (soyIBGE_perc = (soyIBGE_ha/area_ha)*100)

###################
# Add MapBiomas Def4Soy -----
##################
defT4soy.m5 <- read_rds (file.path(dir.MapBiomas_v5, "defSoy_MapB2000_Trans_5_region.rds"))
defT4soy.m5L <- defT4soy.m5 %>% pivot_longer(-CD_GEOCMU, names_to="YEAR", values_to= "area_m")
defT4soy.m5L <- defT4soy.m5L %>% mutate(YEAR= as.numeric (str_sub(YEAR,start = -4)),
                                        def4soyMT5_ha = area_m/10000) %>%  dplyr::select (-area_m)

MunSOYMSoy_full_b <- MunSOYMSoy_full_a %>% left_join(defT4soy.m5L, by = c("GEOCODE"="CD_GEOCMU", "YEAR")) 

####################
# Add soy data from mapbiomas ----
####################
Soy <- read_rds(file.path(dir.MapBiomas_v5, "Soy_region.rds" ))
Soy.l <- Soy %>%  pivot_longer( -CD_GEOCMU, names_to="YEAR", values_to= "area_m")
Soy.l <- Soy.l %>% mutate(YEAR= as.numeric (str_sub(YEAR,start = -4)),
                          soy_ha = area_m/10000) %>%  dplyr::select (-area_m)

MunSOYMSoy_full_c <- MunSOYMSoy_full_b %>% left_join(Soy.l, by= c("GEOCODE" = "CD_GEOCMU", "YEAR"))


###############
# Edit SoyM/ZDC market share ------
###############
## replace_na of soyM share if soy production occurred in municipality based on IBGE or Mapbiomas
FirstYEAR <-  MunSOYMSoy_full_c %>% filter (soyIBGE_ha > 0 | soy_ha>0) %>% group_by(GEOCODE)  %>% summarize (FirstYEAR = min(YEAR)) # only keep those that have soy planted
MunSOYMSoy_full_c <- MunSOYMSoy_full_c %>% left_join(FirstYEAR, by="GEOCODE")

MunSOYMSoy_full_d <- MunSOYMSoy_full_c %>% mutate (soyM_share= if_else(is.na(soyM_share) & YEAR>=(FirstYEAR), 0, soyM_share),
                                                   soyMtrader_share= if_else(is.na(soyMtrader_share) & YEAR>=(FirstYEAR), 0, soyMtrader_share),
                                                   GZDC_share = if_else(is.na(GZDC_share) & YEAR>=(FirstYEAR), 0, GZDC_share),
                                                   GZDCtrader_share = if_else(is.na(GZDCtrader_share) & YEAR>=(FirstYEAR), 0, GZDCtrader_share))


# SOYMSHARE: 50% thresholds
MunSOYMSoy_full_e <- MunSOYMSoy_full_d %>% mutate(soyMtrader_share_50 = if_else(soyMtrader_share>=50, 1, 0))

# write data ----
#######################################################################################################################
write_rds(MunSOYMSoy_full_e, file.path (out, "MarketShare_annual_v1.rds"))
write_csv(MunSOYMSoy_full_e, file.path (out, "MarketShare_annual_v1.csv"))


