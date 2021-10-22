# calculates deforestation hotspots annually by biome

library(tidyverse)

trase_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"
trase25g_rc <- read_rds(file.path (trase_dir , "trase_25_cnpj_GROUPED_processing.rds"))


# hotspots separately for the the Amazon and Cerrado!
trase_g_TDef <- trase25g_rc %>% group_by(GEOCODE, YEAR, BIOME) %>% summarise(TDef_ha_mun = sum(TDef_ha)) %>% ungroup()
trase_g_TDef_90A <- trase_g_TDef %>%  filter (BIOME=="AMAZONIA") %>% group_by(YEAR) %>% summarise(h_q90_Amazon = quantile(TDef_ha_mun , probs= 0.90)) 
trase_g_TDef_90C <- trase_g_TDef %>%  filter (BIOME=="CERRADO") %>% group_by(YEAR) %>% summarise(h_q90_Cerrado = quantile(TDef_ha_mun , probs= 0.90)) 

trase_g_TDef<- trase_g_TDef %>% left_join(trase_g_TDef_90A, by=c( "YEAR"))%>% left_join(trase_g_TDef_90C, by=c( "YEAR"))
trase_g_TDef <- trase_g_TDef %>% mutate (TDefHotSpot_A = if_else(TDef_ha_mun > h_q90_Amazon, TRUE, FALSE),
                                         TDefHotSpot_C = if_else(TDef_ha_mun > h_q90_Cerrado, TRUE, FALSE)) 
# join def hospots
trase_g_TDef  <- trase_g_TDef %>% select(GEOCODE, YEAR, TDef_ha_mun, TDefHotSpot_A, TDefHotSpot_C)
trase25g_rc_defH <- trase25g_rc %>%  left_join(trase_g_TDef , by=c("GEOCODE", "YEAR"))

write_rds(trase25g_rc_defH, file.path (trase_dir , "trase_25_cnpj_GROUPED_processing_defH.rds"))
