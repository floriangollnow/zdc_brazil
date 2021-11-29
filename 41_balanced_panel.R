## this script calculates a balanced panel identifying all municipalities that had soybeans planted from 2005 throughout 2018. 
## it reduces the data to those municipalities that are fully covered/within the Amazon biome.

library(tidyverse)
library(sf)

# set directories
out <-"/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"

# read data ----
# market share
MarketShareData <- read_rds(file.path (out, "MarketShare_annual_v1.rds"))

#select municipalities inside amazon and reduce to years >= 2005
MarketShareData_S05 <- MarketShareData %>% filter (Biome_c=="Amazônia", YEAR>=2005) #
MarketShareData_S06 <- MarketShareData %>% filter (Biome_c=="Amazônia", YEAR>=2006) # for robustness check


# balance sample ----
## and exclude those without soy (Mapbiomas and IBGE)
Soy0_id_temp05 <-   MarketShareData_S05 %>% filter ( YEAR<=2018 & soy_ha > 1 ) %>% select("GEOCODE") %>% group_by(GEOCODE) %>%  summarise(n_geocode = n()) 
soy_ibge05 <-  MarketShareData_S05 %>% filter (YEAR<=2018 & soyIBGE_ha > 0) %>%  select("GEOCODE") %>% unique() %>% mutate(keep=TRUE)#
Soy0_id_temp06 <-   MarketShareData_S06 %>% filter ( YEAR<=2018 & soy_ha > 1 ) %>% select("GEOCODE") %>% group_by(GEOCODE) %>%  summarise(n_geocode = n()) 
soy_ibge06 <-  MarketShareData_S06 %>% filter (YEAR<=2018 & soyIBGE_ha > 0) %>%  select("GEOCODE") %>% unique() %>% mutate(keep=TRUE)#
## combine
Soy0_id_temp05 <- Soy0_id_temp05 %>% left_join(soy_ibge05, by = "GEOCODE") %>% replace_na(list(keep=FALSE))
Soy0_id_temp06 <- Soy0_id_temp06 %>% left_join(soy_ibge06, by = "GEOCODE") %>% replace_na(list(keep=FALSE))

## keep only those that have the full time period (2005-2018 -> 14 observations)
Soy0_id05 <- Soy0_id_temp05 %>% dplyr::filter(n_geocode==14& keep==TRUE) %>% pull(GEOCODE) # keep only those that have the full time period (15 observations)
Soy0_id06 <- Soy0_id_temp06 %>% dplyr::filter(n_geocode==13& keep==TRUE) %>% pull(GEOCODE) # keep only those that have the full time period (12 observations)

##  
Soy0_id_b_tb05 <- as_tibble(Soy0_id05) %>% mutate (balanced05=TRUE) %>% rename(GEOCODE=value)
Soy0_id_b_tb06 <- as_tibble(Soy0_id06) %>% mutate (balanced06=TRUE) %>% rename(GEOCODE=value)

## join with main dataset
MarketShareData_S05b<- MarketShareData_S05 %>% 
  left_join(Soy0_id_b_tb05, by="GEOCODE") %>% 
  left_join(Soy0_id_b_tb06, by="GEOCODE") 

### write data, ready for model: start date 2005! -----
write_rds(MarketShareData_S05b , file.path (out, "MarketShare_Amazon_b05.rds"))



