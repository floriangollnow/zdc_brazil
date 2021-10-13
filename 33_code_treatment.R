# prepare treatment for DID starting 2005
# municipalities are treated after first SoyM market share increase >=50% & Year >=2006


library(tidyverse)

# directories ----
out <-"/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"
out_down <- "/Users/floriangollnow/Downloads"

# read data -----
## Balanced panel of all municiplaities completely inside the Amazon, filtered to 2005 - 2016
MarketShareData_S05b<- read_rds( file.path (out, "MarketShare_Amazon_b05.rds")) %>% filter (YEAR>=2005 & YEAR<2016) 
## select balanced
MarketShareData_S05b <-MarketShareData_S05b %>% filter(balanced05==TRUE) 


## SoyM treatment
MarketShareData_S05b<- MarketShareData_S05b  %>% mutate (soyMtrader_treat_50 = case_when (YEAR>=2006 & soyMtrader_share_50== 1 ~ 1,# set treated
                                                                                          TRUE ~ NA_real_)) %>% 
  group_by(GEOCODE) %>% arrange(GEOCODE, YEAR) %>%  # fill up and down
  fill(soyMtrader_treat_50, .direction="down") %>% # fill treated for all subsequent years after first treatment
  mutate(soyMtrader_treat_50= case_when(is.na(soyMtrader_treat_50) ~ 0, # # fill untreated for all pre-treatment years with 0
                                        TRUE~ soyMtrader_treat_50))

write_csv(MarketShareData_S05b %>% arrange(GEOCODE, YEAR), file.path(out, "SoyZDCs_05_v1.csv"))
write_rds(MarketShareData_S05b %>% arrange(GEOCODE, YEAR), file.path(out, "SoyZDCs_05_v1.rds"))
## 
