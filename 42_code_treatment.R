# prepares treatment variable for DID starting in 2005
# 1) Main: municipalities are treated after first SoyM market share increase >=50% & Year >=2006


library(tidyverse)

# directories ----
out <-"/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"
out_down <- "/Users/floriangollnow/Downloads"

# read data -----
## Balanced panel of all municiplaities completely inside the Amazon, filtered to 2005 - 2016
MarketShareData_S05b<- read_rds( file.path (out, "MarketShare_Amazon_b05.rds")) %>% filter (YEAR>=2005 & YEAR<2016) 
## select balanced
MarketShareData_S05b <-MarketShareData_S05b %>% filter(balanced05==TRUE) 
MarketShareData_S06b <-MarketShareData_S05b %>% filter(balanced06==TRUE) 


# SoyM treatment balanced 05 -----
MarketShareData_S05b<- MarketShareData_S05b  %>% mutate (soyMtrader_treat_50 = case_when (YEAR>=2006 & soyMtrader_share_50== 1 ~ 1,# set treated
                                                                                          TRUE ~ NA_real_),
                                                         soyMtrader_treat_25 = case_when (YEAR>=2006 & soyMtrader_share_25 == 1 ~ 1,
                                                                                          TRUE ~ NA_real_),
                                                         soyMtrader_treat_75 = case_when (YEAR>=2006 & soyMtrader_share_75 == 1 ~ 1,
                                                                                          TRUE ~ NA_real_)) %>% 
  group_by(GEOCODE) %>% arrange(GEOCODE, YEAR) %>%  # fill up and down
  fill(soyMtrader_treat_50, .direction="down") %>% # fill treated for all subsequent years after first treatment
  fill(soyMtrader_treat_25, .direction="down") %>% 
  fill(soyMtrader_treat_75, .direction="down") %>%
  mutate(soyMtrader_treat_50= case_when(is.na(soyMtrader_treat_50) ~ 0, # # fill untreated for all pre-treatment years with 0
                                        TRUE~ soyMtrader_treat_50),
         soyMtrader_treat_25= case_when(is.na(soyMtrader_treat_25) ~ 0, # # fill untreated for all pre-treatment years with 0
                                        TRUE~ soyMtrader_treat_25),
         soyMtrader_treat_75= case_when(is.na(soyMtrader_treat_75) ~ 0, # # fill untreated for all pre-treatment years with 0
                                        TRUE~ soyMtrader_treat_75))

first50 <- MarketShareData_S05b %>% filter (soyMtrader_treat_50==1)%>% group_by(GEOCODE) %>% summarise(first_treat50 = min(YEAR))
MarketShareData_S05b <- MarketShareData_S05b  %>% left_join(first50) 
MarketShareData_S05b <- MarketShareData_S05b %>% replace_na(list(first_treat50=0))


# SoyM treatment balanced 06 ------
MarketShareData_S06b<- MarketShareData_S06b  %>% mutate (soyMtrader_treat_50 = case_when (YEAR>=2006 & soyMtrader_share_50== 1 ~ 1,# set treated
                                                                                          TRUE ~ NA_real_)) %>% 
  group_by(GEOCODE) %>% arrange(GEOCODE, YEAR) %>%  # fill up and down
  fill(soyMtrader_treat_50, .direction="down") %>% # fill treated for all subsequent years after first treatment
  mutate(soyMtrader_treat_50= case_when(is.na(soyMtrader_treat_50) ~ 0, # # fill untreated for all pre-treatment years with 0
                                        TRUE~ soyMtrader_treat_50)) 
first50 <- MarketShareData_S06b %>% filter (soyMtrader_treat_50==1)%>% group_by(GEOCODE) %>% summarise(first_treat50 = min(YEAR))
MarketShareData_S06b <- MarketShareData_S06b  %>% left_join(first50) 
MarketShareData_S06b <- MarketShareData_S06b %>% replace_na(list(first_treat50=0))
MarketShareData_S06b <- MarketShareData_S06b %>% filter (YEAR>=2006) 
# write data -----
write_csv(MarketShareData_S05b %>% arrange(GEOCODE, YEAR), file.path(out, "SoyZDCs_05_v1.csv"))
write_rds(MarketShareData_S05b %>% arrange(GEOCODE, YEAR), file.path(out, "SoyZDCs_05_v1.rds"))

write_csv(MarketShareData_S06b %>% arrange(GEOCODE, YEAR), file.path(out, "SoyZDCs_06_v1.csv"))
write_rds(MarketShareData_S06b %>% arrange(GEOCODE, YEAR), file.path(out, "SoyZDCs_06_v1.rds"))
## 
