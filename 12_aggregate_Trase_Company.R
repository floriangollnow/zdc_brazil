
# builds on 01_data_preparation.R

library(tidyverse)
# Directory
trase_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"
# read data
trase25 <- read_rds(file.path (trase_dir, "trase_25_cnpj_v01.rds")) %>% ungroup()

# summarize it all for unique exporters and export locations for each year

trase25g <- trase25 %>% group_by_at(vars(c(YEAR:MUNICIPALITY, GEOCODE:nCNPJ))) %>% # removing economic block and export location: COUNTRY, `ECONOMIC BLOC`,
  summarise(Stons= sum(SOY_EQUIVALENT_TONNES, na.rm=TRUE),
            FOB_USD= sum(FOB_USD, na.rm=TRUE),
            LU_ha = sum(LAND_USE_HA, na.rm=TRUE),
            SDef5_ha = sum (SOY_DEFORESTATION_5_YEAR_ANNUAL_RISK_HA, na.rm=TRUE),
            S_CO2_em_t = sum (CO2_EMISSIONS_SOY_DEFORESTATION_5_YEAR_ANNUAL_RISK_TCO2, na.rm=TRUE),
            TDef_ha = sum(TERRITORIAL_DEFORESTATION_RISK_HA, na.rm=TRUE),
            T_CO2_emTHa = sum(`CO2_EMISSIONS_TERRITORIAL_DEFORESTATION_RISK_TCO2/HA`, na.rm=TRUE))

trase25g <- trase25g %>% ungroup () 
trase25g %>% View()
write_csv(trase25g , file.path(trase_dir, "trase_25_cnpj_GROUPED.csv"))
write_rds(trase25g , file.path(trase_dir, "trase_25_cnpj_GROUPED.rds"))

#dir(trase_dir)
