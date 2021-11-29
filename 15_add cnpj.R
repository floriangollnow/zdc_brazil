## add cnpj information to company database
# https://m.sebrae.com.br/Sebrae/Portal%20Sebrae/UFs/SP/Pesquisas/MPE_conceito_empregados.pdf

library(tidyverse)
library(pastecs)
cnpj.dir <- "/Users/floriangollnow/dropbox/ZDC_project/Data/cnpj"
trase_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"
trase_out <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj/CompanyCharacteristic"
#read data
trase25g_dh <- read_rds(file.path (trase_dir , "trase_25_cnpj_GROUPED_processing_defH.rds"))
CNPJ.tb<-  read_rds(file.path(cnpj.dir , "Trase_CNPJ_group_join_slurm_7.rds"))

#reduce to unique cnpj's
CNPJ.tb.g <- CNPJ.tb %>%  group_by(main_cnpj) %>% summarise_at(vars(name, n_name_f, cnae_div:capital_max), pastecs::first, na.rm=TRUE)
#join
trase25g_dh_cnpj <- trase25g_dh %>% left_join(CNPJ.tb.g, by=c("main_cnpj"="main_cnpj")) # 

write_rds(trase25g_dh_cnpj, file.path (trase_dir , "trase_25_cnpj_GROUPED_processing_defH_cnpj.rds"))
write_rds(trase25g_dh_cnpj, file.path ("Data", "trase_25_cnpj_GROUPED_processing_defH_cnpj.rds"))
