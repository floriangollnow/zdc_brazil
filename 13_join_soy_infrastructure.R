# adds soybean infrastructure (derived from trase) to company dataset (12__aggregate_trase_company.r)

library(tidyverse)
library(lubridate)


trase_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"
trase.logistics_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/logistics"


trase25g  <- read_rds(file.path(trase_dir, "trase_25_cnpj_GROUPED.rds"))

refining <- read_csv(file.path (trase.logistics_dir, "refining_facilities.csv"), col_types = "cccccddcccccc")
storage <- read_csv(file.path (trase.logistics_dir, "storage_facilities.csv"), col_types = "cccccdccccccccc") # has CNPJ 
storage <- storage %>% mutate (main_cnpj = substr(str_remove_all(storage$cnpj, "[\\.|\\-|\\/]"), 1,8))
storage <- storage %>% group_by(date, uf,geocode, company, main_cnpj) %>% summarise (n_storage = n())

crushing <- read_csv(file.path (trase.logistics_dir, "crushing_facilities.csv"), col_types = "ccccdccccccdccccc")# has cnpj
crushing <- crushing %>% mutate (main_cnpj = substr(str_remove_all(crushing$cnpj, "[\\.|\\-|\\/]"), 1,8))
crushing <- crushing %>% group_by(year,uf,  geocode, company, main_cnpj) %>% summarise (n_crushing = n())

# try do get cnpj for refining from crushing and storage !

refining_company   <- tibble (company= unique(refining$company))
crushing_company <- tibble(crushing %>% ungroup() %>% select(company, main_cnpj) %>% distinct()) %>% filter (!is.na(main_cnpj))
#storage_company <-tibble(storage %>% ungroup() %>% select(company, main_cnpj) %>% distinct()) %>% filter (!is.na(main_cnpj))

refining_company <-refining_company %>% left_join(crushing_company, by="company") 
refining <- refining %>% left_join(refining_company, by='company')



process <-  refining %>%ungroup()  %>% select (company, year, geocode, main_cnpj) %>% bind_rows(crushing %>%ungroup()%>% select (company,  year, geocode,main_cnpj))
process <- process %>% replace_na(list(year=2000))
storage <- storage %>% mutate (year= substr(date, 1,4)) %>% replace_na(list(year=2000))

process <- process %>% mutate(company=  if_else (str_detect(company,"DREYFUS"), "LOUIS DREYFUS", company),
                              company=  if_else (str_detect(company,"ALGODOEIRA"), "ALGODOEIRA", company),
                              company=  if_else (str_detect(company,"SODRUGESTVO"), "SODRUGESTVO", company),
                              company=  if_else (str_detect(company,"CENTURIA"), "CENTURIA", company),
                              company=  if_else (str_detect(company,"IMCOPA"), "IMCOPA", company),
                              company=  if_else (str_detect(company,"INTERAGRO"), "INTERAGRO", company),
                              company=  if_else (str_detect(company,"DREYFUS"), "LOUIS DREYFUS", company),
                              company=  if_else (str_detect(company,"RUBI"), "RUBI", company),
                              company=  if_else (str_detect(company,"SOJANOSSA"), "SOJANOSSA", company),
                              company=  if_else (str_detect(company,"UNILEVER"), "UNILEVER", company))
#View(sort(unique(process$company)))   

process_r_18 <- process %>% group_by(company, geocode, main_cnpj) %>% summarise(RC_18=n() )
storage_r_18 <- storage %>% group_by(company, geocode, main_cnpj) %>% summarise(ST_18=n() )
storage_r <- storage_r_18  %>% ungroup()
process_r <- process_r_18 %>% ungroup()

#join processing rw to trase by trader and main cnpj -> use max reported RC 
trase25g_rc <- trase25g %>% left_join(process_r %>% select(-main_cnpj), by=c("N_EXPORTER"= "company", "GEOCODE"="geocode")) %>% replace_na(list(RC_18=0)) %>% rename(RC_18_t1 = "RC_18")
trase25g_rc <- trase25g_rc %>% left_join(process_r%>% select(-company), by=c("main_cnpj", "GEOCODE"="geocode")) %>% replace_na(list(RC_18=0))%>% rename(RC_18_t2 = "RC_18")
trase25g_rc <- trase25g_rc %>% rowwise () %>%  mutate (RC_18 = max(RC_18_t1,RC_18_t2,na.rm=TRUE)) %>% select (-c(RC_18_t1,RC_18_t2))
#add storage based on cnpj 
storage_r <- storage_r %>% ungroup()
trase25g_rcs <- trase25g_rc %>% left_join(storage_r %>% select(-company),  by=c("main_cnpj", "GEOCODE"="geocode"))%>% replace_na(list(ST_18=0))


View(trase25g_rcs)

# write_csv(trase25g_rc, file.path (trase_dir , "trase_25_cnpj_GROUPED_processing.csv"))
write_rds(trase25g_rcs, file.path (trase_dir , "trase_25_cnpj_GROUPED_processing.rds"))


