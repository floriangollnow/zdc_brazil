# DID model run and calculating avoided deforestation (RQ3&4)

library(tidyverse)
library(did)
#library(sf)
library(ggpubr)

# directories -------
out_file <-"/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"
down_file <- "/Users/floriangollnow/Downloads/"

# read main data file -----
MS_data <- read_rds( file.path(out_file, "SoyZDCs_05_v1.rds"))
MS_data <- MS_data %>% mutate (log_def4soyMT5_ha= log1p(def4soyMT5_ha))

## how many munis and years
n_distinct(MS_data$GEOCODE)
n_distinct(MS_data$YEAR)
n_distinct(MS_data$GEOCODE)*n_distinct(MS_data$YEAR)

# change datatype for did package
MS_data <- MS_data%>% mutate (area_ha_int = as.integer(area_ha),geocode_n= as.numeric(GEOCODE))
MS_data.df <- as.data.frame(MS_data)

# plot distribution of soyM_ZDC ----
ggplot ()+geom_point (data=MS_data, aes(YEAR, soyMtrader_share), color="blue")+
  geom_line (data=MS_data, aes(YEAR, soyMtrader_treat_75*100, colour="75%"))+
  geom_line (data=MS_data, aes(YEAR, soyMtrader_treat_25*100,color="25%"))+
  geom_line (data=MS_data, aes(YEAR, soyMtrader_treat_50*100,color="50%"))+
  scale_x_continuous(limits = c(2005, 2015), breaks = seq(2005, 2015, by=2))+
  scale_colour_manual(name="ZDC thresholds",
                      values=c('75%'="#d95f02", '50%'="#1b9e77", '25%'="#7570b3"))+
  xlab("Year")+ylab("ZDC market share")+
  facet_wrap(~GEOCODE)+
  theme_bw()+
  theme (axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

ggsave(filename = file.path(down_file, "treatment_munis.png"), width=9, height=9 ,units = "cm", dpi=600,scale=2)
# robustness to treatment coding! setting those treatments to 0 if at more than half of the post treatment observations were untreated
MS_data.df_3 <- MS_data.df %>% mutate (first_treat50= case_when(GEOCODE=="1100031" ~ 0,
                                                                GEOCODE=="1101468" ~ 0,
                                                                GEOCODE=="2100055" ~ 0,
                                                                GEOCODE=="5103056" ~ 0,
                                                                GEOCODE=="5104542" ~ 0,
                                                                GEOCODE=="5107941" ~ 0,
                                                                TRUE ~ first_treat50))


# run DiD  ----
## Main model i=2 -> (i=1 -> nevertreated, i=2 -> not yet treated)
for (i in c(1,2)){
  print(i)
  out <- att_gt(yname = "log_def4soyMT5_ha",
                gname = "first_treat50",
                idname = "geocode_n",
                clustervars = "geocode_n",
                tname = "YEAR",
                xformla = ~1,
                data = MS_data.df,
                est_method = "dr",
                control_group = c("nevertreated", "notyettreated")[i],# not yet treated includes all, not yet treated and never treated!
                weightsname = "area_ha_int",
                alp = 0.05,
                biters=10000,
                anticipation = 0
                
  )
  es <- aggte(out, type = "dynamic", max_e = 6, min_e = -6,na.rm=T)
  group_effects <- aggte(out, type = "group", na.rm=T)
  
  print("transformed att")
  #calculated as: https://github.com/bcallaway11/did/blob/master/R/AGGTEobj.R
  print( round((exp(group_effects$overall.att)-1)*100,2))
  alp <- 0.05
  pointwise_cval <- qnorm(1-alp/2)
  overall_cband_upper <- group_effects$overall.att + pointwise_cval*group_effects$overall.se
  overall_cband_lower <- group_effects$overall.att - pointwise_cval*group_effects$overall.se
  print("transformed CI")
  print (round ((exp(overall_cband_upper)-1)*100,2))
  print( round((exp(overall_cband_lower)-1)*100,2))
  print ("transformed SE")
  print( round ((exp(group_effects$overall.se)-1)*100,2))
  # plot average effect over time
  te <- ggdid(es, ylim = c(-4,4))+
    xlab("Year")+ylab("ATT")+ ggtitle(NULL)+
    scale_color_discrete(labels=c("pre-SoyM", "post-SoyM"))+
    theme_bw()+
    theme(legend.title = element_blank(), legend.position = c(0.20,0.85))

  assign(x = paste0(c("n", "ny")[i], "_main"), value = te)
  ggsave(filename = file.path(down_file, c(paste0("W_log_def5_nTreat07.png"),paste0("W_log_def5_nyTreat07", i, ".png"))[i] ),plot =te , width=9, height=9 ,units = "cm", dpi=600)
  
  summary(group_effects)
  summary(es)
  
  if (i==2){
    # save group effect for not yet treated
    group_effects1 <- group_effects
  }
}



# Robuast against treatment definition  ------
out_3 <- att_gt(yname = "log_def4soyMT5_ha",
                gname = "first_treat50",
                idname = "geocode_n",
                clustervars = "geocode_n",
                tname = "YEAR",
                xformla = ~1,
                data = MS_data.df_3,
                est_method = "dr",
                control_group = "notyettreated",
                weightsname = "area_ha_int",
                alp = 0.05,
                biters=10000,
                anticipation = 0)

es_3 <- aggte(out_3, type = "dynamic", max_e = 6, min_e = -6,na.rm=T)
group_effects_3 <- aggte(out_3, type = "group", na.rm=T)
summary(group_effects_3)
print("transformed att")
#calculated as: https://github.com/bcallaway11/did/blob/master/R/AGGTEobj.R
print( round((exp(group_effects_3$overall.att)-1)*100,2))
alp <- 0.05
pointwise_cval <- qnorm(1-alp/2)
overall_cband_upper_3 <- group_effects_3$overall.att + pointwise_cval*group_effects_3$overall.se
overall_cband_lower_3 <- group_effects_3$overall.att - pointwise_cval*group_effects_3$overall.se
print("transformed CI")
print (round ((exp(overall_cband_upper_3)-1)*100,2))
print( round((exp(overall_cband_lower_3)-1)*100,2))
print ("transformed SE")
print( round ((exp(group_effects_3$overall.se)-1)*100,2))

te_3 <- ggdid(es, ylim = c(-4,4))+
  xlab("Year")+ylab("ATT")+ ggtitle(NULL)+
  scale_color_discrete(labels=c("pre-SoyM", "post-SoyM"))+
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.20,0.85))
ny_3_main <- te_3
ggsave(filename = file.path(down_file, "W_log_def5_nyTreat50_3.png" ), width=9, height=9 ,units = "cm", dpi=600)




# Robust against time subset: 2006-2015 -----
#
MS_data_06<- read_rds( file.path(out_file, "SoyZDCs_06_v1.rds"))
MS_data_06 <- MS_data_06 %>% mutate (log_def4soyMT5_ha= log1p(def4soyMT5_ha),
                                         area_ha_int = as.integer(area_ha), 
                                         geocode_n= as.numeric(GEOCODE))
MS_data_06 %>% filter (first_treat50!=2006) %>%select (GEOCODE) %>%  n_distinct()
MS_data_06 %>% filter (first_treat50!=2006) %>%select (YEAR) %>%  n_distinct()
MS_data.df_sub <- as.data.frame (MS_data_06)
out_sub <- att_gt(yname = "log_def4soyMT5_ha",
                  gname = "first_treat50",
                  idname = "geocode_n",
                  clustervars = "geocode_n",
                  tname = "YEAR",
                  xformla = ~1,
                  data = MS_data.df_sub,
                  est_method = "dr",
                  control_group = "notyettreated",
                  weightsname = "area_ha_int",
                  alp = 0.05,
                  biters=10000,
                  anticipation = 0)

es_sub <- aggte(out_sub, type = "dynamic", max_e = 6, min_e = -6,na.rm=T)
group_effects_sub <- aggte(out_sub, type = "group", na.rm=T)
summary(group_effects_sub)
print("transformed att")
#calculated as: https://github.com/bcallaway11/did/blob/master/R/AGGTEobj.R
print( round((exp(group_effects_sub$overall.att)-1)*100,2))
alp <- 0.05
pointwise_cval <- qnorm(1-alp/2)
overall_cband_upper_sub <- group_effects_sub$overall.att + pointwise_cval*group_effects_sub$overall.se
overall_cband_lower_sub <- group_effects_sub$overall.att - pointwise_cval*group_effects_sub$overall.se
print("transformed CI")
print (round ((exp(overall_cband_upper_sub)-1)*100,2))
print( round((exp(overall_cband_lower_sub)-1)*100,2))
print ("transformed SE")
print( round ((exp(group_effects_sub$overall.se)-1)*100,2))

ny_06<- ggdid(es, ylim = c(-4,4))+
  xlab("Year")+ylab("ATT")+ ggtitle(NULL)+
  scale_color_discrete(labels=c("pre-SoyM", "post-SoyM"))+
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.20,0.85))
ggsave(filename = file.path(down_file, "W_log_def5_nTreat50_year06.png" ), width=9, height=9 ,units = "cm", dpi=600)

# save all average time effect plots
write_rds(ny_main,file.path (out_file,"ggplot","model_a.rds"))
write_rds (n_main,file.path (out_file,"ggplot","model_b.rds"))
write_rds (ny_3_main,file.path (out_file,"ggplot","model_c.rds"))
write_rds (ny_06, file.path (out_file,"ggplot","model_d.rds"))

effect_exposure <- ggarrange (ny_main+ggtitle("Model a") , n_main  +ggtitle("Model b"), ny_3_main  +ggtitle("Model c"), ny_06  +ggtitle("Model d"), nrow = 1, ncol=4)
ggsave(filename = file.path(down_file, "effect_exposure.png" ), plot = effect_exposure, width=9*4, height=9 ,units = "cm", dpi=600)

###################################
# Avoided Deforestation ----------------
#################################3
# effects from main model
summary(group_effects)
ATTeffect <-  (exp(group_effects1$overall.att)-1)
exp(group_effects1$overall.se)-1
overall_cband_upper <- exp(group_effects1$overall.att + pointwise_cval*group_effects1$overall.se)-1
overall_cband_lower <- exp(group_effects1$overall.att - pointwise_cval*group_effects1$overall.se)-1


# treatment SoyM and ZDCs (GZDCs) ----

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
## calc avoided deforestation
Stud_area_A <- MarketShareData_A %>% select (GEOCODE, YEAR, defMT, def4soyMT5_ha, first_treat50SoyM) %>% 
  mutate (baselinDef= case_when(first_treat50SoyM!=0 & YEAR >= first_treat50SoyM~ (def4soyMT5_ha/( 1+ ATTeffect)),
                                TRUE ~ def4soyMT5_ha),
          baselinDef_up = case_when(first_treat50SoyM!=0 & YEAR >= first_treat50SoyM ~ (def4soyMT5_ha/(1+overall_cband_upper )),
                                    TRUE ~ def4soyMT5_ha),
          baselinDef_lower = case_when(first_treat50SoyM!=0 & YEAR >= first_treat50SoyM ~ ((def4soyMT5_ha)/(1+overall_cband_lower)),
                                       TRUE ~ def4soyMT5_ha),
          avoidedDef = baselinDef - def4soyMT5_ha,
          avoidedDef_up = baselinDef_up - def4soyMT5_ha,
          avoidedDef_lower = baselinDef_lower - def4soyMT5_ha,
          
          observed =case_when(first_treat50SoyM!=0 & YEAR >= first_treat50SoyM ~ (baselinDef*(1+ATTeffect)),
                              TRUE ~ def4soyMT5_ha))
# check
Stud_area_A %>% select(def4soyMT5_ha, YEAR, first_treat50SoyM, avoidedDef, baselinDef) %>% View()
# print # Geocode, Avoided def
Stud_area_A %>% pull (GEOCODE) %>% n_distinct() 
Stud_area_A %>% pull(avoidedDef ) %>% sum()
Stud_area_A %>% pull(avoidedDef_up ) %>% sum()
Stud_area_A %>% pull(avoidedDef_lower ) %>% sum()
Stud_area_A %>% pull(def4soyMT5_ha) %>% sum()
(Stud_area_A %>% pull(avoidedDef) %>% sum()/Stud_area_A %>% pull(defMT) %>% sum())*100

## percent reduction
(Stud_area_A %>% pull(avoidedDef) %>% sum()/ 
    (Stud_area_A %>% pull(def4soyMT5_ha) %>% sum() +Stud_area_A %>% pull(avoidedDef)%>% sum()))*100
(Stud_area_A %>% pull(avoidedDef_up) %>% sum()/ 
    (Stud_area_A %>% pull(def4soyMT5_ha) %>% sum() +Stud_area_A %>% pull(avoidedDef_up)%>% sum()))*100
(Stud_area_A %>% pull(avoidedDef_lower) %>% sum()/ 
    (Stud_area_A %>% pull(def4soyMT5_ha) %>% sum() +Stud_area_A %>% pull(avoidedDef_lower)%>% sum()))*100

## Cerrado avoided def - GZDC
MarketShareData_C <-  MarketShareData.time %>% filter (Biome_lArea=="Cerrado", YEAR>=2006, YEAR<=2015) 


Stud_area_C <- MarketShareData_C %>% select (GEOCODE, YEAR, def4soyMT5_ha, first_treat50GZDC) 
Stud_area_C  <- Stud_area_C %>% mutate (treatmentDef = case_when(first_treat50GZDC!=0 & YEAR >= first_treat50GZDC ~ (def4soyMT5_ha * (1+ATTeffect)),
                                                                 TRUE ~ def4soyMT5_ha),
                                        treatmentDef_up = case_when(first_treat50GZDC!=0 & YEAR >= first_treat50GZDC ~ (def4soyMT5_ha * (1+overall_cband_upper )),
                                                                    TRUE ~ def4soyMT5_ha),
                                        treatmentDef_lower = case_when(first_treat50GZDC!=0 & YEAR >= first_treat50GZDC ~ (def4soyMT5_ha*(1+overall_cband_lower)),
                                                                       TRUE ~ def4soyMT5_ha),
                                        avoidedDef =  def4soyMT5_ha - treatmentDef,
                                        avoidedDef_up = def4soyMT5_ha - treatmentDef_up,
                                        avoidedDef_lower = def4soyMT5_ha - treatmentDef_lower)
# check
Stud_area_C %>% select(def4soyMT5_ha, YEAR, first_treat50GZDC, avoidedDef ) %>% View()
# print # Geocodem avoided def
Stud_area_C %>% pull(avoidedDef ) %>% sum()
Stud_area_C %>% pull(avoidedDef_up ) %>% sum()
Stud_area_C %>% pull(avoidedDef_lower ) %>% sum()
Stud_area_C %>% pull(def4soyMT5_ha) %>% sum()
# percent reduction
(Stud_area_C %>% pull(avoidedDef ) %>% sum()/Stud_area_C %>% pull(def4soyMT5_ha) %>% sum())*100
(Stud_area_C %>% pull(avoidedDef_up ) %>% sum()/Stud_area_C %>% pull(def4soyMT5_ha) %>% sum())*100
(Stud_area_C %>% pull(avoidedDef_lower ) %>% sum()/Stud_area_C %>% pull(def4soyMT5_ha) %>% sum())*100






