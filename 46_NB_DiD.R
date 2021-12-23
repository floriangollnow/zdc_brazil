#DID model for estimating deforestation leakage using 
# response: average neighbors soy-deforestation 
# treatment ZDC market share treatment
# controll: neighbors average ZDC market share 


library(tidyverse)
library(did)
#library(sf)
library(ggpubr)

# directories -------
out_file <-"/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"
down_file <- "/Users/floriangollnow/Downloads/"

# read main data file -----
MS_data <- read_rds( file.path(out_file, "NB_SoyZDCs_05_v1.rds"))
MS_data <- MS_data %>% mutate (log_def4soyMT5_ha= log1p(def4soyMT5_ha))
MS_data <- MS_data %>% mutate (log_NB_def4soyMT5_ha= log1p(Neigh_soy_def))


## how many munis and years
n_distinct(MS_data$GEOCODE)#45
n_distinct(MS_data$YEAR)#11
n_distinct(MS_data$GEOCODE)*n_distinct(MS_data$YEAR)#495

# change datatype for did package
MS_data <- MS_data%>% mutate (area_ha_int = as.integer(area_ha),geocode_n= as.numeric(GEOCODE))
MS_data.df <- as.data.frame(MS_data)


for (i in c(1,2)){
  print(i)
  out <- att_gt(yname = "log_NB_def4soyMT5_ha",
                gname = "first_treat50",
                idname = "geocode_n",
                clustervars = "geocode_n",
                tname = "YEAR",
                xformla = ~Neighbor_SoyMShare,
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
  
  assign(x = paste0(c("n", "ny")[i], "_main_NB"), value = te)
  ggsave(filename = file.path(down_file, c(paste0("W_log_def5_nTreat07.png"),paste0("W_log_def5_nyTreat07", i, ".png"))[i] ),plot =te , width=9, height=9 ,units = "cm", dpi=600)
  
  summary(group_effects)
  summary(es)
  
  if (i==2){
    # save group effect for not yet treated
    group_effects1 <- group_effects
  }
}
# generate plot and read all prior plots from 43_DiD_avoided_deforestation
ny_main <- read_rds(file.path (out_file,"ggplot","model_a.rds"))
n_main <- read_rds (file.path (out_file,"ggplot","model_b.rds"))
ny_3_main <- read_rds (file.path (out_file,"ggplot","model_c.rds"))
ny_06 <- read_rds ( file.path (out_file,"ggplot","model_d.rds"))
# combine plots
effect_exposure <- ggarrange (ny_main+ggtitle("Model a") , n_main  +ggtitle("Model b"), ny_3_main  +ggtitle("Model c"), ny_06  +ggtitle("Model d"),ny_main_NB  +ggtitle("Model e"), nrow = 1, ncol=5)
ggsave(filename = file.path(down_file, "effect_exposure_nb.png" ), plot = effect_exposure, width=9*5, height=9 ,units = "cm", dpi=600)

