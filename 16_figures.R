
# create final figures for company characteristics

library(tidyverse)
library(ggforce)
library(ggbeeswarm)
library(viridis)
library(RColorBrewer)
library(ggpubr)
library(scales)
trase_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"
trase_out <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj/CompanyCharacteristic"
dir_plot <- "/Users/floriangollnow/Dropbox/ZDC_project/Draft/plot"

trase25g_dh_cnpj.g <- read_rds(file.path (trase_dir , "trase_25_cnpj_GROUPED_processing_defH_cnpj.rds")) %>% ungroup()
##


###############################
# Amazon Biome ------------------
trase25g_dh_amazon_def_risk <- trase25g_dh_cnpj.g %>% filter(YEAR>=2016 & YEAR<=2018& BIOME=="AMAZONIA" & N_EXPORTER!="DOMESTIC CONSUMPTION") %>% 
  group_by( N_EXPORTER, SoyM) %>% 
  summarise (DefRisk_A = sum (TDef_ha,na.rm=TRUE),
             Def_mun_A = sum (TDef_ha_mun,na.rm=TRUE),
             SExport_A= sum (Stons, na.rm=TRUE),
             Years_A = n_distinct(YEAR),
             n_processingAm_18 = sum(RC_18, na.rm = TRUE),
             n_storageAm_18 = sum(ST_18, na.rm = TRUE),
             porte_empresa = first(porte_empresa),
             capital_social_c = first(capital_max))
#from deforestation hotspots in the Amazon
trase25g_dh_amazon_def_risk_2 <- trase25g_dh_cnpj.g %>% filter(YEAR>=2016 & YEAR<=2018& BIOME=="AMAZONIA"  & BIOME=="AMAZONIA" & N_EXPORTER!="DOMESTIC CONSUMPTION", TDefHotSpot_A==TRUE) %>% 
  group_by(N_EXPORTER,  SoyM) %>% 
  summarise ( SExportDefH_A = sum (Stons, na.rm = TRUE),
              YearsDefH =  n_distinct(YEAR),
              n_processingAmDefH_18 = sum(RC_18, na.rm = TRUE),
              n_storageAmDefH_18 = sum(ST_18, na.rm = TRUE),
              defH_A = sum(TDefHotSpot_A, na.rm=TRUE))

trase25g_dh_amazon_def_risk <- trase25g_dh_amazon_def_risk %>% left_join(trase25g_dh_amazon_def_risk_2, by=c("N_EXPORTER","SoyM")) 
# replace Nas
trase25g_dh_amazon_def_risk <- trase25g_dh_amazon_def_risk %>% replace_na(list( SExportDefH_A=0,
                                                                                YearsDefH=0,
                                                                                n_processingAmDefH_18=0,
                                                                                defH_A=0))
# calculate annual means
trase25g_dh_amazon_def_risk <- trase25g_dh_amazon_def_risk %>% mutate(DefRisk_ton = DefRisk_A/SExport_A,
                                                                      DefRisk_year = DefRisk_A/Years_A,
                                                                      Def_mun_ton = Def_mun_A/SExport_A,
                                                                      Def_mun_year = Def_mun_A/Years_A,
                                                                      Exp_Def_Hot_year = SExportDefH_A/Years_A,
                                                                      Exp_Def_Hot_perc = (SExportDefH_A/SExport_A)*100,
                                                                      SExport_year = SExport_A/Years_A,
                                                                      defH_A_year = if_else(is.na(defH_A), 0, defH_A/Years_A),
                                                                      defH_b  = case_when(defH_A>0 ~ TRUE,
                                                                                          TRUE ~FALSE))

#how many ZDCs                                                                                                                                                                                TRUE ~ FALSE))
table(trase25g_dh_amazon_def_risk$SoyM)
table(trase25g_dh_amazon_def_risk$SoyM, trase25g_dh_amazon_def_risk$n_processingAm_18)
table(trase25g_dh_amazon_def_risk$SoyM, trase25g_dh_amazon_def_risk$n_storageAm_18)
table(trase25g_dh_amazon_def_risk$SoyM, trase25g_dh_amazon_def_risk$n_storageAmDefH_18)

trase25g_dh_amazon_def_risk <- trase25g_dh_amazon_def_risk %>% ungroup()
table(trase25g_dh_amazon_def_risk$SoyM, trase25g_dh_amazon_def_risk$n_processingAm_18)

## shared value and soy exports
dots <- 3

#annual soy exports
a<- ggplot(trase25g_dh_amazon_def_risk, aes(SoyM,SExport_year/1000 ))+
  geom_violin ( aes(fill=SoyM), show.legend = F)+
  geom_quasirandom(color="blue", alpha = 0.7, width = 0.1,  size=3)+
  geom_boxplot(width=0.1, fill="transparent", alpha=0.5) +
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  scale_y_log10(labels = comma, limits=c(0.01,10000))+
  theme_bw()+
  labs(title= "Annual soy exports") + #
  scale_x_discrete(labels = c('No-Soy Moratorium','Soy Moratorium'))+
  xlab(NULL)+ ylab("ktons (log scale)")+
  stat_compare_means( method ="wilcox.test", label.y.npc = "bottom",label.x.npc = "left", size=3)+
  annotation_logticks(sides = "l")  

a

# companies share value
b<- ggplot(trase25g_dh_amazon_def_risk, aes(SoyM , capital_social_c/1000000 ))+
  geom_violin ( aes(fill=SoyM), show.legend = F)+
  geom_quasirandom(color="blue", alpha = 0.7, width = 0.1,  size=3)+
  geom_boxplot(width=0.1, fill="transparent", alpha=0.5) +
  #scale_fill_viridis_d( alpha=0.4, name= "ZD commitment", labels = c("NO", "YES"))+
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  scale_y_log10(labels = comma)+
  theme_bw()+
  labs(title= "Company share value") + #2018
  scale_x_discrete(labels = c('No-Soy Moratorium','Soy Moratorium'))+
  xlab(NULL)+ ylab("MBRL (log scale)")+
  annotation_logticks(sides = "l")  +
  stat_compare_means( method ="wilcox.test", label.y.npc = "bottom",label.x.npc = "center", size=3)
b

# exports from deforestation hotspost
d1<- ggplot(trase25g_dh_amazon_def_risk, aes(SoyM ,Exp_Def_Hot_perc))+
  geom_violin ( aes(fill=SoyM), show.legend = F)+
  geom_quasirandom(color="blue", alpha = 0.7, width = 0.1,  size=3)+
  geom_boxplot(width=0.1, fill="transparent", alpha=0.5, outlier.shape = NA) +
  #scale_fill_viridis_d( alpha=0.4, name= "ZD commitment", labels = c("NO", "YES"))+
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  #scale_y_log10(labels = comma, limits=c(0.01,10000))+
  theme_bw()+
  labs(title= "Soy exports from\ndeforestation hotspots") + #
  scale_x_discrete(labels = c('No-Soy Moratorium','Soy Moratorium'))+
  xlab(NULL)+ ylab("%")+
  #annotation_logticks(sides = "l")  +
  stat_compare_means( method ="wilcox.test", label.y.npc = "top",label.x.npc = "center", size=3)
d1


## processing and storage facilities

infr <- trase25g_dh_amazon_def_risk %>% select(SoyM, n_processingAm_18, n_processingAmDefH_18, n_storageAm_18, n_storageAmDefH_18)
infr_l <- infr %>% pivot_longer(cols = n_processingAm_18:n_storageAmDefH_18, names_to = "type", values_to="count")
infr_l_agg <- infr_l %>% group_by(SoyM, type) %>% summarise(count = sum(count, na.rm = TRUE))

e <- ggplot (infr_l_agg, aes(fill=SoyM ,type , count  ))+
  geom_bar(position="dodge", stat="identity", show.legend = F)+
  #scale_fill_viridis_d(alpha = 0.5,name= "ZD commitment", labels = c("NO", "YES"))+
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  scale_x_discrete(labels=c("P.", "P.dh.", "S.", "S.dh.")) +
  labs(title= "Soy infrastructure", x=NULL) + #
  theme_bw()#+
e
# combine figures
figure <- ggarrange(b,a,d1,e, nrow = 1, ncol=4, align="hv", labels = "auto")        
# annotate figure
figure_a <- annotate_figure(figure,
                            top = text_grob("Amazon", face = "bold")
)
figure_a
ggsave(file.path(dir_plot, "Amazon_traders.png"), plot= figure_a, width = 15 ,height = 5)


###


##########################
#  Cerrado ----------------

trase25g_dh_cerrado_def_risk <- trase25g_dh_cnpj.g %>% filter(YEAR>=2016 & YEAR<=2018& BIOME=="CERRADO" & N_EXPORTER!="DOMESTIC CONSUMPTION") %>% 
  group_by( N_EXPORTER, G_ZDC) %>% 
  summarise (DefRisk_C = sum (TDef_ha,na.rm=TRUE),
             Def_mun_C = sum (TDef_ha_mun,na.rm=TRUE),
             SExport_C= sum (Stons, na.rm=TRUE),
             Years_C = n_distinct(YEAR),
             n_processingCe_18 = sum(RC_18, na.rm = TRUE),
             n_storageCe_18 = sum(ST_18, na.rm = TRUE),
             porte_empresa = first(porte_empresa),
             capital_social_c = first(capital_max))

#from deforestation hotspots in the Amazon
trase25g_dh_cerrado_def_risk_2 <- trase25g_dh_cnpj.g %>% filter(YEAR>=2016 & YEAR<=2018& BIOME=="CERRADO" & N_EXPORTER!="DOMESTIC CONSUMPTION", TDefHotSpot_C==TRUE) %>% 
  group_by(N_EXPORTER,  G_ZDC) %>% 
  summarise ( SExportDefH_C = sum (Stons, na.rm = TRUE),
              YearsDefH =  n_distinct(YEAR),
              n_processingCeDefH_18 = sum(RC_18, na.rm = TRUE),
              n_storageCeDefH_18 = sum(ST_18, na.rm = TRUE),
              defH_C = sum(TDefHotSpot_C, na.rm=TRUE))

trase25g_dh_cerrado_def_risk <- trase25g_dh_cerrado_def_risk %>% left_join(trase25g_dh_cerrado_def_risk_2, by=c("N_EXPORTER","G_ZDC")) 
# replace Nas
trase25g_dh_cerrado_def_risk <- trase25g_dh_cerrado_def_risk %>% replace_na(list( SExportDefH_C=0,
                                                                                  YearsDefH=0,
                                                                                  n_processingCeDefH_18=0,
                                                                                  defH_C=0))
#
trase25g_dh_cerrado_def_risk <- trase25g_dh_cerrado_def_risk %>% mutate(DefRisk_ton = DefRisk_C/SExport_C,
                                                                        DefRisk_year = DefRisk_C/Years_C,
                                                                        Def_mun_ton = Def_mun_C/SExport_C,
                                                                        Def_mun_year = Def_mun_C/Years_C,
                                                                        Exp_Def_Hot_year = SExportDefH_C/Years_C,
                                                                        Exp_Def_Hot_perc = (SExportDefH_C/SExport_C)*100,
                                                                        SExport_year = SExport_C/Years_C,
                                                                        defH_C_year = if_else(is.na(defH_C), 0, defH_C/Years_C),
                                                                        defH_b  = case_when(defH_C>0 ~ TRUE,
                                                                                            TRUE ~FALSE))

#how many                                                                                                                                                                               TRUE ~ FALSE))
table(trase25g_dh_cerrado_def_risk$G_ZDC)
table(trase25g_dh_cerrado_def_risk$G_ZDC, trase25g_dh_cerrado_def_risk$n_processingCe_18)
table(trase25g_dh_cerrado_def_risk$G_ZDC, trase25g_dh_cerrado_def_risk$n_storageCe_18)
table(trase25g_dh_cerrado_def_risk$G_ZDC, trase25g_dh_cerrado_def_risk$n_storageCeDefH_18)

trase25g_dh_cerrado_def_risk <- trase25g_dh_cerrado_def_risk %>% ungroup()
table(trase25g_dh_cerrado_def_risk$G_ZDC, trase25g_dh_cerrado_def_risk$n_processingCe_18)

# soy export volumnes
a_c<- ggplot(trase25g_dh_cerrado_def_risk, aes(G_ZDC,SExport_year/1000 ))+
  geom_violin ( aes(fill=G_ZDC), show.legend = F)+
  geom_quasirandom(color="blue", alpha = 0.7, width = 0.1,  size=3)+
  geom_boxplot(width=0.1, fill="transparent", alpha=0.5) +
  #scale_fill_viridis_d( alpha=0.4, name= "ZD commitment", label = c("NO", "YES"))+
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  scale_y_log10(labels = comma, limits=c(0.01,10000))+
  theme_bw()+
  labs(title= "Annual soy exports") + #
  scale_x_discrete(labels = c('No-ZDC','ZDC'))+
  xlab(NULL)+ ylab("ktons (log scale)")+
  annotation_logticks(sides = "l")    +
  stat_compare_means( method ="wilcox.test", label.y.npc = "bottom",label.x.npc = "left", size=3)
a_c
# companies share value
b_c<- ggplot(trase25g_dh_cerrado_def_risk, aes(G_ZDC , capital_social_c/1000000 ))+
  geom_violin ( aes(fill=G_ZDC), show.legend = F)+
  geom_quasirandom(color="blue", alpha = 0.7, width = 0.1,  size=3)+
  geom_boxplot(width=0.1, fill="transparent", alpha=0.5) +
  #scale_fill_viridis_d( alpha=0.4, name= "ZD commitment", label=c("NO", "YES"))+
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  scale_y_log10(labels = comma)+
  theme_bw()+
  labs(title= "Company share value") + #
  scale_x_discrete(labels = c('No-ZDC','ZDC'))+
  xlab(NULL)+ ylab("MBRL (log scale)")+
  annotation_logticks(sides = "l")    +
  stat_compare_means( method ="wilcox.test", label.y.npc = "bottom",label.x.npc = "center", size=3)
b_c

#export from deforestation hotspots
d1_c<- ggplot(trase25g_dh_cerrado_def_risk, aes(G_ZDC ,Exp_Def_Hot_perc))+
  geom_violin ( aes(fill=G_ZDC),  show.legend=F)+
  geom_quasirandom(color="blue", alpha = 0.7, width = 0.1,  size=3)+
  geom_boxplot(width=0.1, fill="transparent", alpha=0.5) +
  #scale_fill_viridis_d( alpha=0.4, name= "ZD commitment", labels= c("NO", "YES"))+
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  #scale_y_log10(labels = comma, limits=c(0.01,10000))+
  theme_bw()+
  labs(title= "Soy exports from\ndeforestation hotspots") + #
  scale_x_discrete(labels = c('No-ZDC','ZDC'))+
  xlab(NULL)+ ylab("%")+
  #annotation_logticks(sides = "l")  +
  stat_compare_means( method ="wilcox.test", label.y.npc = "top",label.x.npc = "center", size=3)

d1_c

##  processing and storage facilities
infr <- trase25g_dh_cerrado_def_risk %>% select(G_ZDC, n_processingCe_18, n_processingCeDefH_18, n_storageCe_18, n_storageCeDefH_18)
infr_l <- infr %>% pivot_longer(cols = n_processingCe_18:n_storageCeDefH_18, names_to = "type", values_to="count")
infr_l_agg <- infr_l %>% group_by(G_ZDC, type) %>% summarise(count = sum(count, na.rm = TRUE))


e_c <- ggplot (infr_l_agg, aes(fill=G_ZDC ,type , count  ))+
  geom_bar(position="dodge", stat="identity")+
  #scale_fill_viridis_d(alpha = 0.5,name= "ZD commitment")+
  scale_fill_brewer(palette = "Set2", name= "ZDC", label=c("NO", "YES"))+
  scale_x_discrete(labels=c("P.", "P.dh.", "S.", "S.dh.")) +
  labs(title= "Soy infrastructure", x=NULL) + #
  theme_bw()#+
e_c
# combine figures
figure <- ggarrange(b_c,a_c,d1_c,e_c, nrow = 1, ncol=4, common.legend = TRUE, legend = 'bottom', align="hv", labels = c("e","f","g","h"))        

figure_b <- annotate_figure(figure,
                            top = text_grob("Cerrado", color = "black", face = "bold"))
figure_b 
ggsave(file.path(dir_plot, "Cerrado_traders.png"), plot= figure_b, width = 15 ,height = 5)

###
#combine Amazon and Cerrado
figure_ab <- ggarrange(figure_a , figure_b, heights = c(0.945,1.055), common.legend = TRUE, nrow = 2, ncol=1, align="hv")
figure_ab
ggsave(file.path(dir_plot, "Amazon_Cerrado_traders_v1.png"), plot= figure_ab, width = 11, height = 7, units = 'cm', dpi=600, scale=3 , bg="white")

