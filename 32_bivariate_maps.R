# bivariate maps SR and ZDC coverage by municipality

library(tidyverse)
library(sf)
library(ggpubr)
#library(ggbeeswarm)
library(Hmisc)
library(RColorBrewer)
library(tmaptools)
library (rmapshaper)
library(viridis)


dir_plot <- "/Users/floriangollnow/Dropbox/ZDC_project/Draft/plot"
trase_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"
in_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA"
admin.dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/Admin/IBGE/2015/br_municipios/"

muni_biome <- st_read(file.path(in_dir, "Admin/IBGE/2015/br_municipios/BRMUE250GC_all_biomes_WGS84.shp")) %>% rmapshaper::ms_simplify( keep=0.05)
munis <- read_sf(file.path(admin.dir, "BRMUE250GC_WGS84.shp"))
states <-read_rds (file.path(in_dir, "Admin/IBGE/StatesBR_WGS84.rds"))%>% rmapshaper::ms_simplify( keep=0.05)
Matopiba <- muni_biome %>% filter ((Stat_bb=="MA"| Stat_bb=="TO"|Stat_bb=="PI"|Stat_bb=="BA")& Bim_lAr=="Cerrado") %>% st_make_valid() %>% st_union() %>%  st_as_sf() %>% mutate(Matopiba=" ")



#trase
trase25g <- read_rds (file.path (trase_dir, "MarketShare_annual_v1.rds"))#"MarketShare_annual_rates_allNew.rds"))
#mammals
mammals <- read_csv  (file.path (in_dir, "IUCN/all_threatened", "all_tvf_sr_muni.csv"), col_types = "ccd")
# combine
trase25g_j <- trase25g %>% left_join(mammals,by=c("GEOCODE"="CD_GEOC"))
trase25g_C <- trase25g_j %>% filter(Biome_lArea == "Cerrado")
trase25g_A <- trase25g_j %>% filter(Biome_lArea == "Amazônia")

######################
# Cerrado prep data -----------
## 2018 -> classify ZDC%
trase25g_C18 <- trase25g_C %>% filter(YEAR==2018) %>% mutate(GZDCtrader_shareClass = case_when(GZDCtrader_share==0| is.na(GZDCtrader_share) ~"0",
                                                                                               GZDCtrader_share>0 & GZDCtrader_share<=25~ paste0(">0","\U2264", "25"),
                                                                                               GZDCtrader_share>25 & GZDCtrader_share<=50~ paste0(">25","\U2264", "50"),
                                                                                               GZDCtrader_share>50 & GZDCtrader_share<=75~ paste0(">50","\U2264", "75"),
                                                                                               GZDCtrader_share>75 & GZDCtrader_share<=100~ paste0(">75","\U2264", "100"))) %>%
  mutate(GZDCtrader_shareClass=factor(GZDCtrader_shareClass, 
                                      levels=c("0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))),
         soy_prod = factor (case_when(soy_ha>0|GZDCtrader_share>0 ~ TRUE,
                                      TRUE ~ FALSE), levels= c("FALSE", "TRUE")))

trase25g_C18_f_sr <-trase25g_C18%>% select(GEOCODE, GZDCtrader_share, GZDCtrader_shareClass,sr, forestMT_ha, GAEZ_apt_forest_area_ha, soy_ha, soyIBGE_ha) %>% mutate (forestMT_ha_mS= forestMT_ha -GAEZ_apt_forest_area_ha)

# calc soy suitable forest sepecies importance
trase25g_C18_f_sr <- trase25g_C18_f_sr %>% mutate(x = exp(log(sr) - (0.25 * log(forestMT_ha)))) 
trase25g_C18_f_sr <- trase25g_C18_f_sr %>% mutate(test= (log(x)+(0.25*log(forestMT_ha))),
                                                  test2 = exp(test),
                                                  scenario = (log(x)+(0.25*log(forestMT_ha_mS))),## forestMT_ha_mS is forest ha minus soy suitable forest
                                                  scenario2 = exp(scenario),
                                                  loss = (sr-scenario2),
                                                  perc_change = case_when(GAEZ_apt_forest_area_ha>0~((sr-scenario2) /sr)*100,
                                                                          TRUE~NA_real_),
                                                  perc_changeClass = cut(perc_change, breaks=c(0,1,10,25,100)))


#trase25g_C18_f_sr %>% View()
# stats
mean(trase25g_C18_f_sr$perc_change, na.rm=TRUE)
mean(trase25g_C18_f_sr %>% filter(GZDCtrader_shareClass==">75≤100") %>% pull (perc_change), na.rm=TRUE)
sd(trase25g_C18_f_sr %>% filter(GZDCtrader_shareClass==">75≤100") %>% pull (perc_change), na.rm=TRUE)
mean(trase25g_C18_f_sr %>% filter(GZDCtrader_shareClass==">50≤75") %>% pull (perc_change), na.rm=TRUE)
mean(trase25g_C18_f_sr %>% filter(GZDCtrader_shareClass==">25≤50") %>% pull (perc_change), na.rm=TRUE)
mean(trase25g_C18_f_sr %>% filter(GZDCtrader_shareClass==">0≤25") %>% pull (perc_change), na.rm=TRUE)
mean(trase25g_C18_f_sr %>% filter(GZDCtrader_shareClass=="0") %>% pull (perc_change), na.rm=TRUE)

sd(trase25g_C18_f_sr$perc_change, na.rm=TRUE)
mean(trase25g_C18_f_sr$loss, na.rm=TRUE)
sd(trase25g_C18_f_sr$loss, na.rm=TRUE)

sum (trase25g_C18_f_sr$soy_ha)

sf_cerrado_sr <-  munis %>% inner_join(trase25g_C18_f_sr, by=c("CD_GEOCMU"= "GEOCODE"))
sf_cerrado_sr <- sf_cerrado_sr %>% ms_simplify()

# ZDC map
ZDC_C <- ggplot()+ 
  geom_sf(data=sf_cerrado_sr, aes(fill=GZDCtrader_shareClass), lwd=0.1, color="black") + #scale_fill_binned(type = 'viridis')
  scale_fill_brewer(palette = "Purples")+
  labs(title= "ZDC market share", fill="ZDC traders muncipality\nmarket share in 2018")+
  theme_bw()+
  theme(legend.position = "bottom")
ZDC_C
# soy suitable forest importance map
b_impo_C <- ggplot()+ 
  geom_sf(data=sf_cerrado_sr, aes(fill=perc_changeClass), lwd=0.1, color="black") + #scale_fill_binned(type = 'viridis')
  labs(title= "Biodiversity importance of soy-suitable forest", fill="Species % dependend\non soy-suitable\nforests")+
  theme_bw()+
  theme(legend.position = "bottom")
b_impo_C

###########################
# Cerrado bivariate map ----
bb <- st_bbox(muni_biome %>% filter (Bim_lAr=="Cerrado"))
# legend
d<-expand.grid(x=1:5,y=1:5)
g.legend_SR<-
  ggplot(d, aes(x,y,fill=atan(x/y),alpha=x+y))+
  geom_tile()+
  scale_fill_viridis()+
  scale_x_continuous(limits=c(0.5,5.5), expand = c(0, 0),breaks=c(1,2,3,4,5), labels=c("0",paste0(">0","\U2264", "25"),
                                                                                       paste0(">25","\U2264", "50"),
                                                                                       paste0(">50","\U2264", "75"),
                                                                                       paste0(">75","\U2264", "100")))+
  scale_y_continuous(limits=c(0.5,5.5),expand = c(0, 0), breaks=c(1,2,3,4,5), labels=c("0",paste0(">0", "\U2264", "1"), 
                                                                                       paste0(">1", "\U2264", "10"),
                                                                                       paste0(">10", "\U2264", "25"),
                                                                                       paste0(">25", "\U2264", "100")))+
  
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=1,l=1, r=10),
        panel.spacing=unit(10,"points"),
        axis.text.y = element_text(angle = 0, hjust =0.5),
        axis.text.x = element_text(angle = 90))+
  labs(y="Species %",
       x="ZDC %")

g.legend_SR
# classify sr values for map
sf_cerrado_sr<- sf_cerrado_sr %>% mutate(bi_sr = case_when(perc_change==0~1,
                                                           perc_changeClass=="(0,1]"~2,
                                                           perc_changeClass=="(1,10]"~3,
                                                           perc_changeClass=="(10,25]"~4,
                                                           perc_changeClass=="(25,100]"~5),
                                         bi_zdc = case_when(GZDCtrader_shareClass=="0"~1,
                                                            GZDCtrader_shareClass==">0≤25"~2,
                                                            GZDCtrader_shareClass==">25≤50"~3,
                                                            GZDCtrader_shareClass==">50≤75"~4,
                                                            GZDCtrader_shareClass==">75≤100"~5)
)
# bivariate map
gmap_bi_SR_C <- ggplot() +
  geom_sf(data=sf_cerrado_sr , 
          aes(fill =atan(bi_zdc/bi_sr),alpha=bi_sr +bi_zdc), color="grey50", show.legend = FALSE)+
  geom_sf(data=sf_cerrado_sr %>% arrange(desc(soy_ha)), aes(color=factor(soy_ha>0, levels=c(TRUE, FALSE))), fill=NA)+
  geom_sf(data=sf_cerrado_sr %>% filter(soy_ha>0), color="#a6611a", fill=NA)+
  scale_color_manual(values = c("#a6611a", "#999999"),labels =c("Soy producing", "No soy production"))+
  geom_sf(data=states,color = "black", fill = NA, size=0.5)+
  geom_sf(data=Matopiba, color="#c51b8a", fill="transparent", size=1.5, show.legend =F)+
  geom_sf_label(data=states, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,12),+1, rep(0,15)), nudge_x = c(rep(0,12),0, rep(0,15)), color="black", fill="white", alpha=0.5)+
  scale_fill_viridis(na.value = "transparent" )+
  labs(color = "Municipality", x="",y="")+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  theme(legend.position = 'bottom')
gmap_bi_SR_C
gg_leg <- get_legend(gmap_bi_SR_C)
ggarrange (gmap_bi_SR_C, ggarrange(NULL,g.legend_SR,NULL, ncol=1, nrow=3) ,ncol = 2, nrow=1,  widths  = c(1,0.45) )



###########
# Amazon prep data -------------
## 2018
trase25g_A18 <- trase25g_A %>% filter(YEAR==2018) %>% mutate(soyMtrader_shareClass = case_when(soyMtrader_share==0| is.na(soyMtrader_share) ~"0",
                                                                                               soyMtrader_share>0 & soyMtrader_share<=25~ paste0(">0","\U2264", "25"),
                                                                                               soyMtrader_share>25 & soyMtrader_share<=50~ paste0(">25","\U2264", "50"),
                                                                                               soyMtrader_share>50 & soyMtrader_share<=75~ paste0(">50","\U2264", "75"),
                                                                                               soyMtrader_share>75 & soyMtrader_share<=100~ paste0(">75","\U2264", "100"))) %>% 
  mutate(soyMtrader_shareClass=factor(soyMtrader_shareClass, 
                                      levels=c("0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))),
         soy_prod = factor (case_when(soy_ha>0|soyMtrader_share>0 ~ TRUE,
                                      TRUE ~ FALSE), levels= c("FALSE", "TRUE")))

trase25g_A18_f_sr <-trase25g_A18%>% select(GEOCODE, soyMtrader_shareClass,sr, forestMT_ha, GAEZ_apt_forest_area_ha, soy_ha) %>% 
  mutate (forestMT_ha_mS= if_else(forestMT_ha -GAEZ_apt_forest_area_ha<0, 0, forestMT_ha -GAEZ_apt_forest_area_ha))

trase25g_A18_f_sr <- trase25g_A18_f_sr %>% mutate(x = exp(log(sr) - (0.25 * log(forestMT_ha)))) 
trase25g_A18_f_sr <- trase25g_A18_f_sr %>% mutate(test= (log(x)+(0.25*log(forestMT_ha))),
                                                  test2 = exp(test),
                                                  scenario = (log(x)+(0.25*log(forestMT_ha_mS))),
                                                  scenario2 = exp(scenario),
                                                  loss = (sr-scenario2),
                                                  perc_change = case_when(GAEZ_apt_forest_area_ha>0~((sr-scenario2) / sr)*100,
                                                                          TRUE~NA_real_),
                                                  perc_changeClass = cut(perc_change, breaks=c(0,1,10,25,100)))


trase25g_A18_f_sr %>% View()
# stats
mean(trase25g_A18_f_sr$perc_change, na.rm=TRUE)
sd(trase25g_A18_f_sr$perc_change, na.rm=TRUE)
mean(trase25g_A18_f_sr$loss, na.rm=TRUE)
summary (trase25g_A18_f_sr$loss)
sd(trase25g_A18_f_sr$loss, na.rm=TRUE)

sf_amazon_sr <-  munis %>% inner_join(trase25g_A18_f_sr, by=c("CD_GEOCMU"= "GEOCODE"))
sf_amazon_sr <- sf_amazon_sr %>% ms_simplify()

# species importance map
b_impo_A <- ggplot()+ 
  geom_sf(data=sf_amazon_sr, aes(fill=cut(perc_change, breaks=c(0,1,10,25,100))), lwd=0.1, color="black") + #scale_fill_binned(type = 'viridis')
  scale_fill_brewer(palette = "PuBuGn", labels= c("0<1%", "1<10%","10<25%","25<100%", "NA"))+
  labs(title= "Biodiversity importance of soy-suitable forest", fill="Species % dependend\non soy-suitable\nforests")+
  theme_minimal()
b_impo_A
# explore by ZDC %
ggplot()+
  geom_bar( data=trase25g_A18_f_sr , aes( x=soyMtrader_shareClass, y=GAEZ_apt_forest_area_ha/1000, fill=perc_changeClass), stat="identity", alpha=0.9)+
  scale_fill_brewer(palette =  "Set1", na.translate=FALSE)+
  labs(title="Soy-suitable forest", x="SoyM-ZDC market share", y="Forest kha", fill="% species dependent\non soy-suitable forest")+
  scale_x_discrete(drop = FALSE)+
  theme_minimal()+
  theme (legend.position = "bottom", 
         panel.grid.major.y = element_line(color="grey80"),
         panel.grid.minor.y = element_line(color="grey80"))

#######################
# Amazon bivariate maps--------
bb <- st_bbox(muni_biome %>% filter (Bim_lAr=="Amazônia"))

sf_amazon_sr<- sf_amazon_sr %>% mutate(bi_sr = case_when(perc_change==0~1,
                                                         perc_changeClass=="(0,1]"~2,
                                                         perc_changeClass=="(1,10]"~3,
                                                         perc_changeClass=="(10,25]"~4,
                                                         perc_changeClass=="(25,100]"~5),
                                       bi_zdc = case_when(soyMtrader_shareClass=="0"~1,
                                                          soyMtrader_shareClass==">0≤25"~2,
                                                          soyMtrader_shareClass==">25≤50"~3,
                                                          soyMtrader_shareClass==">50≤75"~4,
                                                          soyMtrader_shareClass==">75≤100"~5)
)

gmap_bi_SR_A <- ggplot() +
  geom_sf(data=sf_amazon_sr ,
          aes(fill =atan(bi_zdc/bi_sr),alpha=bi_sr +bi_zdc), color="grey50", show.legend = F)+
  geom_sf(data=sf_amazon_sr , aes(color=factor(soy_ha>0, levels=c(TRUE, FALSE))), fill=NA, show.legend = F)+
  geom_sf(data=sf_amazon_sr %>% filter(soy_ha>0) , color="#a6611a", fill=NA, show.legend = F)+
  geom_sf(data=states,color = "black", fill = NA, size=0.5)+
  #geom_sf(data=Matopiba, color="#c51b8a", fill="transparent", size=1.5)+
  scale_color_manual(values = c("#a6611a", "#999999"),labels =c("Soy producing", "No soy production"))+
  geom_sf_label(data=states, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,12),+1, rep(0,15)), nudge_x = c(rep(0,12),0, rep(0,15)), color="black", fill="white", alpha=0.5)+
  scale_fill_viridis(na.value = "transparent", )+
  labs(color = "Municipality", x="",y="")+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  theme(legend.position = 'bottom')
gmap_bi_SR_A
g.legend_SR_1 <- g.legend_SR + theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(fill='transparent'), #transparent legend bg
  legend.box.background = element_rect(fill='transparent') #transparent legend panel
)
gmap_bi_SR_C_a <- gmap_bi_SR_C+annotation_custom(ggplotGrob(g.legend_SR_1), ymin = -1.9, ymax = -8.9, xmin = -53.2, xmax = -60.5)
gmap_bi_SR_C_a
gg_bi_map_sr <- ggarrange (gmap_bi_SR_A, gmap_bi_SR_C_a,labels="auto", ncol=2, nrow=1, widths=c(2,1.231), common.legend = TRUE, legend ="bottom")

ggsave(file.path(dir_plot, "SR_ZDC_bimap_2.png"), plot= gg_bi_map_sr, width = 22 ,height = 11, units = 'cm', dpi=600, scale=2) 



