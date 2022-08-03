# assessing soy suitable forest across ZDC categories in the Brazilian Cerrado and Amazon (RQ2) ----
# 127 following: selection bias based on municipality forest cover? -----
# 175 following: overall and soy-deforestation categorized by ZDC class (SI Figure 4)-----


library(tidyverse)
library(ggpubr)
library(ggbeeswarm)
library(Hmisc)
library(RColorBrewer)


dir_plot <- "/Users/floriangollnow/Dropbox/ZDC_project/Draft/plot"
trase_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"
in_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA"
admin.dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/Admin/IBGE/2015/br_municipios/"

## trase
trase25g <- read_rds (file.path (trase_dir, "MarketShare_annual_v1.rds"))
#
# soy production share Cerrado and amazon
all2018 <- trase25g %>% filter (YEAR==2018) %>% summarise(soyha = sum(soy_ha))
amazon2018 <- trase25g %>% filter (YEAR==2018, Biome_lArea == "Amazônia") %>% summarise(soyha = sum(soy_ha))
cerrado2018 <- trase25g %>% filter (YEAR==2018, Biome_lArea == "Cerrado") %>% summarise(soyha = sum(soy_ha))
amazon2018/all2018
cerrado2018/all2018


## by Biome
trase25g_C <- trase25g %>% filter(Biome_lArea == "Cerrado")
trase25g_A <- trase25g %>% filter(Biome_lArea == "Amazônia")


# Cerrado
# select 2018 with soy and build ZDC classes
trase25g_C18 <- trase25g_C %>% filter(YEAR==2018) %>% mutate(GZDCtrader_shareClass = case_when(soy_ha==0 ~ "No soy",
                                                                                               GZDCtrader_share==0| is.na(GZDCtrader_share) ~"0",
                                                                                               GZDCtrader_share>0 & GZDCtrader_share<=25~ paste0(">0","\U2264", "25"),
                                                                                               GZDCtrader_share>25 & GZDCtrader_share<=50~ paste0(">25","\U2264", "50"),
                                                                                               GZDCtrader_share>50 & GZDCtrader_share<=75~ paste0(">50","\U2264", "75"),
                                                                                               GZDCtrader_share>75 & GZDCtrader_share<=100~ paste0(">75","\U2264", "100"))) %>% 
  mutate(GZDCtrader_shareClass=factor(GZDCtrader_shareClass, 
                                      levels=c("No soy", "0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))),
         soy_prod = factor (case_when(soy_ha>0|GZDCtrader_share>0 ~ TRUE,
                                      TRUE ~ FALSE), levels= c("FALSE", "TRUE")))

# Amazon
# select 2018 with soy
trase25g_A18 <- trase25g_A %>% filter(YEAR==2018) %>% mutate(soyMtrader_shareClass = case_when(soy_ha==0 ~ "No soy",
                                                                                               soyMtrader_share==0| is.na(soyMtrader_share) ~"0",
                                                                                               soyMtrader_share>0 & soyMtrader_share<=25~ paste0(">0","\U2264", "25"),
                                                                                               soyMtrader_share>25 & soyMtrader_share<=50~ paste0(">25","\U2264", "50"),
                                                                                               soyMtrader_share>50 & soyMtrader_share<=75~ paste0(">50","\U2264", "75"),
                                                                                               soyMtrader_share>75 & soyMtrader_share<=100~ paste0(">75","\U2264", "100"))) %>% 
  mutate(soyMtrader_shareClass=factor(soyMtrader_shareClass, 
                                      levels=c("No soy","0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))))#,

###############################
# Soy Suitable Forest by ZDC class ----------
##############################

# Cerrado
# summarize by class
Cerrado_bar <- trase25g_C18  %>%  group_by(GZDCtrader_shareClass) %>% 
  summarise(GAEZ_apt_forest_area_ha= sum(GAEZ_apt_forest_area_ha, na.rm=TRUE))

totalsdd <- Cerrado_bar %>%  group_by(GZDCtrader_shareClass)%>% summarise (NPSUIT = sum(GAEZ_apt_forest_area_ha))
totalsdd <- totalsdd %>% ungroup()%>% mutate(totalF = sum(NPSUIT),
                                             percentF = ((NPSUIT/totalF)*100))
# area by ZDC class
totalsdd 
# total area in kha
totalsdd %>% summarise(npsuit = sum(NPSUIT)/1000)


gg_g_aptC <- ggplot()+
  geom_bar(data=Cerrado_bar , aes (y=GAEZ_apt_forest_area_ha/1000, x=GZDCtrader_shareClass),stat = "identity", position='stack')+
  labs(title = "Cerrado: Soy-suitable forest", x="ZDC market share", y="Forest kha", fill="Municipality")+
  geom_text(data=totalsdd, aes(x= GZDCtrader_shareClass, y= (NPSUIT/1000)+1500),
            label=paste0(format (round(totalsdd$NPSUIT/1000, 0), big.mark=","),"kha\n (", round(totalsdd$percentF, 0),"%)"),
            size=3, show.legend = FALSE)+
  theme_minimal()
gg_g_aptC

#### Table 1
# only municipalities of soy-production  
totalsdd_zdc <- Cerrado_bar %>% filter (GZDCtrader_shareClass!="No soy" )%>% group_by(GZDCtrader_shareClass)%>% summarise (NPSUIT = sum(GAEZ_apt_forest_area_ha))
totalsdd_zdc <- totalsdd_zdc %>% ungroup()%>% mutate(totalF = sum(NPSUIT),
                                                     percentF = ((NPSUIT/totalF)*100))

totalsdd_zdc


## Amazonia
Amazon_bar <- trase25g_A18  %>%  group_by(soyMtrader_shareClass) %>% summarise(GAEZ_apt_forest_area_ha= sum(GAEZ_apt_forest_area_ha, na.rm=TRUE))

totalsdd <- Amazon_bar %>% group_by(soyMtrader_shareClass)%>% summarise (NPSUIT = sum(GAEZ_apt_forest_area_ha))
totalsdd <- totalsdd %>% ungroup()%>% mutate(totalF = sum(NPSUIT),
                                             percentF = ((NPSUIT/totalF)*100))

gg_g_aptA <- ggplot()+
  geom_bar(data=Amazon_bar , aes (y=GAEZ_apt_forest_area_ha/1000, x=soyMtrader_shareClass),stat = "identity", position='stack')+
  labs(title = "Amazon: Soy-suitable forest", x="ZDC market share", y="Forest kha", fill="Municipality")+
  geom_text(data=totalsdd, aes(x= soyMtrader_shareClass, y= (NPSUIT/1000)+2500),
            label=paste0(format (round(totalsdd$NPSUIT/1000, 0), big.mark=","),"kha\n (", round(totalsdd$percentF, 0),"%)"),
            size=3, show.legend = FALSE)+
  scale_x_discrete(drop = FALSE)+
  theme_minimal()
gg_g_aptA

#### Table 1
# only municipalities of soy-production  
totalsdd_zdc <- Amazon_bar %>% filter (soyMtrader_shareClass!="No soy" )%>% group_by(soyMtrader_shareClass)%>% summarise (NPSUIT = sum(GAEZ_apt_forest_area_ha))
totalsdd_zdc <- totalsdd_zdc %>% ungroup()%>% mutate(totalF = sum(NPSUIT),
                                                     percentF = ((NPSUIT/totalF)*100))

totalsdd_zdc

gg_CA <- ggarrange(gg_g_aptC,gg_g_aptA, ncol=2, nrow = 1, labels = "auto",common.legend = TRUE)
ggsave (filename = file.path(dir_plot, "suit_forest_cm.png"),plot=gg_CA, width = 11, height = 6 , units = "cm", dpi=600, scale=2)
ggsave (filename = file.path(dir_plot, "suit_forest_cm.tiff"),plot=gg_CA, width = 11, height = 6 , units = "cm", dpi=600, scale=2)




######################
# selection bias based on municipality forest cover? -----
######################
# cerrado
trase25g_C18 %>% group_by(GZDCtrader_shareClass) %>% summarise(M=round(mean(forestMT_perc), 0),
                                                               s = round(sd (forestMT_perc), 0))

gg_SB_C <- ggplot(data=trase25g_C18, aes(y= forestMT_perc, x=GZDCtrader_shareClass))+
  geom_quasirandom(data=trase25g_C18, aes(y= forestMT_perc, x=GZDCtrader_shareClass), alpha = 0.7, width = 0.30,  size=2, col="#FC8D62")+
  #scale_color_continuous(type="viridis")+
  geom_violin(data=trase25g_C18, aes(y= forestMT_perc, x=GZDCtrader_shareClass), fill=NA, color="grey30")+
  geom_boxplot( width=0.20, alpha=0.5,outlier.shape = NA)+
  stat_compare_means(comparisons = list( c("No soy", paste0(">75","\U2264", "100")),
                                         c("0", paste0(">75","\U2264", "100")),
                                         c(paste0(">0","\U2264", "25"),paste0(">75","\U2264", "100")),
                                         c(paste0(">25","\U2264", "50"),paste0(">75","\U2264", "100")),
                                         c(paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))), method ="wilcox.test")+
  #stat_compare_means(label.y = 135)+
  labs(title = "Cerrado: Municipality forest cover 2018", y= "Forest %", x="ZDC market share")+
  theme_minimal()
# amazon
trase25g_A18 %>% group_by(soyMtrader_shareClass) %>% summarise(M=round(mean(forestMT_perc), 0),
                                                               s = round(sd (forestMT_perc), 0))


gg_SB_A<-  ggplot(data=trase25g_A18, aes(y= forestMT_perc, x=soyMtrader_shareClass))+
  geom_quasirandom(data=trase25g_A18, aes(y= forestMT_perc, x=soyMtrader_shareClass), alpha = 0.7, width = 0.30,  size=2, col="#FC8D62")+
  geom_violin(data=trase25g_A18, aes(y= forestMT_perc, x=soyMtrader_shareClass), fill=NA, color="grey30",width = 1)+
  geom_boxplot( width=0.20, alpha=0.5,outlier.shape = NA)+
  stat_compare_means(comparisons = list( c("No soy", paste0(">75","\U2264", "100")),
                                         c("0", paste0(">75","\U2264", "100")),
                                         #c(paste0(">0","\U2264", "25"),paste0(">75","\U2264", "100")),
                                         c(paste0(">25","\U2264", "50"),paste0(">75","\U2264", "100")),
                                         c(paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100"))), method ="wilcox.test")+
  scale_x_discrete(drop=FALSE)+
  labs(title = "Amazon: Municipality forest cover 2018", y= "Forest %", x="ZDC market share")+
  theme_minimal()

gg_SB_CA <- ggarrange(gg_SB_C,gg_SB_A, ncol=2, nrow = 1, labels = "auto")
ggsave (filename = file.path(dir_plot, "selectionbias_forest_cm.png"),plot=gg_SB_CA, width = 11, height = 6 , units = "cm", dpi=600, scale=2)
ggsave (filename = file.path(dir_plot, "selectionbias_forest_cm.tiff"),plot=gg_SB_CA, width = 11, height = 6 , units = "cm", dpi=600, scale=2)
## combine
gg_combine <- ggarrange(gg_g_aptC,gg_g_aptA,gg_SB_C,gg_SB_A, ncol=2, nrow = 2,labels = "auto")
ggsave (filename = file.path(dir_plot, "suitforest_selectionbias_forest_cm.tiff"),plot=gg_combine, width = 11, height = 11 , units = "cm", dpi=600, scale=2)


#################################
## overall and soy-deforestation categorized by ZDC class -----
#################################
# Amazonia
trase25g_A10y <-  trase25g_A %>% filter (YEAR>2010&YEAR<2019) %>% 
  mutate (soyMtrader_shareClass= case_when(soy_ha==0 ~ "No soy",
                                           soyMtrader_share==0| is.na(soyMtrader_share) ~"0",
                                           soyMtrader_share>0 & soyMtrader_share<=25~ paste0(">0","\U2264", "25"),
                                           soyMtrader_share>25 & soyMtrader_share<=50~ paste0(">25","\U2264", "50"),
                                           soyMtrader_share>50 & soyMtrader_share<=75~ paste0(">50","\U2264", "75"),
                                           soyMtrader_share>75 & soyMtrader_share<=100~ paste0(">75","\U2264", "100"))) %>% 
  mutate(soyMtrader_shareClass=factor(soyMtrader_shareClass, 
                                      levels=c("No soy","0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100")))) %>% 
  group_by(soyMtrader_shareClass, YEAR) %>% summarise(def4soyMT5_ha=sum(def4soyMT5_ha),
                                                      defMT = sum(defMT),
                                                      suit_forest = last(GAEZ_apt_forest_area_ha))

totalsdd <- trase25g_A10y %>% group_by(soyMtrader_shareClass)%>% summarise (def4soyMT5_ha = sum(def4soyMT5_ha))
totalsdd <- totalsdd %>% ungroup()%>% mutate(totalSDeF = sum(def4soyMT5_ha),
                                             percentSDeF = ((def4soyMT5_ha/totalSDeF)*100))
total_dd <- trase25g_A10y %>% group_by(YEAR) %>% summarise(def4soyMT5_ha= sum(def4soyMT5_ha),
                                                           defMT = sum(defMT)) %>% 
  mutate (def4soy_per = (def4soyMT5_ha/defMT)*100)
total_dd

def_c <- trase25g_A10y %>% mutate (def = defMT-def4soyMT5_ha) %>%  
  pivot_longer(cols= c(def4soyMT5_ha, def), values_to = "ha", names_to = "Deforestation") %>% 
  group_by(Deforestation, YEAR) %>%summarise(ha=sum(ha)) 

def_allA<-ggplot()+ 
  geom_bar( data=def_c, aes(x=as.factor(YEAR), y=ha/1000, fill=Deforestation), stat = 'identity', position = 'stack', width = 0.7, alpha=0.9)+
  scale_fill_brewer(palette = "Set2",name=NULL, label=c("Deforestation for non-soy", "Deforestation for soy"))+
  geom_text(data=total_dd, aes(x= as.factor(YEAR), y= (defMT/1000)+50),
            label=paste0(format (round(total_dd$defMT/1000, -1), big.mark=","),"kha"),
            size=3, show.legend = FALSE)+
  geom_text(data=total_dd, aes(x= as.factor(YEAR), y=(def4soyMT5_ha/1000)+125),
            label=paste0(format (round(total_dd$def4soyMT5_ha/1000, 0), big.mark=","),"kha\n (", round(total_dd$def4soy_per, 0),"%)"),
            size=3, show.legend = FALSE)+
  scale_x_discrete(labels=c( (2011:2015), paste0(2016:2018, "*")))+
  theme_minimal()+
  labs(title= "Amazon: Deforestation", x=NULL, y="kha")+
  theme (legend.position = "bottom", 
         panel.grid.major.y = element_line(color="grey80"),
         panel.grid.minor.y = element_line(color="grey80"))

trase25g_A10y %>% filter (YEAR==2015) %>% group_by(soyMtrader_shareClass) %>% summarise(def = round (sum(defMT)/1000,0),
                                                                                        sdef = round(sum(def4soyMT5_ha)/1000,0)) %>% 
  mutate(perc_sdef= round((sdef/sum(sdef))*100, 0),
         perc_def = round((def/sum(def))*100, 0),
         sum_sdef = sum(sdef),
         sum_def= sum(def))

def_fraca <- 
  ggplot(trase25g_A10y, aes(x=as.factor(YEAR), y=def4soyMT5_ha/1000, fill=soyMtrader_shareClass))+ 
  geom_bar(  stat = 'identity', position='fill', alpha=0.9, width = 0.7)+
  scale_fill_brewer(palette = "Set2", "ZDC %")+
  scale_x_discrete(labels=c( (2011:2015), paste0(2016:2018, "*")))+
  theme_minimal()+
  labs(title= "Amazon: Soy deforestation", x=NULL, y="")+
  theme (legend.position = "bottom",
         panel.grid.major.y = element_line(color="grey80"),
         panel.grid.minor.y = element_line(color="grey80"))

def_ggA <- ggarrange(def_allA, def_fraca, ncol=2, nrow=1, align='hv', labels = "auto")
ggsave (filename = file.path(dir_plot, "def_amazon.png"),plot=def_ggA, width = 12, height = 4 )



##  Cerrado -----
trase25g_C10y <-  trase25g_C %>% filter (YEAR>2010 & YEAR<2019) %>% 
  mutate (GZDCtrader_shareClass= case_when(soy_ha ==0 ~ "No soy",
                                           GZDCtrader_share==0| is.na(GZDCtrader_share) ~"0",
                                           GZDCtrader_share>0 & GZDCtrader_share<=25~ paste0(">0","\U2264", "25"),
                                           GZDCtrader_share>25 & GZDCtrader_share<=50~ paste0(">25","\U2264", "50"),
                                           GZDCtrader_share>50 & GZDCtrader_share<=75~ paste0(">50","\U2264", "75"),
                                           GZDCtrader_share>75 & GZDCtrader_share<=100~ paste0(">75","\U2264", "100"))) %>% 
  mutate(GZDCtrader_shareClass=factor(GZDCtrader_shareClass, 
                                      levels=c("No soy","0",paste0(">0","\U2264", "25"), paste0(">25","\U2264", "50"),paste0(">50","\U2264", "75"),paste0(">75","\U2264", "100")))) %>% 
  group_by(GZDCtrader_shareClass, YEAR) %>% summarise(def4soyMT5_ha=sum(def4soyMT5_ha),
                                                      defMT = sum(defMT),
                                                      GZDCtrader_shareClass= last (GZDCtrader_shareClass),
                                                      suit_forest = last(GAEZ_apt_forest_area_ha))

totalsdd <- trase25g_C10y %>% group_by(GZDCtrader_shareClass)%>% summarise (def4soyMT5_ha = sum(def4soyMT5_ha))
totalsdd <- totalsdd %>% ungroup()%>% mutate(totalSDeF = sum(def4soyMT5_ha),
                                             percentSDeF = ((def4soyMT5_ha/totalSDeF)*100))
total_dd <- trase25g_C10y %>% group_by(YEAR) %>% summarise(def4soyMT5_ha= sum(def4soyMT5_ha),
                                                           defMT = sum(defMT)) %>% 
  mutate (def4soy_per = (def4soyMT5_ha/defMT)*100)
total_dd
trase25g_C10y %>% filter (YEAR==2015) %>% group_by(GZDCtrader_shareClass) %>% summarise(def = round (sum(defMT)/1000,0),
                                                                                        sdef = round(sum(def4soyMT5_ha)/1000,0)) %>% 
  mutate(perc_sdef= round((sdef/sum(sdef))*100, 2),
         perc_def = round((def/sum(def))*100, 2),
         sum_sdef = sum(sdef),
         sum_def= sum(def))
# a high in 2012
trase25g_C10y %>% filter (YEAR==2012) %>% group_by(GZDCtrader_shareClass) %>% summarise(def = round (sum(defMT)/1000,0),
                                                                                        sdef = round(sum(def4soyMT5_ha)/1000,0)) %>% 
  mutate(perc_sdef= round((sdef/sum(sdef))*100, 2),
         perc_def = round((def/sum(def))*100, 2))



# substract soy def from overall def
def_c <- trase25g_C10y %>% mutate (def = defMT-def4soyMT5_ha) %>%  
  pivot_longer(cols= c(def4soyMT5_ha, def), values_to = "ha", names_to = "Deforestation") %>% 
  group_by(Deforestation, YEAR) %>%summarise(ha=sum(ha)) 

def_allC<-ggplot()+ 
  geom_bar( data=def_c, aes(x=as.factor(YEAR), y=ha/1000, fill=Deforestation), stat = 'identity', position = 'stack', width = 0.7, alpha=0.9)+
  scale_fill_brewer(palette = "Set2",name=NULL, label=c("Deforestation for non-soy", "Deforestation for soy"))+
  geom_text(data=total_dd, aes(x= as.factor(YEAR), y= (defMT/1000)+30),
            label=paste0(format (round(total_dd$defMT/1000, -1), big.mark=","),"kha"),
            size=3, show.legend = FALSE)+
  geom_text(data=total_dd, aes(x= as.factor(YEAR), y=(def4soyMT5_ha/1000)+70),
            label=paste0(format (round(total_dd$def4soyMT5_ha/1000, -1), big.mark=","),"kha\n (", round(total_dd$def4soy_per, 0),"%)"),
            size=3, show.legend = FALSE)+
  scale_x_discrete(labels=c( (2011:2015), paste0(2016:2018, "*")))+
  theme_minimal()+
  labs(title= "Cerrado: Deforestation", x=NULL, y="kha")+
  theme (legend.position = "bottom", 
         panel.grid.major.y = element_line(color="grey80"),
         panel.grid.minor.y = element_line(color="grey80"))


def_fracC <- 
  ggplot(trase25g_C10y, aes(x=as.factor(YEAR), y=def4soyMT5_ha/1000, fill=GZDCtrader_shareClass))+ 
  geom_bar(  stat = 'identity', position='fill', alpha=0.9, width = 0.7)+
  scale_fill_brewer(palette = "Set2", "ZDC %")+
  scale_x_discrete(labels=c( (2011:2015), paste0(2016:2018, "*")))+
  theme_minimal()+
  labs(title= "Cerrado: Soy deforestation", x=NULL, y="")+
  theme (legend.position = "bottom",
         panel.grid.major.y = element_line(color="grey80"),
         panel.grid.minor.y = element_line(color="grey80"))

def_ggC <- ggarrange(def_allC, def_fracC, ncol=2, nrow=1, align='hv', labels = "auto")
ggsave (filename = file.path(dir_plot, "def_cerrado.png"),plot=def_ggC, width = 12, height = 4 )



## combine figure
def_ggAC<- ggarrange(ggarrange (def_allC, def_allA, ncol=2, nrow=1, align='hv', labels = c("a","b"), common.legend = TRUE, legend="bottom"),
                     ggarrange(def_fracC,def_fraca, ncol=2, nrow=1, align='hv', labels = c("c","d"), common.legend = TRUE, legend="bottom"),
                     ncol=1,nrow=2, align='hv')
ggsave (filename = file.path(dir_plot, "def_all_cm.tiff"),plot=def_ggAC, width = 11, height = 11, units = 'cm', dpi=600, scale=2 , bg="white")

