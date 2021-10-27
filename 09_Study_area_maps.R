## study area maps
## maps soy def2000-2010-2019 and forest2019

library(raster)
library(sf)
library(tidyverse)
library(rmapshaper)
library(nngeo)
library(ggpubr)
library(RColorBrewer)
dir_plot <- "/Users/floriangollnow/Dropbox/ZDC_project/Draft/plot"

raster_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/GEE/MapBiomas_v5_maps"
biomes <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/Biomes/Biomes"
states_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/Admin/IBGE"
munis_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/Admin/IBGE/2015/br_municipios"
out <- '/Users/floriangollnow/Dropbox/ZDC_project/Draft/plot'
#spatial
cerrado <- read_sf (file.path(biomes, "Brazil_cerrado.shp"))
amazon <- read_sf (file.path(biomes, "Brazil_amazon.shp"))
munis <- read_sf (file.path(munis_dir, "BRMUE250GC_all_biomes_WGS84.shp"))
states <- read_rds (file.path(states_dir,"StatesBR_WGS84.rds"))

munis <- munis %>% ms_simplify()
states <- states %>% ms_simplify()

# mapopiba 
states_cerrado <- st_intersection( states, cerrado)
MatoP <- states_cerrado %>% filter(name=="Cerrado" & (State_abb=="MA"|State_abb=="TO"|State_abb=="PI"|State_abb=="BA"))
MatoP<- MatoP %>% group_by(name) %>% summarise(Matopiba=first(name)) %>% st_cast()  %>% mutate (Matopiba="")
#ggplot()+geom_sf(data=MatoP)

##########
# main_lu ----------
# Cerrad0 ----------
main_lu <- raster(file.path(raster_dir,"mapbiomas_FFSS_GAEZapt_Cerrado_1000_v5.tif"))
defS <- raster(file.path(raster_dir,"mapbiomas_SDEF_Cerrado_1000_v5.tif"))
boundary <- as_Spatial(cerrado)
main_lu<- aggregate(main_lu,2,fun=modal, na.rm=TRUE)
defS <- aggregate(defS,6,fun=max, na.rm=TRUE)

defS <- resample(defS, main_lu, method='ngb')

main_lum <- mask(main_lu, boundary)
defS_m <- mask(defS, boundary)

main_lu <-main_lum
defS <- defS_m
fq1 <- function(x, y) {#combine main LU and soy deforestaion, highlighting soy-deforestation by larger pixel size
  z <- ifelse(y==1, 6, ifelse(y==2,7,x))}
main_lu <- overlay(main_lu, defS, fun=fq1)


#prepare raster for ggplot use
main_lu.coord<- xyFromCell(main_lu, seq_len(ncell(main_lu)))
main_lu.v <- (getValues(main_lu))
main_lu.df <- as.data.frame(main_lu.v)
names(main_lu.df)<-"value"
# change 0 to NA her
main_lu.df[ which(main_lu.df$value==0),]<- NA 
unique(main_lu.df)
#main_lu.df$value <- factor(main_lu.df$value, levels = c("1","2","3","4","5","0"))
# use factors for LU 
main_lu.df$value <- factor(main_lu.df$value, levels = c("1","2","8","3","4","5","6","7","9"))
levels(main_lu.df$value)
main_lu.df1 <- cbind(main_lu.coord, main_lu.df)

## bounding box for plot extend (needed cause data has different extends)
bb <- st_bbox(cerrado)
# prepare state names for map
states_cropped <- st_crop(states, bb)
states_cropped <- states_cropped %>% filter (State_abb!="ES", State_abb!="RO")
colors4<- c(brewer.pal(8, 'Set2'), "#0c2c84")[c(5,1,8,7,6,3,2,4,9)]

# plot
gg_luC <- ggplot() +
  geom_tile(data=main_lu.df1,  aes(x,y, fill = value)) +
  scale_fill_manual(values = colors4, na.value=NA,# c("1"="#c59ff4", "2"="#e66101")
                    name = "LULCC",
                    labels = c("Non-suit forest","Soy-suit forest","Grassland","Pasture", "Cropland","Soy", paste0("Soy-deforestation\n>2000\U2264", "2010"),paste0("Soy-deforestation\n",">2010\U2264", "2019"), "Water"), 
                    na.translate=FALSE, drop=FALSE)+
  geom_sf(data=states,color = "black", fill = NA, size=0.5, lty="longdash")+
geom_sf(data=MatoP, color="white",fill = NA, size=1.4)+
  geom_sf(data=MatoP, aes(lty=Matopiba), color="#a65628" ,fill = NA, size=1)+#"#7570b3"##5e3c99""#dd1c77"
  geom_sf_label(data=states_cropped, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,7),+1, rep(0,6)), nudge_x = c(rep(0,7),0, rep(0,6)), color="black", fill="white", alpha=0.5)+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  guides (fill=guide_legend(order=1),
          pattern=guide_legend(order=2),
          color= guide_legend(order=3))+
  theme(legend.position = "bottom", axis.title.x=element_blank(),axis.title.y=element_blank())

#gg_luC
ggsave(file.path(dir_plot, "TEST_LU_Def.png"), plot=gg_luC, width = 21/2 ,height = 7)
legendgg <- get_legend(gg_luC)

####################
# amazon LU ---------
states_amazon <- st_intersection( states, amazon)

main_lu <- raster(file.path(raster_dir,"mapbiomas_FFSS_GAEZapt_Amazon_1000_v5.tif"))
defS <- raster(file.path(raster_dir,"mapbiomas_SDEF_Amazon_1000_v5.tif"))

boundary <- as_Spatial(amazon)
main_lu<- aggregate(main_lu,2,fun=modal, na.rm=TRUE)
defS <- aggregate(defS,6,fun=max, na.rm=TRUE)
defS <- resample(defS, main_lu, method='ngb')
main_lum <- mask(main_lu, boundary)
defS_m <- mask(defS, boundary)

main_lu <-main_lum
defS <- defS_m
fq1 <- function(x, y) {#combine main LU and soy deforestaion, highlighting soy-deforestation by larger pixel size
  z <- ifelse(y==1, 6, ifelse(y==2,7,x))}
main_lu <- overlay(main_lu, defS, fun=fq1)

#plot(soym)
main_lu.coord<- xyFromCell(main_lu, seq_len(ncell(main_lu)))
main_lu.v <- (getValues(main_lu))
main_lu.df <- as.data.frame(main_lu.v)
names(main_lu.df)<-"value"
unique(main_lu.df$value)
main_lu.df[ which(main_lu.df$value==0),]<- NA
main_lu.df$value <- factor(main_lu.df$value, levels =c("1","2","8","3","4","5","6","7","9"))
main_lu.df1 <- cbind(main_lu.coord, main_lu.df)

##crop state names
bb <- st_bbox(amazon)
states_cropped <- st_crop(states, bb)

gg_luA <- ggplot() +
  geom_tile(data=main_lu.df1,  aes(x,y, fill = value)) +
  scale_fill_manual(values = colors4, na.value=NA,# c("1"="#c59ff4", "2"="#e66101")
                    name = "LULCC",
                    labels = c("Non-suit forest","Soy-suit forest","Grassland","Pasture", "Cropland","Soy", paste0("Soy-deforestation\n>2000\U2264", "2010"),paste0("Soy-deforestation\n",">2010\U2264", "2019"), "Water"), na.translate=FALSE)+
  geom_sf(data=states,color = "black", fill = "NA", size=0.5, lty="longdash")+
geom_sf_label(data=states_cropped, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,8),-1.5, rep(0,5)), nudge_x = c(rep(0,8),-0.5, rep(0,5)), color="black", fill="white", alpha=0.5)+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  theme(legend.position = "right", axis.title.x=element_blank(),axis.title.y=element_blank())

#gg_luA
ggsave(file.path(dir_plot, "TEST_Amazon_LU_Def.png"), plot=gg_luA, width = 21/2 ,height = 7)

gg_maps <- ggarrange(gg_luA, gg_luC, ncol=2, nrow=1, labels="auto", align = "v",
                     common.legend = TRUE,
                     legend.grob=legendgg ,
                     widths=c(2,1.20),
                     legend="bottom",
                     label.x = 0,
                     label.y = 1
)

ggsave(file.path(dir_plot, "LU_Def.png"), plot=gg_maps, width = 11, height = 6 , units = "cm", dpi=600, scale=2, bg="white")
ggsave(file.path(dir_plot, "LU_Def_4.png"), plot=gg_maps, width = 11, height = 6 , units = "cm", dpi=600, scale=3, bg="white")


########################
### soy def only: ------
# Cerrado soy def -----
raster_dir2<- "/Users/floriangollnow/Dropbox/ZDC_project/PaperRachael/Data/Mapbiomas"
main_lu <- raster(file.path(raster_dir2,"mapbiomas_SoyDeforestation_1000_v5.tif"))

main_lu<- aggregate(main_lu,5,fun=max)
boundary <- as_Spatial(cerrado)
main_lum <- mask(main_lu, boundary)

main_lu <-main_lum

main_lu.coord<- xyFromCell(main_lu, seq_len(ncell(main_lu)))
main_lu.df <- as.data.frame(getValues(main_lu))
names(main_lu.df)<-"value"
main_lu.df$value <- factor(main_lu.df$value, levels = c("1","2"))
main_lu.df1 <- cbind(main_lu.coord, main_lu.df)

##crop state names
bb <- st_bbox(cerrado)
states_cropped <- st_crop(states, bb)
states_cropped <- states_cropped %>% filter (State_abb!="ES", State_abb!="RO")

gg_lu <- ggplot() +
  geom_sf (data=cerrado, fill="grey90", color=NA)+
  geom_tile(data=main_lu.df1,  aes(x,y, fill = value)) +
  scale_fill_manual(values = c("1"="#c59ff4", "2"="#ca0020"), na.value=NA,# c("1"="#c59ff4", "2"="#e66101")
                    name = "LUCC", labels = c("Soy", "Soy-\nDeforestation"), na.translate=FALSE)+
  geom_sf(data=states,color = "black", fill = NA, size=0.5, lty="longdash")+
  geom_sf(data=MatoP, color="white", fill = NA, size=1.4)+
  geom_sf(data=MatoP, aes(lty=Matopiba), color="#a65628",fill = NA, size=1)+#"#7570b3"#5e3c99
  geom_sf_label(data=states_cropped, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,7),+1, rep(0,6)), nudge_x = c(rep(0,7),0, rep(0,6)), color="black", fill="white", alpha=0.5)+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  guides (fill=guide_legend(order=1))+
  theme(legend.position = "bottom", axis.title.x=element_blank(),axis.title.y=element_blank())# ,legend.box="vertical", legend.box="vertical",

#gg_lu

ggsave(file.path(dir_plot,"gg_soydef_test.png"))
legendgg <- get_legend(gg_lu)

#################
# amazon soy def ---------
main_lu <- raster(file.path(raster_dir2,"mapbiomas_SoyDeforestation2_1000_v5.tif"))

main_lu<- aggregate(main_lu,5,fun=max)
boundary <- as_Spatial(amazon)
main_lum <- mask(main_lu, boundary)

main_lu <-main_lum
#plot(soym)
main_lu.coord<- xyFromCell(main_lu, seq_len(ncell(main_lu)))
main_lu.df <- as.data.frame(getValues(main_lu))
names(main_lu.df)<-"value"
main_lu.df$value <- factor(main_lu.df$value, levels = c("1","2"))
main_lu.df1 <- cbind(main_lu.coord, main_lu.df)

##crop state names
bb <- st_bbox(amazon)
states_cropped <- st_crop(states, bb)

gg_lu2 <- ggplot() +
  geom_sf (data=amazon, fill="grey90", color=NA)+
  geom_tile(data=main_lu.df1,  aes(x,y, fill = value)) +
  scale_fill_manual(values = c("1"="#c59ff4", "2"="#ca0020"), na.value=NA,# c("1"="#c59ff4", "2"="#e66101")
                    name = "LUCC", labels = c("Soy", "Soy-\nDeforestation"), na.translate=FALSE)+
  geom_sf(data=states,color = "black", fill = NA, size=0.5, lty="longdash")+
  #geom_sf(data=pa_ind, aes (color=Designation ),alpha=0.5) +
  #scale_color_manual  (values=c("Indigenous"="#fdb863" , "PA"="#e66101"))+#"#fc8d62", "#e78ac3"
  # geom_sf(data=MatoP, color="white", fill = NA, size=1.4)+
  # geom_sf(data=MatoP, aes(lty=Matopiba), color="#a65628",fill = NA, size=1)+#"#7570b3"#5e3c99
  geom_sf_label(data=states_cropped, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,8),-1.5, rep(0,5)), nudge_x = c(rep(0,8),-0.5, rep(0,5)), color="black", fill="white", alpha=0.5)+
  # geom_sf(data=sf::st_point_on_surface(states_cropped),colour = "white",alpha=0.7, size = 10)+
  # geom_sf_text(data=states_cropped, aes(label = State_abb),fun.geometry = st_point_on_surface)+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  theme(legend.position = "right", axis.title.x=element_blank(),axis.title.y=element_blank())# ,legend.box="vertical", legend.box="vertical",
#gg_lu

#write_rds(gg_lu, file.path(,"ggplots","gg_soydef.rds"))
ggsave(file.path(dir_plot,"gg_soydef_test2.png"))

gg_def_maps <- ggarrange(gg_lu2, gg_lu, ncol=2, nrow=1, labels="auto", align = "v",
                         common.legend = TRUE,
                         legend.grob=legendgg ,
                         widths=c(2,1.20),
                         legend="bottom",
                         label.x = 0,
                         label.y = 1
)

ggsave(file.path(dir_plot, "Soy_Def.png"), plot=gg_def_maps, width = 11, height = 6 , units = "cm", dpi=600, scale=2, bg="white")#width = 21-6 ,height = 7)
ggsave(file.path(dir_plot, "Soy_Def_5.png"), plot=gg_def_maps, width = 11, height = 6 , units = "cm", dpi=600, scale=3, bg="white")#width = 21-6 ,height = 7)

