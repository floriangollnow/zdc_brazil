## study area maps
## maps soy deforestation 2000-2010-2019 and forest 2019

library(raster)
library(sf)
library(tidyverse)
library(rmapshaper)
library(nngeo)
library(ggpubr)
library(RColorBrewer)
library(units)
dir_plot <- "/Users/floriangollnow/Dropbox/ZDC_project/Draft/plot"

#raster_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/GEE/MapBiomas_v5_maps"
raster_dir <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/GEE/MapBiomas_v6_maps"
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
#//1 Forest, 2 Suitable Forest, 3 Pasture, 4Crop,8 grassland, 9 water, 5 soy , 6 soy-def 00-10,7 soy-def 11-18 
defS <- main_lu %>% reclassify( rcl=matrix(c(0,0, 1,0, 2,0, 3,0, 4,0, 5,0, 6,1, 7,2, 8,0, 9,0),byrow = T, ncol = 2 ))

boundary <- as_Spatial(cerrado)
main_lu<- aggregate(main_lu,2,fun=modal, na.rm=TRUE)
defS <- aggregate(defS,5,fun=max, na.rm=TRUE)

defS <- resample(defS, main_lu, method='ngb')

main_lum <- mask(main_lu, boundary)
defS_m <- mask(defS, boundary)

main_lu <-main_lum

defS <- defS_m

fq1 <- function(x, y) {#combine main LU and soy deforestation, highlighting soy-deforestation by larger pixel size
  z <- ifelse(y==1, 6, ifelse(y==2,7,x))}
main_lu <- raster::overlay(main_lu, defS, fun=fq1)

#prepare raster for ggplot use
main_lu.coord<- xyFromCell(main_lu, seq_len(ncell(main_lu)))
main_lu.v <- (getValues(main_lu))
main_lu.df <- as.data.frame(main_lu.v)
names(main_lu.df)<-"value"
# change 0 to NA her
main_lu.df[ which(main_lu.df$value==0),]<- NA 
unique(main_lu.df)
# use factors for LU 
main_lu.df$value <- factor(main_lu.df$value, levels = c("1","2","8","3","4","5","6","7","9"))
levels(main_lu.df$value)
main_lu.df1 <- cbind(main_lu.coord, main_lu.df)

## bounding box for plot extend (needed cause data has different extends)
bb <- st_bbox(cerrado)
# prepare state names for map
states_cropped <- st_crop(states, bb)
states_cropped <- states_cropped %>% filter (State_abb!="ES", State_abb!="RO")
colors4<- c(brewer.pal(8, 'Set2'), "#E41A1C","#F781BF", "#377EB8")[c(5,1,8,7,6,3,10,9,11)]#"#0c2c84"
#colors5 <- c(brewer.pal(9, 'Set1')[c(1:8)], "#129912")[c(3,9,8,7,6,4,5,1,2)]
print(brewer.pal(9, 'Set1'))
# plot
gg_luC <- ggplot() +
  geom_tile(data=main_lu.df1,  aes(x,y, fill = value)) +
  # scale_fill_manual(values = c("1"="#c59ff4", "2"="#d01c8b", "3"="#FFFFB2","4"="#7fbc41", "5"="#129912" ), na.value=NA,# c("1"="#c59ff4", "2"="#e66101")"#f1b6da"
  #                   name = "LUCC", labels = c("Soy", "Soy-\nDeforestation","Pasture" , "Forest", "Soy-suitable\nForest"), na.translate=FALSE)+
  
  scale_fill_manual(values = colors4, na.value=NA,# c("1"="#c59ff4", "2"="#e66101")
                    name = NULL,
                    labels = c("Forest","Soy-suitable\nforest","Grassland","Pasture", "Cropland","Soy", paste0("Soy-deforestation\n>2000\U2264", "2005"),paste0("Soy-deforestation\n",">2005\U2264", "2019"), "Water"), 
                    na.translate=FALSE, drop=FALSE)+
  geom_sf(data=states,color = "black", fill = NA, size=0.5, lty="longdash")+
  geom_sf(data=MatoP, color="white",fill = NA, size=1.6)+
  geom_sf(data=MatoP, aes(lty=Matopiba), color="#FF6700",fill = NA, size=1.2)+#"#7570b3"#5e3c99
  #geom_sf(data=MatoP, aes(lty=Matopiba), color="#a65628" ,fill = NA, size=1)+#"#7570b3"##5e3c99""#dd1c77"
  geom_sf_label(data=states_cropped, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,7),+1, rep(0,6)), nudge_x = c(rep(0,7),0, rep(0,6)), color="black", fill="white", alpha=0.5)+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  guides (fill=guide_legend(order=1),
          #pattern=guide_legend(order=2),
          color= guide_legend(order=3))+
  theme(legend.position = "bottom", 
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.key.size= unit(1.5,'cm'),
        axis.title.x=element_blank(),axis.title.y=element_blank())

#gg_luC
ggsave(file.path(dir_plot, "TEST_LU_Def_v7.png"), plot=gg_luC, width = 21/2 ,height = 7)
legendgg <- get_legend(gg_luC)

####################
# amazon LU ---------
states_amazon <- st_intersection( states, amazon)

main_lu <- raster(file.path(raster_dir,"mapbiomas_FFSS_GAEZapt_Amazon_1000_v5.tif"))
defS <- main_lu %>% reclassify( rcl=matrix(c(0,0, 1,0, 2,0, 3,0, 4,0, 5,0, 6,1, 7,2, 8,0, 9,0),byrow = T, ncol = 2 ))

boundary <- as_Spatial(amazon)
main_lu<- aggregate(main_lu,2,fun=modal, na.rm=TRUE)
defS <- aggregate(defS,5,fun=max, na.rm=TRUE)
defS <- resample(defS, main_lu, method='ngb')
main_lum <- mask(main_lu, boundary)
defS_m <- mask(defS, boundary)

main_lu <-main_lum
defS <- defS_m
fq1 <- function(x, y) {#combine main LU and soy deforestation, highlighting soy-deforestation by larger pixel size
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
                    labels = c("Non-suit forest","Soy-suitable\nforest","Grassland","Pasture", "Cropland","Soy", paste0("Soy-deforestation\n>2000\U2264", "2010"),paste0("Soy-deforestation\n",">2010\U2264", "2019"), "Water"), na.translate=FALSE)+
  geom_sf(data=states,color = "black", fill = "NA", size=0.5, lty="longdash")+
  geom_sf_label(data=states_cropped, aes(label = State_abb),fun.geometry = st_point_on_surface, nudge_y =  c(rep(0,8),-1.5, rep(0,5)), nudge_x = c(rep(0,8),-0.5, rep(0,5)), color="black", fill="white", alpha=0.5)+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), expand = FALSE)+
  theme_bw()+
  theme(legend.position = "right", axis.title.x=element_blank(),axis.title.y=element_blank())

#gg_luA
ggsave(file.path(dir_plot, "TEST_Amazon_LU_Def_v7.png"), plot=gg_luA, width = 21/2 ,height = 7)

gg_maps <- ggarrange(gg_luA, gg_luC, ncol=2, nrow=1, labels="auto", align = "v",
                     common.legend = TRUE,
                     legend.grob=legendgg ,
                     widths=c(2,1.20),
                     legend="bottom",
                     label.x = 0,
                     label.y = 1
)

ggsave(file.path(dir_plot, "LU_Def_v7.png"), plot=gg_maps, width = 11, height = 6 , units = "cm", dpi=600, scale=2, bg="white")
ggsave(file.path(dir_plot, "LU_Def_4_v7.png"), plot=gg_maps, width = 11, height = 6 , units = "cm", dpi=600, scale=3, bg="white")

