# prepare species richness data (RQ2)
# intesect species ranges with muncipalities in teh Amazon and Cerrado (resource intensive)
library(tidyverse)
library(sf)

dir_iucn <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/IUCN/all_threatened/"
dir_admin <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/Admin/IBGE/2015/br_municipios"
dir_biome <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/Biomes/Biomes"

iucn <- read_sf(file.path (dir_iucn, "data_0.shp"))# derived from https://www.iucnredlist.org/
biome <- read_sf (file.path(dir_biome, "Brazil_Biomes.shp"))

admin <- read_sf (file.path(dir_admin, "BRMUE250GC_all_biomes_WGS84.shp"))

admin<- admin %>% filter(Bim_lAr=="Cerrado"| Bim_lAr=="Amazônia")
admin <- admin %>% group_by(CD_GEOC, Bim_lAr) %>% summarize()

iucn_present <- iucn %>% filter(PRESENCE  ==1)

# how many species per biome
biome_iucn <- biome %>% st_intersection(iucn_present)
biome_iucn %>% filter(name=="Cerrado") %>% distinct(BINOMIAL) #%>% nrow()
biome_iucn %>% filter(name=="Amazônia") %>% distinct(BINOMIAL) %>% nrow()

# intersect municipalities and species ranges
all(st_is_valid(iucn_present))
admin_u_cerrado <- admin %>% filter(Bim_lAr=="Cerrado") 
admin_u_amazonia <- admin %>% filter(Bim_lAr=="Amazônia")
iucn_present_c <- iucn_present
iucn_present_a <- iucn_present
cerrado_iucn <- st_intersection(iucn_present_c, admin_u_cerrado)
all(st_is_valid(cerrado_iucn))
cerrado_iucn_v <- cerrado_iucn
cerrado_iucn_v <- cerrado_iucn_v %>% mutate(ID1= paste(CD_GEOC, as.character(ID_NO),sep="_"), ID2= paste(CD_GEOC, BINOMIAL, sep="_"))
write_sf(cerrado_iucn_v, file.path(dir_iucn, "cerrado_iucn2.shp"))
amazon_iucn <- st_intersection(iucn_present_a, admin_u_amazonia)
all(st_is_valid(amazon_iucn))
amazon_iucn_v <- st_make_valid(amazon_iucn)
amazon_iucn_v <- amazon_iucn_v %>% mutate(ID1= paste(CD_GEOC, as.character(ID_NO),sep="_"), ID2= paste(CD_GEOC, BINOMIAL, sep="_"))

write_sf(amazon_iucn_v, file.path(dir_iucn, "amazon_iucn2.shp"))

