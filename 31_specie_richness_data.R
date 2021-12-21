# prepare species richness data (RQ2)

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



# within each municipality
# muni_iucn_s <- admin %>% st_intersects(iucn_present,sparse = FALSE)
# muni_iucn_SR <- apply(muni_iucn_s, 1, sum, na.rm=TRUE)# count the number of species per munis
# muni_iucn_SR <- tibble (sr=muni_iucn_SR)
# admin_sr <- admin %>% bind_cols(muni_iucn_SR)

# write_sf(admin_sr, file.path(dir_iucn, "muni_sr.shp"))
#ggplot(admin_sr) +geom_sf(aes(fill=sr))

# intersect with munis biome
# generate biome boundaries from munis 
all(st_is_valid(iucn_present))
admin_u_cerrado <- admin %>% filter(Bim_lAr=="Cerrado") #%>% st_union()
admin_u_amazonia <- admin %>% filter(Bim_lAr=="Amazônia")# %>% st_union()
iucn_present_c <- iucn_present# st_crop(iucn_present, admin_u_cerrado)
iucn_present_a <- iucn_present#st_crop(iucn_present, admin_u_amazonia)
cerrado_iucn <- st_intersection(iucn_present_c, admin_u_cerrado)
all(st_is_valid(cerrado_iucn))
cerrado_iucn_v <- cerrado_iucn#st_make_valid(cerrado_iucn)
cerrado_iucn_v <- cerrado_iucn_v %>% mutate(ID1= paste(CD_GEOC, as.character(ID_NO),sep="_"), ID2= paste(CD_GEOC, BINOMIAL, sep="_"))
write_sf(cerrado_iucn_v, file.path(dir_iucn, "cerrado_iucn2.shp"))
amazon_iucn <- st_intersection(iucn_present_a, admin_u_amazonia)
all(st_is_valid(amazon_iucn))
amazon_iucn_v <- st_make_valid(amazon_iucn)
amazon_iucn_v <- amazon_iucn_v %>% mutate(ID1= paste(CD_GEOC, as.character(ID_NO),sep="_"), ID2= paste(CD_GEOC, BINOMIAL, sep="_"))
#write_sf(cerrado_iucn, file.path(dir_iucn, "cerrado_iucn.shp"))
write_sf(amazon_iucn_v, file.path(dir_iucn, "amazon_iucn2.shp"))


#############
## to tibble
# admin_sr.tb <- as_tibble(admin_sr) %>% select (-geometry)
# write_csv(admin_sr.tb, file.path (dir_iucn, "all_tvf_sr_muni.csv"))
# write_rds(admin_sr.tb, file.path (dir_iucn, "all_tvf_sr_muni.rds"))
# write_rds(admin_sr.tb, file.path ("Data", "all_tvf_sr_muni.rds"))