# prepare species richness data

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
biome_iucn <- biome %>% st_intersects(iucn_present,sparse = FALSE)
biome_iucn_SR <- apply(biome_iucn, 1, sum, na.rm=TRUE)


# within each municipality
muni_iucn_s <- admin %>% st_intersects(iucn_present,sparse = FALSE)


muni_iucn_SR <- apply(muni_iucn_s, 1, sum, na.rm=TRUE)# count the number of species per munis
muni_iucn_SR <- tibble (sr=muni_iucn_SR)
admin_sr <- admin %>% bind_cols(muni_iucn_SR)

write_sf(admin_sr, file.path(dir_iucn, "muni_sr.shp"))
#ggplot(admin_sr) +geom_sf(aes(fill=sr))

## to tibble
admin_sr.tb <- as_tibble(admin_sr) %>% select (-geometry)
write_csv(admin_sr.tb, file.path (dir_iucn, "all_tvf_sr_muni.csv"))
write_rds(admin_sr.tb, file.path (dir_iucn, "all_tvf_sr_muni.rds"))
