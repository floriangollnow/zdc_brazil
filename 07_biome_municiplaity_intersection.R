# intersecting Municipality 2015 shape with BIOMEs:
# a) assigne municipalityes based on largest area of intersection
# b) assigne municipalities to unique and combined biomes, for example Amazon (unique match) or Amazon-Cerrado for two intersections 

library (tidyverse)
# intersecting municipalities and biomes to set and define biome memberships

library(sf)
library (units)
library(lwgeom)

admin.dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/Admin/IBGE/2015/br_municipios"
biome.dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/Biomes/Biomes"

stateCode <- read_csv(file.path  (admin.dir,"States_Code.txt"), col_types = "ccc") ## State code with munis
mun <- read_sf(file.path  (admin.dir, "BRMUE250GC_WGS84.shp"))
bio <- read_sf(file.path(biome.dir, "Brazil_Biomes.shp"))

mun_st <- mun %>% mutate (StCode = substr(CD_GEOCMU, 1,2)) %>%  
  left_join(stateCode, by =c("StCode"="Geocode")) %>%
  rename (State = Name, State_abb = Abbreviation)


mun_st_int <- mun_st %>%  sf::st_intersection (bio)# biome intersection
mun_st_int <- mun_st_int %>%  rename(Biome = name, Biome_id = objectid) %>% select (-c(objectid_1,shape_leng, st_areasha,st_lengths))

#add area of intersctions
mun_st_int <- mun_st_int %>%  mutate (Area_I=set_units(st_area(mun_st_int), ha))
# convert to tiblle
mun_st_int_tibble <- as_tibble (mun_st_int) %>% select( "CD_GEOCMU" ,"Biome_id", "Biome", "State", "State_abb","Area_I")
# and remove double associations -> largest area with group by and filter
mun_st_int_tibble_u <- mun_st_int_tibble %>% group_by (CD_GEOCMU) %>% filter(Area_I==max(Area_I))#keeps the rows with max Area

# area of munis
mun_area <- mun %>%  mutate (Area_ha= set_units(st_area(mun), ha)) 
muni_Biome_larea <- mun_area %>% left_join (mun_st_int_tibble_u, by="CD_GEOCMU")# join intersections to muni file
##out
muni_Biome_larea

# and combine double association
# use tibble from above line 30


mun_st_int_tibble_all_u <- mun_st_int_tibble %>% group_by (CD_GEOCMU) %>% summarise(Biome_1= nth (Biome,1, order_by = -1* Area_I), # largest intersction
                                                                                    Biome_2= nth (Biome,2, order_by = -1* Area_I), # second larges intersection
                                                                                    Biome_3= nth (Biome,3, order_by = -1* Area_I), # third largest intersection
                                                                                    Biome_n = n_distinct(Biome)) # number of intersection
mun_st_int_tibble_all_u <- mun_st_int_tibble_all_u %>% unite ("Biome_c",Biome_1, Biome_2, Biome_3, remove = FALSE) %>%  mutate (Biome_c= gsub("_NA","", Biome_c)) %>% mutate (Biome_c= gsub("_NA","", Biome_c))
unique(mun_st_int_tibble_all_u$Biome_c)

muni_Biome_all_comb <- mun_area %>% left_join (mun_st_int_tibble_all_u, by="CD_GEOCMU")

#######################################################################################################################################
#combine  the different intersections by municipality

muni_Biome_all <- mun_area %>% left_join (mun_st_int_tibble_u, by="CD_GEOCMU") %>% 
  rename (Biome_lArea = Biome) %>% 
  left_join (mun_st_int_tibble_all_u, by="CD_GEOCMU")
#write_sf (muni_Biome_all , file.path(admin.dir,"BRMUE250GC_all_biomes_WGS84.shp"))
#write_rds (muni_Biome_all , file.path(admin.dir,"BRMUE250GC_all_biomes_WGS84.rds"))
write_rds (muni_Biome_all , file.path("Data","BRMUE250GC_all_biomes_WGS84.rds"))
