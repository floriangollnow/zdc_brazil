## data preparation for leakage assessment
# calculate adjacency matrix
# calculate adjacency average soy-deforestation and ZDC market share
# based on balanced panel and treatment coding (4.2)

library(tidyverse)
library (sf)
library(spdep)

out <-"/Users/floriangollnow/Dropbox/ZDC_project/DATA/TraseData2015/bulk_download2/BRAZIL_SOY_2.5.0_pc/Brazil_Soy_25_cnpj/"
# data table
MarketShareData_S05b <- read_rds( file.path(out, "SoyZDCs_05_v1.rds"))
MarketShareData_S05b <- MarketShareData_S05b %>% arrange(GEOCODE, YEAR)
# spatial data
shape <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/Admin/IBGE/2015/br_municipios"
muni <- read_rds(file.path(shape, "BRMUE250GC_all_biomes_WGS84.rds"))
#muni_A <- muni %>% filter(Biome_c=="Amazônia")
muni_A <- muni%>% select (CD_GEOCMU) %>% inner_join(MarketShareData_S05b %>% filter(YEAR==2005, balanced05=TRUE) , by = c("CD_GEOCMU"= "GEOCODE"))  
muni_A <- muni_A %>% arrange (CD_GEOCMU)
#plot(as_Spatial(muni_A))
#ggplot(muni_A)+geom_sf(aes(fill=Biome_lArea.x))
myneighbors <- poly2nb(muni_A)
str(myneighbors)

##remove municipalities without neighbors
sub_nb <- subset(myneighbors, subset=card(myneighbors) > 0)
str(sub_nb)
muni_A_sub  <- muni_A %>% subset(subset=card(myneighbors) > 0)
plot(as_Spatial(muni_A_sub))
weights <- nb2listw(sub_nb, style="W",  zero.policy=TRUE)

#add spatial lagged SoyM_Share for 0k
# remove munis without neighbors from tibble
id <- tibble (GEOCODE = muni_A_sub$CD_GEOCMU, sub=TRUE)
MarketShareData_S05b_sub <- MarketShareData_S05b %>% left_join(id) %>% filter(sub==TRUE) %>% ungroup()

MarketShareData_S05b_sub <- MarketShareData_S05b_sub%>% mutate (Neigh_soy_def=0,Neighbor_SoyMShare=0)

#check tha neighbors and dataset is in the same order
MarketShareData_S05b_sub$GEOCODE[ MarketShareData_S05b_sub$YEAR==2005]==muni_A_sub$CD_GEOCMU

# calculate neighborhood values from weights matrics (sums, because calculations are weighted by number of neighbors alreaduy. )
for(i in 1:length (MarketShareData_S05b_sub)){
  for (b in unique(MarketShareData_S05b_sub$YEAR)){
    MarketShareData_S05b_sub[ MarketShareData_S05b_sub$YEAR==b,  "Neigh_soy_def"][i,]<-
      colSums( MarketShareData_S05b_sub [MarketShareData_S05b_sub$YEAR==b,  "def4soyMT5_ha"][weights[["neighbours"]][[i]],]* weights[["weights"]][[i]])
    MarketShareData_S05b_sub[ MarketShareData_S05b_sub$YEAR==b,  "Neighbor_SoyMShare"][i,]<-
      colSums( MarketShareData_S05b_sub [MarketShareData_S05b_sub$YEAR==b,  "soyM_share"][weights[["neighbours"]][[i]],]* weights[["weights"]][[i]])
    
  }}

MarketShareData_S05b_sub %>% View()

write_csv(MarketShareData_S05b_sub, file.path(out, "NB_SoyZDCs_05_v1.csv"))
write_rds(MarketShareData_S05b_sub, file.path(out, "NB_SoyZDCs_05_v1.rds"))
