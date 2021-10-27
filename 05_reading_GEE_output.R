#reading and combining mapbiomas v5 output from GEE.

library(tidyverse)

dir_in <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/GEE/MapBiomas_v5_raw"
dir_out <- "/Users/floriangollnow/Dropbox/ZDC_project/DATA/GEE/MapBiomas_v5"

files_in <- unique (str_sub (dir(dir_in, pattern=".csv$"), end=-7))
files_in[c(3,4,5,6)] # soy aptidute

#for (i in files_in){
for (i in files_in[c(3,4,5,6)]){  
  test <- read_csv(file.path(dir_in,paste0(i, "_1.csv"))) %>% length()# adaptive colltypes
  
  tab1 <- read_csv(file.path(dir_in,paste0(i, "_1.csv")), col_types = paste0("c", paste0(rep("d", times=test-1 ),collapse="")))
  
  tab2 <- read_csv(file.path(dir_in,paste0(i, "_2.csv")), col_types = paste0("c", paste0(rep("d", times=test-1 ),collapse="")))
  
  tab <- tab1 %>%  bind_rows(tab2)
  
  
  tab <- tab %>% group_by(CD_GEOCMU	) %>% summarise_all(sum, na.rm=TRUE)
  
  write_csv(tab , file.path(dir_out,paste0(i, ".csv")))
  write_rds(tab , file.path(dir_out,paste0(i, ".rds")))
}
