# read and prepare CNPJ data for RQ1
# this one ran on the server, due to large file size of the cnpj data derived from https://rdrr.io/github/georgevbsantiago/qsacnpj/f/README.Rmd 

library (tidyverse)
library(data.table)
library(rslurm)
in_dir <- "/nfs/gollnow-data/cpnj/duplicate"
dir.trase_cnpj <- "/nfs/gollnow-data/TraseData2015/bulk_download2/BRAZIL_SOY_2.4/Brazil_Soy_242_cnpj/Update_EzE"

#reads all cnpj data (10GB file) and reduces it to the main cnpj 
## cnpj data derived from: v0.1.9 https://rdrr.io/github/georgevbsantiago/qsacnpj/f/README.Rmd


in_list <- data.frame(x=file.path  (in_dir,"cnpj_dados_cadastrais_pj.csv"), y= file.path(dir.trase_cnpj, "trase_v24.csv"))
tab_f <- function (x, y){
  x = fread(input = x, sep="#")#, nrows=1000)
  #x <-  read_delim(x ,delim="#",col_types = "ccccccccccccccccccccccccccccccdccccccccc")
  y <- read_delim(y , col_types = "cccccccccccdddcd",delim = ";", locale = locale(decimal_mark="."))
  
  x_nr <- nrow(x)
  write_csv (as.data.frame(x_nr), file.path("/nfs/gollnow-data/cpnj/duplicate/tmp", "n_rows.csv") )# just checking that all goes fine
  
  x_nr_c <- 1:x_nr
  x_nr_cut  <- cut(x_nr_c, 100, labels=FALSE)
  list_out <- list()
  for (i in 1:max(x_nr_cut)){
    x1 <- x[which(x_nr_cut==i),]
    
    x1 <- x1 %>%  mutate (main_cnpj=substring(cnpj, first=1, last=8),
                          cnae_div=substring(cnae_fiscal, first=1, last=2),
                          cnae_grupo= substring(cnae_fiscal, first=1, last=3),
                          cnae_class= substring(cnae_fiscal, first=1, last=5),
                          cnae_cod = substring(cnae_fiscal, first=1, last=7))
    write_rds (x1, file.path("/nfs/gollnow-data/cpnj/duplicate/tmp", paste0("cnpj_x_",i,".rds")))
    
    x_group1 <- x1 %>% group_by(main_cnpj) %>% summarise (name =  first(razao_social),
                                                          n_names = n_distinct(razao_social),
                                                          n_name_f = first (nome_fantasia),
                                                          cod_pais = first (cod_pais),
                                                          nm_pais = first (nm_pais),
                                                          cnae_div= first(cnae_div),
                                                          cnae_grupo= first(cnae_grupo),
                                                          cnae_class = first(cnae_class),
                                                          cnae_cod = first(cnae_cod),
                                                          cnae_fiscal = first(cnae_fiscal),
                                                          porte_empresa = max(porte_empresa, na.rm=TRUE),
                                                          capitalS_1 = first (capital_social_empresa),
                                                          capital_min = min(capital_social_empresa, na.rm=TRUE),
                                                          capital_max = max(capital_social_empresa, na.rm=TRUE),
                                                          capital_sum = sum (capital_social_empresa, na.rm=TRUE))
    write_rds (x_group1, file.path("/nfs/gollnow-data/cpnj/duplicate/tmp", paste0("cnpj_x_group_",i,".rds")))
    
    #assign(paste0 ("x_group1_", i), x_group1)
    list_out[[i]]<- x_group1
  }
  x_group <- bind_rows(list_out)
  rm(list_out)
  
  y_unique_cnpj <- tibble (trase_cnpj = unique(y$CNPJ))
  y_unique_cnpj <- y_unique_cnpj %>%  mutate (main_cnpj=substring(trase_cnpj, first=1, last=8))
  
  join_cnpj <- y_unique_cnpj %>% left_join(x_group, by="main_cnpj")
  write_rds(x_group, file.path("/nfs/gollnow-data/cpnj/duplicate/tmp", "CNPJ_group_slurm_7.rds"))
  write_rds(join_cnpj, file.path("/nfs/gollnow-data/cpnj/duplicate", "Trase_CNPJ_group_join_slurm_7.rds"))
}


# run on R slurm cluster
sjob <- slurm_call(tab_f, jobname = 'cnpj7',
                   list(x=in_list$x, y=in_list$y), submit = TRUE)


