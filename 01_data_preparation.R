# Identifying ZDC companies
# combining trase and CNPJ data 

library(tidyverse)
library(pastecs)

# data directories
## trase data 
dir.trase <- '/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc'
dir.trase_cnpj <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/BRAZIL_SOY_2.4/Brazil_Soy_242_cnpj/Update_EzE"
## cnpj data 
dir.cnpj <-"/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_CNPJ"
## spatial data 
admin.dir <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/Admin/IBGE/2015/br_municipios"
## out directory
out <- "/Users/floriangollnow/Dropbox/ZDC_project/Data/TraseData2015/bulk_download2/Brazil_Soy_2.5.0_pc/Brazil_Soy_25_cnpj"

# read data
#get trase 2.5 data
trase25 <- read_delim(file.path(dir.trase, "BRAZIL_SOY_2.5.0_pc.csv"),delim = ",", locale = locale(decimal_mark="."))
# get trase cnpj data
trase.cpnj <- read_delim(file.path(dir.trase_cnpj, "trase_v24.csv"), col_types = "cccccccccccdddcd",delim = ";", locale = locale(decimal_mark="."))
# clean IBGE geo-code
trase25 <- trase25 %>%  mutate(GEOCODE = gsub ("BR-", "", TRASE_GEOCODE))

# consolidate Names of SoyM companies ( trase 2.5)
traseE.b <- tibble(T_EXPORTER = sort (unique (trase25$EXPORTER)), N_EXPORTER= NA_character_)
traseE.b <- traseE.b %>% mutate (N_EXPORTER= if_else(str_detect(T_EXPORTER, "ABC INDUS"), "ABC INDUSTRIA", N_EXPORTER),
                                 N_EXPORTER= if_else(grepl( "^ADM", T_EXPORTER), "ADM", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "A D M"), "ADM", N_EXPORTER), 
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "AGRIBRASIL"), "AGRIBRASIL", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "AGREX"), "AGREX", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "MAGGI"), "AMAGGI", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "BALDO"), "BALDO", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "BINATURAL"), "BINATURAL", N_EXPORTER), 
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "BUNGE"), "BUNGE", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "CARAMURU"), "CARAMURU", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "CARGILL"), "CARGILL", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "CHS"), "CHS", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "CJ INTERNATIONAL"), "CJ SELECTA", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "SELECTA"), "CJ SELECTA", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "COAMO"), "COAMO", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "COFCO"), "COFCO", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "CULTURALE"), "CULTURALE", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "DREYFUS"), "LOUIS DREYFUS", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "ENGELHART"), "ENGELHART", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "FIAGRIL"), "FIAGRIL", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "GAVILON"), "GAVILON", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "GLENCORE"), "GLENCORE", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "INVIVO"), "INVIVO", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "IMCOPA"), "IMCOPA", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "JBS"), "JBS", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "LDC"), "LOUIS DREYFUS", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "MARUBENI"), "MARUBENI", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "MULTIGRAIN"), "MULTIGRAIN", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "NIDERA"), "NIDERA", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "NOBLE"), "NOBLE", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "NOVA AGRI"), "NOVA AGRI", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "OLAM"), "OLAM", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "OLEOS MENU"), "OLEOS MENU", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "PERDUE"), "PERDUE", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "SEARA"), "SEARA", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "SODRUGESTVO"), "SODRUGESTVO", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "TIMBRO"), "TIMBRO", N_EXPORTER),
                                 N_EXPORTER= if_else(str_detect(T_EXPORTER, "ALIANCA AGRICOLA"), "SALIANCA AGRICOLA DO CERRADO", N_EXPORTER))
# combine consolidated names with original data
trase25_f <- trase25%>% left_join(traseE.b, by = c("EXPORTER"="T_EXPORTER"))                               
# add SoyM-ZDC to data
trase25_f  <- trase25_f  %>% mutate (SoyM_Y = case_when(!is.na(N_EXPORTER) & YEAR >= 2006 & BIOME=="AMAZONIA" ~ TRUE,
                                                        TRUE ~ FALSE),
                                     SoyM = case_when(!is.na(N_EXPORTER) ~ TRUE,
                                                      TRUE ~ FALSE))
# add missing names to consolidated names column
trase25_f <- trase25_f %>%  mutate(N_EXPORTER = if_else (is.na(N_EXPORTER), EXPORTER,
                                                         N_EXPORTER))

# add Global Commitments 
trase25_f<- trase25_f %>% mutate (N_EXPORTER= if_else(str_detect(EXPORTER, "DENOFA"), "DENOFA", N_EXPORTER),
                                  N_EXPORTER= if_else(str_detect(EXPORTER, "UNIL"), "UNILEVERS", N_EXPORTER))


trase25_f<- trase25_f %>%  mutate (G_ZDC_Y = case_when(N_EXPORTER=="DENOFA" & YEAR > 2014 ~ TRUE,
                                                       N_EXPORTER=="CARGILL" & YEAR > 2014 ~ TRUE,
                                                       N_EXPORTER=="ADM" & YEAR > 2015 ~ TRUE,
                                                       N_EXPORTER=="AMAGGI" & YEAR > 2017 ~ TRUE,
                                                       N_EXPORTER=="BUNGE" & YEAR > 2016 ~ TRUE,
                                                       N_EXPORTER== "LOUIS DREYFUS" & YEAR>2018 ~ TRUE,
                                                       N_EXPORTER=="COFCO" & YEAR> 2019  ~ TRUE,
                                                       N_EXPORTER=="GLENCORE" & YEAR>2019 ~ TRUE,
                                                       N_EXPORTER=="UNILEVERS" & YEAR>2010 ~ TRUE,
                                                       TRUE~FALSE))
trase25_f<- trase25_f %>%  mutate (G_ZDC = case_when(N_EXPORTER=="DENOFA"  ~ TRUE,
                                                     N_EXPORTER=="CARGILL"  ~ TRUE,
                                                     N_EXPORTER=="ADM" ~ TRUE,
                                                     N_EXPORTER=="AMAGGI"  ~ TRUE,
                                                     N_EXPORTER=="BUNGE"  ~ TRUE,
                                                     N_EXPORTER== "LOUIS DREYFUS"  ~ TRUE,
                                                     N_EXPORTER=="COFCO"   ~ TRUE,
                                                     N_EXPORTER=="GLENCORE"  ~ TRUE,
                                                     N_EXPORTER=="UNILEVERS"  ~ TRUE,
                                                     TRUE~FALSE))

# adding the CNPJ code 
## Consolidate names between trase 2.5 (1st) and trase 2.4 cnpj (2nd)
trase25_f <- trase25_f  %>% mutate (N_EXPORTER= if_else(str_detect(N_EXPORTER, "C.VALE"), "C. VALE", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "CGG"), "CGG", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGRO SOJA"), "AGRO SOJA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "ATLANTIS"), "ATLANTIS LOGISTICA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "MS PESCADOS"), "MS PESCADOS COMERCIO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROBOM"), "AGROBOM", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "MAEDA"), "MAEDA AGROINDUSTRIAL", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "BP BIOENERGIA"), "BP BIOENERGIA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "CERRADINHO"), "CERRADINHO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "CEAGRO"), "CEAGRO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROVENCI"), "AGROVENCI", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "BIOSEV"), "BIOSEV", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "LAR COOPERATIVA AGROINDUSTRIAL"), "LAR", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "CERVEJARIA PETROPOLIS"), "CERVEJARIA PETROPOLIS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "UGGERI"), "UGGERI", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "GALVANI"), "GALVANI", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROPECUARIA VALE DAS UVAS"), "AGROPECUARIA VALE DAS UVAS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "SEMENTES ESPERANCA"), "SEMENTES ESPERANCA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "USINA MONTE ALEGRE"), "USINA MONTE ALEGRE", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROVIGNA"), "AGROVIGNA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "SPERAFICO"), "SPERAFICO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "GAMA COMERCIO"), "GAMA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "FORCE ONE"), "FORCE ONE INDUSTRIA E COMERCIO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "UNGI CAFE"), "UNGI CAFE", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROREGIONAL"), "AGROREGIONAL", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "VITAGRI"), "VITAGRI", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "BELAGRICOLA"), "BELAGRICOLA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "FERRARI, ZAGATTO"), "FERRARI ZAGATTO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "OLEOPLAN"), "OLEOPLAN", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "BOCCHI"), "BOCCHI", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "SERRA MORENA"), "SERRA MORENA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "SUSTENTAGRO"), "SUSTENTAGRO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AFRICONTINENTAL"), "AFRICONTINENTAL", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "TRADEAGRO"), "TRADEAGRO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "BR BEAUTY"), "BR BEAUTY", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "FRONTIERS WTA"), "FRONTIERS WTA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "JARI CELULOSE"), "JARI CELULOSE PAPEL E EMBALAGENS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROFRUTAS"), "AGROFRUTAS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "ANIGER"), "ANIGER", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AXIS"), "AXIS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "INCEMA"), "INCEMA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "ORSA CELULOSE"), "ORSA CELULOSE PAPEL E EMBALAGENS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "PIMEX"), "PIMEX", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "Q.ALIMENTARE"), "Q.ALIMENTARE", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "SCOULAR BRASIL"), "SCOULAR BRASIL", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "SINA EXPORT"), "SINA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "USINA MONTE ALEGRE"), "USINA MONTE ALEGRE", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "VOTORANTIM"), "VOTORANTIM", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "ALDEKE"), "ALDEKE", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "EXPORSAN"), "EXPORSAN", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "BOMFIGLIO"), "BOMFIGLIO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "USACIGA"), "USACIGA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "VULCABRAS"), "VULCABRAS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "BRAFRIK"), "BRAFRIK", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "COUROTEX"), "COUROTEX", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "FENIX - COMERCIO"), "FENIX", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "HELA INGREDIENTES BRASIL"), "HELA INGREDIENTES BRASIL", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "JUMIL-JUSTINO DE MORAIS"), "JUMIL-JUSTINO DE MORAIS", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "L 3 COMERCIO"), "L 3 COMERCIO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "M3 COMERCIO"), "M3 COMERCIO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "TECNAL INDUSTRIA"), "TECNAL INDUSTRIA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "TECTOR ENGENHARIA"), "TECTOR ENGENHARIA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "VERA CRUZ"), "VERA CRUZ", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "VITALLY COMERCIO"), "VITALLY COMERCIO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "CTA INDUSTRIA"), "CTA INDUSTRIA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "LFG COMERCIO"), "LFG COMERCIO", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "READI-BR COMERCIAL"), "READI-BR COMERCIAL", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "TOTALE DISTRIBUIDORA"), "TOTALE DISTRIBUIDORA", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "AMAZONIA MIX"), "AMAZONIA MIX", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "GENEXPORT DO BRASIL"), "GENEXPORT DO BRASIL", N_EXPORTER),
                                    N_EXPORTER= if_else(str_detect(N_EXPORTER, "P.A.F. COMERCIAL"), "P.A.F. COMERCIAL", N_EXPORTER))
## consolidating names in trase 2.4 cnpj code
trase.cpnj <- trase.cpnj %>%  mutate (N_EXPORTER=TRADER)
trase.cpnj <- trase.cpnj %>% mutate (N_EXPORTER= if_else(str_detect(N_EXPORTER, "ABC INDUS"), "ABC INDUSTRIA", N_EXPORTER),
                                     N_EXPORTER= if_else(grepl( "^ADM", N_EXPORTER), "ADM", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "A D M"), "ADM", N_EXPORTER),# same # different CNPJ likely different company
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "MAGGI"), "AMAGGI", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BUNGE"), "BUNGE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "CARGILL"), "CARGILL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "DREYFUS"), "LOUIS DREYFUS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "LDC"), "LOUIS DREYFUS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "SEARA"), "SEARA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "FIAGRIL"), "FIAGRIL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "NIDERA"), "NIDERA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "NOBLE"), "NOBLE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "COFCO"), "COFCO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BALDO"), "BALDO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "IMCOPA"), "IMCOPA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGREX"), "AGREX", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "CHS"), "CHS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "COAMO"), "COAMO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "ENGELHART"), "ENGELHART", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "GAVILON"), "GAVILON", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "GLENCORE"), "GLENCORE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "INVIVO"), "INVIVO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "MARUBENI"), "MARUBENI", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "MULTIGRAIN"), "MULTIGRAIN", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "NOVA AGRI"), "NOVA AGRI", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "OLAM"), "OLAM", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "PERDUE"), "PERDUE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "SODRUGESTVO"), "SODRUGESTVO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "TIMBRO"), "TIMBRO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "SELECTA"), "CJ SELECTA", N_EXPORTER),##
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BINATURAL"), "BINATURAL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "JBS"), "JBS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "OLEOS MENU"), "OLEOS MENU", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGRIBRASIL"), "AGRIBRASIL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "CULTURALE"), "CULTURALE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "DENOFA"), "DENOFA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "UNIL"), "UNILEVERS", N_EXPORTER),
                                     
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "PMG AGRICOLA COMERCIAL LTDA"), "PMG TRADING", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGRENCO"), "AGRENCO DO BRASIL", N_EXPORTER),
                                     
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "MAEDA"), "MAEDA AGROINDUSTRIAL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "CGG"), "CGG", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGRO SOJA"), "AGRO SOJA", N_EXPORTER),
                                     #N_EXPORTER= if_else(str_detect(N_EXPORTER, "CEAGRO"), "CEAGRO AGRONEGOCIOS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "ATLANTIS"), "ATLANTIS LOGISTICA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "MS PESCADOS"), "MS PESCADOS COMERCIO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROBOM"), "AGROBOM", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BP BIOENERGIA"), "BP BIOENERGIA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "CERRADINHO"), "CERRADINHO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROVENCI"), "AGROVENCI", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "GALVANI"), "GALVANI", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGRENCO"), "AGRENCO DO BRASIL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "SEMENTES ESPERANCA"), "SEMENTES ESPERANCA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROVIGNA"), "AGROVIGNA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "FORCE ONE"), "FORCE ONE INDUSTRIA E COMERCIO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "UNGI CAFE"), "UNGI CAFE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROREGIONAL"), "AGROREGIONAL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "VITAGRI"), "VITAGRI", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BELAGRICOLA"), "BELAGRICOLA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "UNEXPA"), "UNEXPA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BSBIOS"), "BSBIOS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "FERRARI ZAGATTO"), "FERRARI ZAGATTO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "SERRA MORENA"), "SERRA MORENA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "SUSTENTAGRO"), "SUSTENTAGRO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AFRICONTINENTAL"), "AFRICONTINENTAL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "TRADEAGRO"), "TRADEAGRO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BR BEAUTY"), "BR BEAUTY", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "FRONTIERS WTA"), "FRONTIERS WTA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "JARI CELULOSE"), "JARI CELULOSE PAPEL E EMBALAGENS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AGROFRUTAS"), "AGROFRUTAS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "ANIGER"), "ANIGER", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AXIS"), "AXIS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "INCEMA"), "INCEMA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "PIMEX"), "PIMEX", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "Q.ALIMENTARE"), "Q.ALIMENTARE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "VOTORANTIM"), "VOTORANTIM", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "ALDEKE"), "ALDEKE", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "EXPORSAN"), "EXPORSAN", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BOMFIGLIO"), "BOMFIGLIO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "USACIGA"), "USACIGA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "VULCABRAS"), "VULCABRAS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "BRAFRIK"), "BRAFRIK", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "COUROTEX"), "COUROTEX", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "FENIX - COMERCIO"), "FENIX", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "HELA INGREDIENTES BRASIL"), "HELA INGREDIENTES BRASIL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "JUMIL-JUSTINO DE MORAIS"), "JUMIL-JUSTINO DE MORAIS", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "L 3 COMERCIO"), "L 3 COMERCIO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "M3 COMERCIO"), "M3 COMERCIO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "TECNAL INDUSTRIA"), "TECNAL INDUSTRIA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "TECTOR ENGENHARIA"), "TECTOR ENGENHARIA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "VERA CRUZ"), "VERA CRUZ", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "VITALLY COMERCIO"), "VITALLY COMERCIO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "SCOULAR BRASIL"), "SCOULAR BRASIL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "CTA INDUSTRIA"), "CTA INDUSTRIA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "KANEMATSU"), "KANEMATSU", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "LFG COMERCIO"), "LFG COMERCIO", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "READI-BR COMERCIAL"), "READI-BR COMERCIAL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "TOTALE DISTRIBUIDORA"), "TOTALE DISTRIBUIDORA", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "AMAZONIA MIX"), "AMAZONIA MIX", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "GENEXPORT DO BRASIL"), "GENEXPORT DO BRASIL", N_EXPORTER),
                                     N_EXPORTER= if_else(str_detect(N_EXPORTER, "P.A.F. COMERCIAL"), "P.A.F. COMERCIAL", N_EXPORTER))


## aggregate CNPJ to 8 numbers (main tax code)
trase.cpnj <- trase.cpnj %>% mutate (main_cnpj = substring(trase.cpnj$CNPJ, first=1, last=8))
## report first tax code per company and count thenumber of individual cnpjs for each company
trase.cpnj.a <- trase.cpnj %>% group_by(N_EXPORTER) %>% summarise(main_cnpj = pastecs::first (main_cnpj, na.rm=TRUE), 
                                                                  T_CNPJ_first = pastecs::first (CNPJ, na.rm=TRUE),
                                                                  nCNPJ=n_distinct(CNPJ))
## combine trase 2.5 with CNPJ information from trase 2.4 
trase25_f <- trase25_f %>% dplyr::left_join(trase.cpnj.a, by="N_EXPORTER") 

write_csv(trase25_f , file.path (out, "trase_25_cnpj_v01.csv"))
write_rds(trase25_f , file.path (out, "trase_25_cnpj_v01.rds"))






