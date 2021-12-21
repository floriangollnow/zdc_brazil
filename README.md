Replication code for "Adoption and market coverage influence the effectiveness of supply chain policies" doi:XXX

## Status: 
replication and cleaning in progress (replication complete, cleaning complete, brief descriptions present)

## Authors
Analysis was designed by Florian Gollnow, Federico Cammelli, Kimberly Carlson and Rachael Garrett. Code was written by Florian Gollnow

## Data: 
all data used are openly accessible. Replication data is available [here](https://www.dropbox.com/sh/irog673gk6yy5az/AAB9FTCjn-0Bg-6RSLIQoDUUa?dl=0). Other data is available on request.

## Dependencies
Code was written in R 4.1.0, land use and deforestation data was extracted via Google Earth Engine (GEE). R Packages required include tidyverse, sf, raster, and did among others.

## Summary of scripts 
results from *italicize* scripts provided for result replication in *bold*. Scripts run sequential, with each main section beeing based on the main data preparation (section 0). 

0. data preparation  
  0.1 reads trase data, consolidates company names, adds ZDCs, adds CNPJ  
  0.2 GEE Mapbiomas forest area  
  0.3 GEE Mapbiomas soy area  
  0.4 GEE Mapbiomas soy-deforestation  
  0.5 GEE soy suitable forest
  0.6 reading GEE output  
  0.7 *Biomes-municipality intersection (output provided in data folder)*  
  0.8 *data combination and aggregation municipality (output provided in data folder)*      
1. company analysis (builds upon 0.8 data preparation, use data from 1.5 for replication)   
  1.1 CNPJ data preparation  
  1.2 CNPJ# Trase join  
  1.3 Soy Infrastructure 
  1.4 Deforestation hot spots  
  1.5 *add cnpj data (output provided in Data folder)*  
  1.6 **create figures for manuscript**  
2. ZDC spatial coverage (builds on 0.8)   
  2.1 forest suit adoption bias deforestation
3. *biodiversity analysis (builds upon 0.7 and 3.1)*   
  3.1 Species richness data intersection with Municipalities 
  3.2 GEE species range intersection with forest and soy-suitable forest
  3.2 Quantifying overlap of species ranges with soy-suitable forests and ZDC market share  
4. DiD analysis (builds upon 0.8)   
  4.1 **balanced panel for all municipalities inside the Amazon that had soybean planted throughout 2005-2018**  
  4.2 **code SoyM ZDC treatment variable**  
  4.3 **DiD analysis and avoided deforestation**   
  4.4 maps treatment year and municipality  
5. Study area map  
  5.1 GEE Mapbiomas raster maps for visualization  
  5.2 Study area maps   



