Replication code for "Adoption and market coverage influence the effectiveness of supply chain policies" doi:XXX


## Authors
Analysis was designed by Florian Gollnow, Federico Cammelli, Kimberly Carlson and Rachael Garrett. Code was written by Florian Gollnow

## Data: 
all data used are openly accessible. Replication data is available upon request.

## Dependencies
Code was written in R 4.1.0, land use and deforestation data was extracted via Google Earth Engine (GEE). R Packages required include tidyverse, sf, raster, and did.

##Summary of scripts
0. data preparation (base for all)
  0.1 reads trase data, consolidates company names, adds ZDCs, adds CNPJ
  0.2
1. company analysis (builds upon 0. data preparation)
  1.1
2. biodiversity analysis (builds upon 0. data preparation)
  2.1
3. DiD analysis (builds upon 0. data preparation)
  3.1 data aggregation
  3.2 balance panel for all municipalities inside the Amzon that had soybean planted throughout 2005-2018
  3.3 code SoyM treatment variable
  3.4 DiD analysis and avoided deforestation 
  3.5 maps treatment year and municipality
4. Data scripts
  4.1 Soy Deforestation (Mapbiomas)
  4.2 Study area map
  4.3 Biomes 



