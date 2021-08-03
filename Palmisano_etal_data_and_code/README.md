---
title: "Long-Term Demographic Trends in Prehistoric Italy: climate impacts and regionalised socio-ecological trajectories (dataset and R scripts) README"
authors: Alessio Palmisano, Andrew Bevan, Alexander Kabelindde, Neil Roberts, and Stephen Shennan
dataset_version: 1.0
---

# Long-Term Demographic Trends in Prehistoric Italy: climate impacts and regionalised socio-ecological trajectories (dataset and R scripts) 
The present digital archive is the outcome of the paper: Palmisano, A., Bevan, A., Kabelinnde, A., Roberts, N., and Shennan, S., 2021. Long-Term Demographic Trends in Prehistoric Italy: climate impacts and regionalised socio-ecological trajectories. Journal of World Prehistory, 34.  

The dataset included here provides a collection of 4,010 radiocarbon dates for a period spanning between 11,000 and 1500 uncalibrated yrs. BP. In addition, the digital archive related to this paper provides reproducible analyses in the form of one script written in R statistical computing language. 

NB:the present digital archive does not include the 11 palaeoclimate records for the paper published in Journal of World Prehistory. They will be published separately in forthcoming publications by the holders of those data. Therefore, the following figures reproduced by using the R script and data here provided will not show the curves of the palaeoclimatic records published in the original paper: Figs. 6-10.     
.   

###### CONTENTS ######

#R_script
* Script for calibrating and analysing radiocarbon dates and for reproducing Figures 1-5, and 7-10 (radiocarbon.R).


#Folder "shp"
*ESRI polygon shapefile of the European countries (eur_states_lo.shp).
*ESRI polygon shapefile of Italy (italy.shp).
*ESRI point shapefile of 11 palaeoclimate proxies (palaeoclimate_records.shp).
*ESRI polygon shapefile of lakes and major water bodies in Italy (lakes.shp).
*ESRI polygon shapefile of five geo-cultural regions (italy_regions.shp).
*ESRI polygon shapefile of the regional GDP in Italy (italy_GDP_region.shp).

#Folder "function"
 * Original R package rcarbon's modified source code for plotting overlapping SPDs and Bootstrapped composite Kernel Density Estimation (cKDE) (plot.compositeKDE.R).
 
#Folder "csv"
* csv files of radiocarbon dates (dates.csv).
* A .txt file providing the field description for the attributes of the radiocarbon dates (dates_key.txt).
* A .txt file providing a list of references from which the radiocarbon dates were collected (references_dates.txt).

#Folder "rda"
* RData file storing the outputs (e.g. SPDs, null models, permutation test) generated via the radiocarbon dataset (radiocarbon.RData).
* RData file storing the Pearson’s correlations (r) values ranging from +1 to -1 by using a 500-year-time moving window (correlations.RData).

#Folder "raster"
* Digital elevation model (DEM) of Italy (dem_ita.tif).

#Folder "pdf"
an empty folder where to store the figures outputted via the R scripts provided here. 

###### Licences
Dataset: CC-BY (http://creativecommons.org/licenses/by/4.0/)

Code: MIT (http://opensource.org/licenses/MIT year: 2021, copyright holder: Alessio Palmisano, Andrew Bevan, Alexander Kabelindde, Neil Roberts, and Stephen Shennan)

##### Dependencies
R version 4.1.0 (2021-05-18)

Packages
* cartography (v. 3.0.0)
* doParallel (v. 1.0.16)
* maps (v. 3.3.0)
* maptools (v. 1.1-1)
* raster (v. 3.4-13)
* rcarbon (v. 1.4.2)
* rgdal (v. 1.5-23)
* sf (v. 1.0-2)
* spatstat (v.2.2-0)


