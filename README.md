# Visualizing agrochemical and crop changes across Canada

This file contains part of the R code to run **exploratory analysis** for the article:

**Malaj, E.**, Freistadt L, Morrissey CA. (2020). Spatio-Temporal Patterns of Crops and Agrochemicals in Canada Over 35 Years. *Frontiers in Environmental Science* 8, https://doi.org/10.3389/fenvs.2020.556452

The original datasets and full R code produced from this study are available at: Federated Research Data Repository (FRDR) https://doi.org/10.20383/101.0272


## Depends

To run this analysis two files are require:  
(i) `dataClean.RData` file produced in the repository **[CleanCensusDat](https://github.com/eginamalaj/CleanCensusDat)**;  
(ii) CDshapefile folder where `2016CD_ag.shp` file is necessary for mapping. 

R version 3.6.1

Packages: tidyverse, rgdal, doBy, egg, classInt, raster, cowplot

## Agrochemicals

Data vizalization and exloratory analysis was performed to idnetify trends in agrochemical use across Canada for the last 35 years. The figure below shows the percent of the cropland treated with agrochemicals for each Census of Agriculture year and agricultural region in Canada. Agrochemicals include fertilizers, insecticides, fungicides, and herbicides, and the agricultural regions are ordered from west to east: British Columbia (Pacific; turquoise), Alberta, Saskatchewan and Manitoba (Prairie; orange), Ontario and Quebec (Central; red), and Nova Scotia, New Brunswick, Newfoundland/Labrador, Prince Edward Island (Atlantic; blue). Note that the date range of available data from the Census of Agriculture varies by agrochemical group: fertilizers (1991–2016), insecticides and fungicides (1996–2016), and herbicides (1981–2016). Two census division units for herbicides and 11 census division units for fertilizers reported 100% area cropped, suggesting frequent, repeated applications in a growing season.

![boxplot_agrochem](https://user-images.githubusercontent.com/54320408/101517334-41d67600-3946-11eb-9c78-e03e4453ca5a.png)

The 2016 Census of Agriculture boundary file for the CD units (`2016CD_ag.shp` in CDshapefile folder) was used for spatial mapping. Mapping categories were optimized to illustrate the spatial distribution of each agrochemical group based on their specific distributions by census year. For example, the figure below shows the distribution of fungicides for the year 2016. 

![hist_fung_2016](https://user-images.githubusercontent.com/54320408/101517528-777b5f00-3946-11eb-8539-4d0ab5a40f37.png)

Most of the data followed a similar, left-skewed distributions, and therefore quantile distribution of classes was performed for multi-map vizualitation for all groups. Figure below shows the geographic distribution of agrochemicals calculated as percent of cropland treated with fertilizers, insecticides, fungicides, and herbicides for each census of agriculture year across Canada. Note that the date range from the Census of Agriculture varies by agrochemical group: fertilizers (1991–2016), insecticides and fungicides (1996–2016), and herbicides (1981–2016). Two census division units for herbicides and 11 census division units for fertilizers reported > 100% area cropped, suggesting frequent, repeated applications in a growing season

![map_agrochem](https://user-images.githubusercontent.com/54320408/101517595-8a8e2f00-3946-11eb-826a-2b7ec8bfcff8.png)


## Crops

Figure below shows proportion of land planted with nine major agricultural crops for eight census years (1981–2016) for each of the four agricultural regions in Canada. The agricultural regions are ordered from west to east: British Columbia (Pacific), Alberta, Saskatchewan and Manitoba (Prairie), Ontario and Quebec (Central), and Nova Scotia, New Brunswick, Newfoundland/Labrador, Prince Edward Island (Atlantic).

![crop_groups](https://user-images.githubusercontent.com/54320408/101517725-ae517500-3946-11eb-8154-0f93b856dccf.png)

There were notable regional patterns in the distribution of major crops with: (i) canola, wheat, pulses, oats, and barley mostly grown in the Prairie region; (ii) soybeans and corn almost exclusively grown in the Central region; (iii) fruits and vegetables grown in all provinces, but with the greatest area in production in the Central and Pacific regions; and (iv) potatoes predominantly grown in the Atlantic and the Prairie regions. This can be visually inspected by mapping these groups of crops. The proportion of land treated in the CD unit was calculated as the area in each class divided by the total area in cropland. Classes included:   
(i) cereals and pulses;  
(ii) oilseeds and soybeans; and   
(iii) fruits and vegetables. 

![map_crops](https://user-images.githubusercontent.com/54320408/101517655-9aa60e80-3946-11eb-8e4a-f804b1124575.png)

Here, we can notice specific areas where the changes happened, such as a decrease in cereals and increase in oilseeds for the Prairie region.


## Summary

This study reveals rapid, widescale, and sustained increases in agrochemical applications across much of Canada’s four agricultural regions over three decades. Dramatic shifts in agrochemical treatments and related cropping patterns were most notable for the Prairie and Central regions, but they were associated with different crop types that are unique to each region.

Areas with high agrochemical applications were related to the dominant crop production areas in Canada, and they were concentrated in specific regions such as the Prairies and southern Ontario. For example, area in oilseeds in the northern and eastern part of the Prairies coincides with the region with a high proportion of land area being treated with agrochemicals for the same time period. Pesticides are heavily applied in the canola crop in the Prairies. Similarly, in the Central part of Canada, soybeans and corn are the dominant crops, and they are likely driving significant increases in areas treated with agrochemicals in southern Ontario.


