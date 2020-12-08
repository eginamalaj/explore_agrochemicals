#
#
#
require(tidyverse)
require(rgdal)
require(doBy)# summaryBy
require(egg)# ggarrange
require(classInt)#classIntervals
require(raster)# getData
require(cowplot) #plot_grid
#
#
#
# Load data that was created from dataClean.R file
load("censdat.RData")# data saved as cens
#
# Import shapefiles with stats Canada's census units
cd<- readOGR("CDshapefile/2016CD_ag.shp","2016CD_ag")
#
# ------------------------------------------------------------------------------------------------------
# Functions For Plotting
# ------------------------------------------------------------------------------------------------------
#
#
# Theme for plotting maps
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_blank(), 
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}
#
#
# ------------------------------------------------------------------------------------------------------
#
# Cleaning & Calculations of variables
#
# ------------------------------------------------------------------------------------------------------
#
# General formatting of dataset
#
cens2<- cens%>%
  separate(SGC, into = c('CDUID', 'other'), sep = 4)%>% # create a CD id
  dplyr::select(-other)%>%
  mutate(CCS=as.character( # add 0 to make the GEO_CCS all with 3 numbers and all characters
    str_pad(CCS, width = 3, pad = "0")),
    CD=as.character( # add 0 to make the GEO_CD all with 2 numbers and all characters
      str_pad(CD, width = 2, pad = "0")))%>%
  filter(!NTFAREA==0) # 200 entries where there are no farms reporting - remove
#
#
####
#################### Formatting for CD level
#
# CD level from census
#
c_cda<-cens2%>%
  filter(!CD== "00" & CCS== "000") # in the CD level - CD not 00 excludes CAR data and CCS = 000 includes CD
#
c_cd<-c_cda[!duplicated(c_cda[,c("CDUID","C_YEAR")]),]
nrow(c_cd)-nrow(c_cda) # 28 entries removed as duplicated - mainly due to reporting in the CAR level for Prairies in 2011 
#
#
######################
#
# CD level from CCS 
#
# Restrict only to CCS
#
# For each parameter - remove NA and 0 and sum up
#
c_ccs<- cens2%>%
  filter(!cens2$CCS=="000")%>%
  replace(is.na(.), 0)%>% # Replace NA with 0 so we can sum them up - see above no real zeros
  dplyr::select(-AREAID,-PROV,-CAR,-CD,-CCS) # remove so summarize down there
#
#
c_ccs2<-summaryBy(data=c_ccs, .~C_YEAR+CDUID , FUN=sum, na.rm=TRUE, keep.names = T)
nrow(c_ccs2)
#
################
#
# Replace NA values in CD level from census
#
c_cd2<-merge(c_cd,c_ccs2, by=c("CDUID","C_YEAR"), all.x=T)
#
c_cd3<-c_cd2%>%
  mutate(TFAREA = ifelse(is.na(TFAREA.x), TFAREA.y, TFAREA.x),
         NTFAREA=NTFAREA.x,
         CRPLND = ifelse(is.na(CRPLND.x), CRPLND.y, CRPLND.x),
         NCRPLND=NCRPLND.x,
         FERTIL = ifelse(is.na(FERTIL.x), FERTIL.y, FERTIL.x),
         NFERTIL=NFERTIL.x,
         HERBCI = ifelse(is.na(HERBCI.x), HERBCI.y, HERBCI.x),
         NHERBCI=NHERBCI.x,
         INSECTI = ifelse(is.na(INSECTI.x), INSECTI.y, INSECTI.x),
         NINSECTI = NINSECTI.x,
         FUNGIC = ifelse(is.na(FUNGIC.x), FUNGIC.y, FUNGIC.x),
         NFUNGIC = NFUNGIC.y,
         grain = ifelse(is.na(grain.x), grain.y, grain.x),
         oilsd = ifelse(is.na(oilsd.x), oilsd.y, oilsd.x),
         plses = ifelse(is.na(plses.x), plses.y, plses.x),
         veg = ifelse(is.na(veg.x), veg.y, veg.x),
         frt = ifelse(is.na(frt.x), frt.y, frt.x),
         CANOLA = ifelse(is.na(CANOLA.x), CANOLA.y, CANOLA.x),
         OATS = ifelse(is.na(OATS.x), OATS.y, OATS.x),
         BARLEY = ifelse(is.na(BARLEY.x), BARLEY.y, BARLEY.x),
         SOYBNS = ifelse(is.na(SOYBNS.x), SOYBNS.y, SOYBNS.x),
         POTATS = ifelse(is.na(POTATS.x), POTATS.y, POTATS.x),
         vegNoPotat = ifelse(is.na(vegNoPotat.x), vegNoPotat.y, vegNoPotat.x),
         corn = ifelse(is.na(corn.x), corn.y, corn.x),
         wheat = ifelse(is.na(wheat.x), wheat.y, wheat.x),
         SALES95 = ifelse(is.na(SALES95.x), SALES95.y, SALES95.x),
         TOTEXP = ifelse(is.na(TOTEXP.x), TOTEXP.y, TOTEXP.x),
         TILCONV = ifelse(is.na(TILCONV.x), TILCONV.y, TILCONV.x),
         TILCONS = ifelse(is.na(TILCONS.x), TILCONS.y, TILCONS.x),
         TILLNO = ifelse(is.na(TILLNO.x), TILLNO.y, TILLNO.x)
  )%>%
  dplyr::select(-c(9:68))%>%
  filter(TFAREA>0 & CRPLND>0) # remove 66 entries with 0 for farm area or cropland as there are no real "zeros" but not reports
## 
#
pCD2<- c_cd3%>%
  # Calculations
  mutate(#Convert acres in ha
    wheat=wheat/2.471, 
    oats=(OATS)/2.471,
    barley=(BARLEY)/2.471,
    corn=corn/2.471,
    canola = (CANOLA)/2.471,
    soybeans = (SOYBNS)/2.471,
    plses=plses/2.471,
    veg=vegNoPotat/2.471,
    potatoes=(POTATS)/2.471,
    frt=frt/2.471,
    frtveg=(veg+frt),
    crplnd=CRPLND/2.471,
    tfarea=TFAREA/2.471,
    insha=INSECTI/2.471,
    herha=HERBCI/2.471,
    funha=FUNGIC/2.471,
    fertha=FERTIL/2.471,
    #
    # Everything converted
    wheatP= wheat/crplnd,
    oatsP=OATS/crplnd,
    barleyP=BARLEY/crplnd,
    cornP=corn/crplnd,
    canolaP=canola/crplnd,
    soybeansP=soybeans/crplnd,
    plsesP=plses/crplnd,
    frtvegP=(veg+frt)/crplnd,
    potatoesP=potatoes/crplnd,
    frmsiz= (tfarea/NTFAREA),
    landSimp=crplnd/tfarea, #landscape simplification - cropland area/total farm area
    insPrc = insha/crplnd, # % insecticide used - ins treated area/cropland area
    herPrc = herha/crplnd, # % herbicides used - herb treated area/cropland area
    funPrc = funha/crplnd, # % fungicides used - fung treated area/cropland area
    fertPrc = fertha/crplnd)# %fertilizers used - fert treated area/cropland area

colnames(pCD2)[c(4,6)]<-c("GEO_PROV","GEO_CD")
pCD2$REGION<-NA
#
# Name regions
#
pCD2$REGION[pCD2$GEO_PROV==10 | pCD2$GEO_PROV==11 | pCD2$GEO_PROV==12 | pCD2$GEO_PROV==13]<-"ATLANTIC"
pCD2$REGION[pCD2$GEO_PROV==24 | pCD2$GEO_PROV==35]<-"CENTRAL"
pCD2$REGION[pCD2$GEO_PROV==46 | pCD2$GEO_PROV==47 | pCD2$GEO_PROV==48]<-"PRAIRIE"
pCD2$REGION[pCD2$GEO_PROV==59]<-"PACIFIC"
#
#
# Format data - all units where chemicals were reported and the years of importance
# 
pest<-pCD2[,c("GEO_PROV","REGION","CDUID","C_YEAR","herPrc","funPrc","insPrc", "fertPrc",
              "NHERBCI","NFUNGIC","NINSECTI","NFERTIL","frmsiz",
              "herha","funha","insha","fertha","landSimp")]
#
# Split per group of agrochemicals
# remove 0 which means there were no farms reporting
# remove years when insecticides & fungicides were reported together
#      
ins<-pest[!pest$NINSECTI==0 & !pest$insha==0 & !(pest$C_YEAR == "1981" | pest$C_YEAR == "1986" | pest$C_YEAR == "1991"),
          c("GEO_PROV","REGION","CDUID","C_YEAR","insPrc","NINSECTI","insha","frmsiz","landSimp")]
#
#
fung<-pest[!pest$NFUNGIC==0 & !pest$funha==0 & !(pest$C_YEAR == "1981" | pest$C_YEAR == "1986" | pest$C_YEAR == "1991"),
           c("GEO_PROV","REGION","CDUID","C_YEAR","funPrc","NFUNGIC","funha","frmsiz")]
#
#
# Herbicides reported every year
herb<-pest[!pest$NHERBCI==0 & !pest$herha==0, 
           c("GEO_PROV","REGION","CDUID","C_YEAR","herPrc","NHERBCI","herha","frmsiz")]
#
# Very few fertilizer data before '90s - removed
#
fert<-pest[!pest$NFERTIL==0 & !pest$fertha==0 & !(pest$C_YEAR == "1981" | pest$C_YEAR == "1986"),
           c("GEO_PROV","REGION","CDUID","C_YEAR","fertPrc","NFERTIL","fertha","frmsiz")]
#
#
# ------------------------------------------------------------------------------------------------------
#
# Boxplot - Changes over time in agrochemicals
#
# ------------------------------------------------------------------------------------------------------
#
# Insecticides
#
# Order of provinces in Canada from West to East
ins$REGION <- factor(ins$REGION, levels = c("PACIFIC","PRAIRIE", "CENTRAL", "ATLANTIC"))
#
# plot
is<- 
  ins %>% 
  ggplot(aes(x=factor(C_YEAR),y=(insPrc)*100, fill=REGION)) +
  geom_boxplot() +
  xlab("Year")+ 
  facet_wrap(~REGION,ncol = 4) +
  scale_fill_manual(values=c("#41B6C4","#E69F00","#FC4E07","#225EA8"))+ # ColToGrey
  scale_y_continuous(limits = c(0,70), breaks = c(0,20,40,60))+
  labs(x="",y="Insecticides (%)",title="") +
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,colour="black"),
        axis.text.y = element_text(colour="black"),
        strip.text.x = element_blank(),
        plot.margin = unit(c(-0.7,0.1,-0.6,0.1), "cm"), # ("top", "right", "bottom", "left") # remove the white space of around the graph
        legend.position = "none")
#
#
# Repeat for all other agrochemicals 
#
# Fungicides
#
fung$REGION <- factor(fung$REGION, levels = c("PACIFIC","PRAIRIE", "CENTRAL", "ATLANTIC"))
#
fn<- fung %>% 
  ggplot(aes(x=factor(C_YEAR),y=(funPrc)*100, fill=REGION)) +
  geom_boxplot() +
  xlab("Year")+ 
  facet_wrap(~REGION,ncol = 4) +
  scale_fill_manual(values=c("#41B6C4","#E69F00","#FC4E07","#225EA8"))+
  scale_y_continuous(limits = c(0,70), breaks = c(0,20,40,60))+
  labs(x="",y="Fungicides (%)",title="") +
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.margin = unit(c(-0.7,0.1,-0.6,0.1), "cm"),
        strip.text.x = element_blank(),
        legend.position = "none")
#
# Herbicides
#
herb$REGION <- factor(herb$REGION, levels = c("PACIFIC","PRAIRIE", "CENTRAL", "ATLANTIC"))
#
hb<- herb %>% 
  ggplot(aes(x=factor(C_YEAR),y=(herPrc)*100, fill=REGION)) +
  geom_boxplot() +
  xlab("Year")+ 
  facet_wrap(~REGION,ncol = 4)+
  scale_fill_manual(values=c("#41B6C4","#E69F00","#FC4E07","#225EA8"))+
  scale_y_continuous(limits = c(0,150), breaks = c(0,20,40,60,80,100,150))+
  labs(x="",y="Herbicides (%)",title="") +
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.margin = unit(c(-0.7,0.1,-0.6,0.1), "cm"),
        strip.text.x = element_blank(),
        legend.position = "none")
#
#
fert$REGION <- factor(fert$REGION, levels = c("PACIFIC","PRAIRIE", "CENTRAL", "ATLANTIC"))
#
fer<- 
  fert %>% 
  ggplot(aes(x=factor(C_YEAR),y=(fertPrc)*100, fill=REGION)) +
  geom_boxplot() +
  #geom_jitter(width=0.1,alpha=0.2) +
  xlab("Year")+ 
  facet_wrap(~REGION,ncol = 4, strip.position="top") +
  scale_fill_manual(values=c("#41B6C4","#E69F00","#FC4E07","#225EA8"))+
  scale_y_continuous(limits = c(0,220), breaks = c(0,50,100,220))+
  labs(x="",y="Fertilizers (%)",title="") +
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,colour="black"),
        axis.text.y = element_text(colour="black"),
        plot.margin = unit(c(-0.5,0.1,-0.6,0.1), "cm"),
        strip.text.y = element_text(margin = margin(0,0.1,0,0.1, "cm")),
        legend.position = "none")
#
#
x11(width = 10, height = 10)
pbox<- ggarrange(fer,is, fn, hb,
                 labels = c("A", "B", "C", "D"),
                 ncol = 1, nrow = 4)
#
#
#------------------------------------------------------------------------------------------------------
#
# Exploring crop trends 
#
# ------------------------------------------------------------------------------------------------------
#
crps<-as.data.frame(pCD2 %>% # select columns
                      dplyr::select("REGION","C_YEAR","CDUID","wheat","oats","barley","corn","canola",
                                    "soybeans","plses","frtveg","potatoes")%>% #"mixoilsd","mixgrain" removed
                      gather(-REGION,-C_YEAR,-CDUID, key="Crops", value= "area")%>% # from wide to long
                      group_by(C_YEAR, REGION,Crops) %>% # group to sum
                      summarise(area= sum(area))) # sum
#
#
crps2<-as.data.frame(crps %>%
                       group_by(REGION,C_YEAR) %>%
                       mutate(tot = sum(as.numeric(area)),
                              prc= ((area/tot)*100)))
#
as.factor(crps2$Crops)->crps2$Crops
gsub("frtveg", "Fruit/Veg", crps2$Crops)->crps2$Crops
gsub("plses", "Pulses", crps2$Crops)->crps2$Crops
#
tools::toTitleCase(crps2$Crops)->crps2$Crops
as.factor(crps2$Crops)->crps2$Crops
#
#
crps2$REGION <- factor(crps2$REGION, levels = c("PACIFIC","PRAIRIE", "CENTRAL", "ATLANTIC"))
#
crps3 <-crps2 %>% 
  mutate(Crops = fct_relevel(Crops, 
                             "Canola", "Soybeans", "Fruit/Veg", 
                             "Potatoes", "Pulses","Oats", 
                             "Corn", "Barley", "Wheat"))
#
x11(width=9, height = 4.5)
cr <- 
  ggplot(aes(y = prc, x = C_YEAR, fill = Crops), data=crps3) + 
  geom_bar(stat="identity")+
  facet_wrap(~REGION,ncol = 4, strip.position="top") +
  scale_fill_brewer(palette = "Spectral")+ # Spectral
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 70,hjust = 1,colour="black", size=12),
        axis.text.y = element_text(colour="black"),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")), # change the size of the box with names
        legend.justification = "bottom", # position of the legend
        legend.position="right",
        legend.margin=margin(0,8,0,-10),
        plot.margin = unit(c(0,0,-0.5,-0.5), "cm"))+ # ("top", "right", "bottom", "left") # remove the white space of around the graph
  labs(x = "",y = "") 
#
#
#
#
#------------------------------------------------------------------------------------------------------
#
# Maps of Canada showing spatial changes in agrochemicals 
#
#------------------------------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------------------------------
#### 1. Join the agrochemical data with the shapefile
# ------------------------------------------------------------------------------------------------------
#
aea.proj <-"+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#
cd<- spTransform(cd, CRS(aea.proj))
#
# Region tells what to use as ID
cdF<-fortify(cd, region="CDUID")
as.factor(cdF$id)->cdF$id
#
# Merge fortified data with insecticide data
#
as.factor(ins$CDUID)->ins$CDUID
idt<-merge(cdF, ins, by.x="id",by.y="CDUID"); idt[is.na(idt$CDNAME),] # ok
idt[is.na(idt$pestPrc),]; nrow(data.frame(unique(idt$id)))# all polygons match
idt<-idt[order(idt$order), ]# order for plotting
#
#
# Repeat for other agrochem
# Merge fortified data with herbicide data
#
as.factor(herb$CDUID)->herb$CDUID
hdt<-merge(cdF, herb, by.x="id",by.y="CDUID")
hdt[is.na(hdt$pestPrc),]; nrow(data.frame(unique(hdt$id))) 
hdt<-hdt[order(hdt$order),]
#
#
# Merge fortified data with fungicide data
#
as.factor(fung$CDUID)->fung$CDUID
fdt<-merge(cdF, fung, by.x="id",by.y="CDUID")
fdt[is.na(fdt$funPrc),]; nrow(data.frame(unique(fdt$id))) # one polygon not matched
fdt<-fdt[order(fdt$order), ]
#
#
# Merge fortified data with fertilizer data
#
as.factor(fert$CDUID)->fert$CDUID
frdt<-merge(cdF, fert, by.x="id",by.y="CDUID")
frdt[is.na(frdt$funPrc),]; nrow(data.frame(unique(frdt$id))) # all polygons match
frdt<-frdt[order(frdt$order), ]
#
#
# ------------------------------------------------------------------------------------------------------
#### 2. Shapefile of Canada
# ------------------------------------------------------------------------------------------------------
#
# Map of Canada for the plot grob
camap <- getData("GADM", country = "canada", level = 1)# need raster package!
camap <- spTransform(camap, CRS(aea.proj))
camapF <- fortify(camap)
#
#
### Canada Borders
#
# To highlight the provinces
#
camapF$positive <- ifelse(camapF$id==1|camapF$id==4|camapF$id==7| # Prairie
                            camapF$id==3|camapF$id==13| #ON/QB
                            camapF$id==6| # BC
                            camapF$id==8|camapF$id==9|camapF$id==11|camapF$id==2, # Maritime
                          "Yes", "No") 
#
# Polygon only with the provinces with data
camapF2<-camapF[camapF$positive=="Yes",]
#
#
# ------------------------------------------------------------------------------------------------------
# 3. Plotting
# ------------------------------------------------------------------------------------------------------
#
# Insecticides
#
x11(); hist(idt$insPrc[idt$C_YEAR=="1996"]) # skewed to the left - quantile distribution
brk_iy <- classIntervals((idt$insPrc)*100, n=5, style="quantile") # divide the data
#
i_lab<-c()
i_brks<- brk_iy$brks
for(y in 1:length(i_brks)){
  i_lab <- c(i_lab,round(i_brks[y + 1],0))
}
i_lab <- i_lab[1:length(i_lab)-1]
#
idt$brks_i <- cut((idt$insPrc)*100, 
                  breaks = i_brks, 
                  include.lowest = TRUE, 
                  labels = i_lab)
i_brks_scale <- levels(idt$brks_i)
i_labels_scale <- rev(i_brks_scale)
#
i <- ggplot() +
  geom_polygon(data = idt, aes(fill = brks_i, 
                               x = long, 
                               y = lat, 
                               group = group)) +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = "black", size = 0.1)+
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom",
        axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        strip.text = element_text(size = 16, face = "bold"))+
  scale_x_continuous(limits = c(-1500000, 4200000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+ 
  facet_wrap(~C_YEAR, ncol=1, strip.position = "left") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_manual(
    values = (c("#7F0000","#B30000","#EF6548","#FDBB84","#FEE8C8")),  # cols generated from brewer.pal(9,"OrRd")
    breaks = rev(i_brks_scale), 
    name = "Insecticides (%)",
    drop = FALSE,
    labels = i_labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(50 / length(i_lab), units = "mm"), 
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))
#
#
# ------------------------------------------------------------------------------------------------------
#
# Fungicides
x11(); hist(fung$funPrc[fung$C_YEAR=="2016"], main="2016", xlab = "Prop. Fungicide Use") # to the left use quantile
brk_fy <- classIntervals((fdt$funPrc)*100, n=5, style="quantile")
#
f_lab<-c()
f_brks<- brk_fy$brks
for(y in 1:length(f_brks)){
  f_lab <- c(f_lab,round(f_brks[y + 1],0))
}
f_lab <- f_lab[1:length(f_lab)-1]
#
fdt$brks_f <- cut((fdt$funPrc)*100, 
                  breaks = f_brks, 
                  include.lowest = TRUE, 
                  labels = f_lab)
f_brks_scale <- levels(fdt$brks_f)
f_labels_scale <- rev(f_brks_scale)
#
f <- ggplot() +
  geom_polygon(data = fdt, aes(fill = brks_f, 
                               x = long, 
                               y = lat, 
                               group = group)) +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = "black", size = 0.1)+
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom",
        #plot.title = element_text(size = 25, face = "bold"),
        axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        strip.text = element_text(size = 16, face = "bold"))+
  scale_x_continuous(limits = c(-1500000, 4200000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+ 
  facet_wrap( ~ C_YEAR, ncol=1, strip.position = "left") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_manual(
    values = (c("#253494","#2C7FB8","#41B6C4","#A1DAB4","#FFFFCC")),
    breaks = rev(f_brks_scale), # !!! 
    name = "Fungicides (%)",
    drop = FALSE,
    labels = f_labels_scale, #!!!!
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(50 / length(f_lab), units = "mm"), # !!!
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))
#
#
# Herbicide Reduced - for main paper
#
x11(); hist(herb$herPrc[herb$C_YEAR=="1996"])
brk_hy <- classIntervals((hdt$herPrc)*100, n=5, style="quantile")
#
h_lab<-c()
h_brks<- brk_hy$brks
for(y in 1:length(h_brks)){
  h_lab <- c(h_lab,round(h_brks[y + 1],0))
}
h_lab <- h_lab[1:length(h_lab)-1]
#
hdt$brks_h <- cut((hdt$herPrc)*100, 
                  breaks = h_brks, 
                  include.lowest = TRUE, 
                  labels = h_lab)
h_brks_scale <- levels(hdt$brks_h)
h_labels_scale <- rev(h_brks_scale)
#
# Not all years included
hdt2<-hdt[hdt$C_YEAR=="1981"|hdt$C_YEAR=="1991"|hdt$C_YEAR=="2001"|
            hdt$C_YEAR=="2011"|hdt$C_YEAR=="2016",]
#
#
hr <- ggplot() +
  geom_polygon(data = hdt2, aes(fill = brks_h, 
                                x = long, 
                                y = lat, 
                                group = group)) +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = "black", size = 0.1)+
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom",
        #plot.title = element_text(size = 25, face = "bold"),
        axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        strip.text = element_text(size = 16, face = "bold"))+
  scale_x_continuous(limits = c(-1500000, 4200000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+ 
  facet_wrap(~C_YEAR, ncol=1, strip.position = "left") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_manual(
    values = (c("#006D2C","#31A354","#74C476","#BAE4B3","#EDF8E9")),
    breaks = rev(h_brks_scale), # !!! 
    name = "Herbicides (%)",
    drop = FALSE,
    labels = h_labels_scale, #!!!!
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(50 / length(h_lab), units = "mm"), # !!!
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))
#
#
# Fertilizers
# 
#
frdt<-frdt[!frdt$C_YEAR==1996,]
x11(); hist(fert$fertPrc[fert$C_YEAR=="1996"]) # mainly to the left use quantile
brk_fry <- classIntervals((frdt$fertPrc)*100, n=5, style="quantile")
#
fr_lab<-c()
fr_brks<- brk_fry$brks
for(y in 1:length(fr_brks)){
  fr_lab <- c(fr_lab,round(fr_brks[y + 1],0))
}
fr_lab <- fr_lab[1:length(fr_lab)-1]
#
frdt$brks_fr <- cut((frdt$fertPrc)*100, 
                    breaks = fr_brks, 
                    include.lowest = TRUE, 
                    labels = fr_lab)
fr_brks_scale <- levels(frdt$brks_fr)
fr_labels_scale <- rev(fr_brks_scale)
#
fr <- ggplot() +
  geom_polygon(data = frdt, aes(fill = brks_fr, 
                                x = long, 
                                y = lat, 
                                group = group)) +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = "black", size = 0.1)+
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom",
        #plot.title = element_text(size = 25, face = "bold"),
        axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        strip.text = element_text(size = 16, face = "bold"))+
  scale_x_continuous(limits = c(-1500000, 4200000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+ 
  facet_wrap(~C_YEAR, ncol=1, strip.position = "left") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_manual(
    values = (c("#810f7c","#8856a7","#8c96c6","#b3cde3","#edf8fb")),  # cols generated from brewer.pal(9,"Greys")
    breaks = rev(fr_brks_scale), # !!! 
    name = "Fertilizers (%)",
    drop = FALSE,
    labels = fr_labels_scale, #!!!!
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(50 / length(fr_lab), units = "mm"), # !!!
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))
#
#
x11(width = 30, height = 18)
allin<-plot_grid(fr,i,f,hr, ncol=4) # takes a couple of minutes to run
#
# ------------------------------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------------------------------
#
# Maps of Canada showing spatial changes in crops 
#
# ------------------------------------------------------------------------------------------------------
#
# Re-group crops
crps_rg<-as.data.frame(pCD2 %>% # select columns
                         dplyr::select("REGION","C_YEAR","CDUID","wheat","oats","barley","corn","canola",
                                       "soybeans","plses","frtveg","potatoes","crplnd")%>%
                         group_by(C_YEAR, REGION,CDUID) %>% # group to sum
                         mutate(cereals= wheat+oats+barley+corn+plses,
                                oilseeds=canola+soybeans,
                                frtveg=frtveg+potatoes,
                                crplnd=crplnd,
                                cerealsP=cereals/crplnd,
                                oilseedsP=oilseeds/crplnd,
                                frtvegP=frtveg/crplnd)
)
#
crps_rg[crps_rg$cerealsP>1,]# several cereal & pulses crops planted in the same growing season - data from 1986 all Prairies
crps_rg$cerealsP[crps_rg$cerealsP>1]<-1 # sub with 1 so we only have 100% -  for mapping colors mainly
#
# Merge fortified data with crop data
#
as.factor(crps_rg$CDUID)->crps_rg$CDUID
crdt<-merge(cdF,crps_rg, by.x="id",by.y="CDUID"); crps_rg[is.na(crps_rg$CDNAME),] # ok
crdt[is.na(crdt$cereals),]; nrow(data.frame(unique(crdt$id)))# all polygons match
#
#
## Cereals
#
x11(); hist(crps_rg$cerealsP[crps_rg$C_YEAR=="1996"]) # non normal
brk_gry <- classIntervals((crdt$cerealsP)*100, n=5, style="quantile")
#
gr_lab<-c()
gr_brks<- brk_gry$brks
for(y in 1:length(gr_brks)){
  gr_lab <- c(gr_lab,round(gr_brks[y + 1],0))
}
gr_lab <- gr_lab[1:length(gr_lab)-1]
#
crdt$brks_gr <- cut((crdt$cerealsP)*100, 
                    breaks = gr_brks, 
                    include.lowest = TRUE, 
                    labels = gr_lab)
gr_brks_scale <- levels(crdt$brks_gr)
gr_labels_scale <- rev(gr_brks_scale)
#
gr <- ggplot() +
  geom_polygon(data = crdt, aes(fill = brks_gr, 
                                x = long, 
                                y = lat, 
                                group = group)) +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = "black", size = 0.1)+
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom",
        #plot.title = element_text(size = 25, face = "bold"),
        axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title=element_text(size=22),
        legend.text=element_text(size=18),
        strip.text = element_text(size = 20, face = "bold"))+
  scale_x_continuous(limits = c(-1500000, 4200000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+ 
  facet_wrap(~C_YEAR, ncol=1, strip.position = "left") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_manual(
    values = rev(brewer.pal(5,"PuRd")), # cols generated from brewer.pal(9,"PuRd")
    breaks = rev(gr_brks_scale), # !!! 
    name = "Cereals + Pulses (%)",
    drop = FALSE,
    labels = gr_labels_scale, #!!!!
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(70 / length(gr_lab), units = "mm"), # !!!
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))
#
#
## Oilseeds
#
x11(); hist(crps_rg$oilseedsP[crps_rg$C_YEAR=="1996"])# not normally distributed - quantile
brk_osy <- classIntervals((crdt$oilseedsP)*100, n=5, style="quantile")
#
os_lab<-c()
os_brks<- brk_osy$brks
for(y in 1:length(os_brks)){
  os_lab <- c(os_lab,round(os_brks[y + 1],0))
}
os_lab <- os_lab[1:length(os_lab)-1]
#
crdt$brks_os <- cut((crdt$oilseedsP)*100, 
                    breaks = os_brks, 
                    include.lowest = TRUE, 
                    labels = os_lab)
os_brks_scale <- levels(crdt$brks_os)
os_labels_scale <- rev(os_brks_scale)
#
os <- ggplot() +
  geom_polygon(data = crdt, aes(fill = brks_os, 
                                x = long, 
                                y = lat, 
                                group = group)) +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = "black", size = 0.1)+
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom",
        #plot.title = element_text(size = 25, face = "bold"),
        axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title=element_text(size=22),
        legend.text=element_text(size=18),
        strip.text = element_text(size = 20, face = "bold"))+
  scale_x_continuous(limits = c(-1500000, 4200000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+ 
  facet_wrap(~C_YEAR, ncol=1, strip.position = "left") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_manual(
    values = rev(brewer.pal(5,"Purples")), # cols generated from brewer.pal(9,"PuBu")
    breaks = rev(os_brks_scale), # !!! 
    name = "Canola + Soybeans (%)",
    drop = FALSE,
    labels = os_labels_scale, #!!!!
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(70 / length(os_lab), units = "mm"), # !!!
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))
#
#
# Fruits and vegetable
#
x11(); hist(crdt$frtvegP[crdt$C_YEAR=="2016"])# not normally distributed - quantile
classIntervals((crdt$frtvegP)*100, n=5, style="quantile") 
brk_fvy <- classIntervals((crdt$frtvegP)*100, n=5, style="fixed", fixedBreaks=c(0, 0.0029, 0.080, 5, 10, 66.2)) # based on quantiles
#
fv_lab<-c()
fv_brks<- brk_fvy$brks
for(y in 1:length(fv_brks)){
  fv_lab <- c(fv_lab,round(fv_brks[y + 1],0))
}
fv_lab <- fv_lab[1:length(fv_lab)-1]
#
crdt$brks_fv <- cut((crdt$frtvegP)*100, 
                    breaks = fv_brks, 
                    include.lowest = TRUE, 
                    labels = fv_lab)
fv_brks_scale <- levels(crdt$brks_fv)
fv_labels_scale <- rev(fv_brks_scale)
#
fv <- ggplot() +
  geom_polygon(data = crdt, aes(fill = brks_fv, 
                                x = long, 
                                y = lat, 
                                group = group)) +
  geom_polygon(data = camapF2, aes(x=long, y=lat, group = group), fill=NA, color = "black", size = 0.1)+
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom",
        #plot.title = element_text(size = 25, face = "bold"),
        axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.title=element_text(size=22),
        legend.text=element_text(size=18),
        strip.text = element_text(size = 20, face = "bold"))+
  scale_x_continuous(limits = c(-1500000, 4200000))+ # everything visible
  scale_y_continuous(limits = c(4800000, 7500000))+ 
  facet_wrap(~C_YEAR, ncol=1, strip.position = "left") +
  labs(x = NULL, 
       y = NULL)+
  scale_fill_manual(
    values = (brewer.pal(5,"YlOrBr")), # cols generated from brewer.pal(9,"PuRd")
    breaks = rev(fv_brks_scale), # !!! 
    name = "Fruits + Vegetables (%)",
    drop = FALSE,
    labels = fv_labels_scale, #!!!!
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(70 / length(fv_lab), units = "mm"), # !!!
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    ))
#
#
#
X11(width = 18, height = 20)
allcrop<-plot_grid(gr,os,fv, ncol=3)
#
# ------------------------------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------------------------------
#






