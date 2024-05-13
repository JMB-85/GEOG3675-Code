install.packages("tidyverse")
install.packages("sf")
install.packages("sp")
install.packages("ggplot")
install.packages("sfheaders")
install.packages("arsenal")
install.packages("reshape2")
install.packages("rjsonlite")
library("tidyverse")
library("sf")
library("sp")
library("ggplot2")
library("sfheaders")
library("arsenal")
library("reshape2")
library("jsonlite")
#import Store data from https://drive.google.com/file/d/1B8M7m86rQg2sx2TsHhFa2d-x-dZ1DbSy/view
#newest version is v30 (December 2023), other versions used are v23 (March 2022) and v3 (November 2014)
v30 <- read.csv("uk_glx_open_retail_points.csv")
v23 <- read.csv("geolytix_retailpoints_v23_202203.csv")
v3 <- read.csv("geolytix_retailpoints_v3_201411.csv")

#data cleansing
v30 <- subset(v30, select =-c(retailer,store_name,add_one,add_two,town,suburb,postcode,long_wgs,lat_wgs,pqi,
                              open_date,geom_p_4326,geom_p_27700,county,post_area,shop_town,region))
v23 <- subset(v23, select =-c(retailer,store_name,add_one,add_two,town,suburb,postcode,long_wgs,lat_wgs,pqi,
                              open_date,county))
v3 <- subset(v3, select =-c(Retailer,StoreName,Add1,Add2,Town,Locality,Postcode,LongWGS84,LatWGS84))

#Removal of stores outside the scope of this study -e.g. predominantly non-food retail, stores located in Hospitals
#Motorway Service areas and those selling wholesale goods.
#v30
v30 <- v30[!grepl("Asda Living", v30$fascia),]
v30 <- v30[!grepl("Costco", v30$fascia),]
v30 <- v30[!grepl("Makro", v30$fascia),]
v30 <- v30[!grepl("Marks and Spencer Home", v30$fascia),]
v30 <- v30[!grepl("Marks and Spencer MSA", v30$fascia),]
v30 <- v30[!grepl("Marks and Spencer Outlet", v30$fascia),]
v30 <- v30[!grepl("Marks and Spencer Hospital", v30$fascia),]
v30 <- v30[!grepl("Marks and Spencer Travel SF", v30$fascia),]
v30 <- v30[!grepl("Morrisons Home and Nutmeg", v30$fascia),]
v30 <- v30[!grepl("Waitrose MSA", v30$fascia),]

#v23
v23 <- v23[!grepl("Asda Living", v23$fascia),]
v23 <- v23[!grepl("Costco", v23$fascia),]
v23 <- v23[!grepl("Makro", v23$fascia),]
v23 <- v23[!grepl("Marks and Spencer Home", v23$fascia),]
v23 <- v23[!grepl("Marks and Spencer MSA", v23$fascia),]
v23 <- v23[!grepl("Marks and Spencer Outlet", v23$fascia),]
v23 <- v23[!grepl("Marks and Spencer Hospital", v23$fascia),]
v23 <- v23[!grepl("Marks and Spencer Travel SF", v23$fascia),]
v23 <- v23[!grepl("Morrisons Home and Nutmeg", v23$fascia),]
v23 <- v23[!grepl("Waitrose MSA", v23$fascia),]

#v3
v3 <- v3[!grepl("Asda Living", v3$Fascia),]
v3 <- v3[!grepl("Marks And Spencer Home", v3$Fascia),]
v3 <- v3[!grepl("Marks And Spencer MSA", v3$Fascia),]
v3 <- v3[!grepl("Marks And Spencer Outlet", v3$Fascia),]
v3 <- v3[!grepl("Marks And Spencer Travel SF", v3$Fascia),]
v3 <- v3[!grepl("Waitrose MSA", v3$Fascia),]

# Rationalising store names by parent company

fascia_Ration <- c('Aldi Local'="Aldi",'Aldi'="Aldi",'Amazon Fresh' = "Amazon",'Asda Express'="Asda",'Asda Supercentre'="Asda",'Asda On the Move'="Asda",
                   'Booths'="Booths",'Budgens'="Budgens",'Asda PFS'="Asda",'Asda Supermarket'="Asda",'Asda Superstore'="Asda", 'Cooltrader'="Heron",
                   'Dunnes Stores'="Dunnes Stores",'Eurospar'="Eurospar",'Eurospar PFS'="Eurospar",'Little Waitrose'="Waitrose",'Little Waitrose Shell'="Waitrose",
                   'Farmfoods'="Farmfoods",'Marks and Spencer BP'="Marks and Spencer",'Marks and Spencer Food To Go'="Marks and Spencer",
                   'Marks and Spencer Foodhall'="Marks and Spencer",'Marks and Spencer Simply Food'="Marks and Spencer",
                   'Morrisons Daily'="Morrisons",'Morrisons Select'="Morrisons",'Sainsburys Local'="Sainsburys",
                   'Spar PFS'="Spar",'Tesco Express'="Tesco",'Tesco Express Esso'="Tesco",'Tesco Extra'="Tesco",
                   'The Co-operative Food PFS'="The Co-operative Food",'The Food Warehouse'="Iceland",'Heron'="Heron",
                   'Iceland'="Iceland",'Lidl'="Lidl",'Marks and Spencer'="Marks and Spencer",'Morrisons'="Morrisons",
                   'Planet Organic'="Planet Organic", 'Sainsburys'="Sainsburys",'Spar'="Spar",'Swift'="Iceland",'Tesco'="Tesco",
                   'The Co-operative Food'="The Co-operative Food",'Waitrose'="Waitrose",'Whole Foods Market'="Whole Foods Market",
                   'Jacks'="Tesco",'Mere'="Mere",'Tesco Metro'="Tesco",
                   'ASDA Supercentre'="Asda",'ASDA Supermarket'="Asda",'Marks And Spencer BP'="Marks and Spencer",
                   'Marks And Spencer Food Outlet'="Marks and Spencer",'Marks And Spencer Simply Food'="Marks and Spencer",'Marks And Spencer'="Marks and Spencer",
                   'Morrisons Local'="Morrisons",'Whole Foods'="Whole Foods Market")

v30$fascia <- as.character(fascia_Ration[v30$fascia])
v23$fascia <- as.character(fascia_Ration[v23$fascia])
v3$Fascia <- as.character(fascia_Ration[v3$Fascia])

# Rationalising Store sizes

size_Ration <- c('< 3,013 ft2 (280m2)'="Small",'3,013 < 15,069 ft2 (280 < 1,400 m2)'="Medium",
                 '15,069 < 30,138 ft2 (1,400 < 2,800 m2)'="Large",'30,138 ft2 > (2,800 m2)'="X_Large")

v30$size_band <- as.character(size_Ration[v30$size_band])
v23$size_band <- as.character(size_Ration[v23$size_band])

# set CRS to EPSG:27700 for EPC data and set points based on easting/northing
v30 = st_as_sf(v30, coords=c("bng_e", "bng_n"), crs="EPSG:27700")
v23 = st_as_sf(v23, coords=c("bng_e", "bng_n"), crs="EPSG:27700")
v3 = st_as_sf(v3, coords=c("EastingBNG", "NorthingBNG"), crs="EPSG:27700")

# cutting out non-Great Britain stores
# Make polygon to remove Northern Ireland, Channel Islands and Isle of Man using OSGB36 coordinates
Big_Poly <- cbind(c(632471, 63352, 210648, 283337, 196331, -37018, 476577, 697605, 632471), c(110669, -18531, 418176, 490865, 519500, 897612, 1263856, 330269, 110669))
Big_Poly <- sf_polygon(obj=Big_Poly)
st_crs(Big_Poly) <- "EPSG:27700"

# using newly made polygon, filter out stores outside the polygon.
v30_clean <- st_filter(v30, Big_Poly)
v23_clean <- st_filter(v23, Big_Poly)
v3_clean <- st_filter(v3, Big_Poly)

#import PWC data LSOA/Data Zones and remove unnecessary columns 
# 2021 England and Wales, from https://www.data.gov.uk/dataset/1b61943c-f5e1-4398-babe-5c487257864e/lower-layer-super-output-areas-december-2021-ew-population-weighted-centroids
PWC_EW <- read.csv("LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_4329361596324526285.csv")
PWC_EW <- subset(PWC_EW, select =-c(FID,GlobalID))
PWC_EW = st_as_sf(PWC_EW, coords=c("x", "y"), crs="EPSG:27700")
PWC_EW = rename(PWC_EW)
# 2011 Scotland, from https://www.data.gov.uk/dataset/8aabd120-6e15-41bf-be7c-2536cbc4b2e5/data-zone-centroids-2011
PWC_S <- read_sf("SG_DataZone_Cent_2011.shp")
PWC_S <- subset(PWC_S, select =-c(Name,TotPop2011,ResPop2011,HHCnt2011,Easting,Northing))

#Renaming first column to match
names(PWC_EW)[names(PWC_EW) == 'LSOA21CD'] <- 'Area_Code'
names(PWC_S)[names(PWC_S) == 'DataZone'] <- 'Area_Code'

#Merge both sets of PWC
PWC_merge <- rbind(PWC_EW, PWC_S)

#import LSOA/DZ demographic data all entries from both 2021 E&W Census and 2012 Scottish Census, all formed into a percentage of either individuals
# or households, as appropriate.
demo <- read.csv("merged_demo_data.csv")
summary(demo)

#merge PWC and demo data
PWC_demo <- merge(PWC_merge, demo)

#making buffers for each version of the store data for both Aldi & Lidl together as well as the 'Big 4' supermarkets.
#subsetting the different groups
#v30
v30_AL <- subset(v30_clean, fascia == "Aldi")
v30_LI <- subset(v30_clean, fascia == "Lidl")
v30_DISC <- rbind(v30_AL, v30_LI)
rm(v30_AL)
rm(v30_LI)

v30_SA <- subset(v30_clean, fascia == "Sainsburys")
v30_MO <- subset(v30_clean, fascia == "Morrisons")
v30_TE <- subset(v30_clean, fascia == "Tesco")
v30_AS <- subset(v30_clean, fascia == "Asda")
v30_B4 <- rbind(v30_SA, v30_MO, v30_TE, v30_AS)
rm(v30_SA)
rm(v30_MO)
rm(v30_TE)
rm(v30_AS)

#v23
v23_AL <- subset(v23_clean, fascia == "Aldi")
v23_LI <- subset(v23_clean, fascia == "Lidl")
v23_DISC <- rbind(v23_AL, v23_LI)
rm(v23_AL)
rm(v23_LI)

v23_SA <- subset(v23_clean, fascia == "Sainsburys")
v23_MO <- subset(v23_clean, fascia == "Morrisons")
v23_TE <- subset(v23_clean, fascia == "Tesco")
v23_AS <- subset(v23_clean, fascia == "Asda")
v23_B4 <- rbind(v23_SA, v23_MO, v23_TE, v23_AS)
rm(v23_SA)
rm(v23_MO)
rm(v23_TE)
rm(v23_AS)

#v3
v3_AL <- subset(v3_clean, Fascia == "Aldi")
v3_LI <- subset(v3_clean, Fascia == "Lidl")
v3_DISC <- rbind(v3_AL, v3_LI)
rm(v3_AL)
rm(v3_LI)

v3_SA <- subset(v3_clean, Fascia == "Sainsburys")
v3_MO <- subset(v3_clean, Fascia == "Morrisons")
v3_TE <- subset(v3_clean, Fascia == "Tesco")
v3_AS <- subset(v3_clean, Fascia == "Asda")
v3_B4 <- rbind(v3_SA, v3_MO, v3_TE, v3_AS)
rm(v3_SA)
rm(v3_MO)
rm(v3_TE)
rm(v3_AS)

# Buffers at 500m
#v30
v30_DISC_Buffers500m <- st_buffer(v30_DISC, 500)
v30_DISC_Buffers500m <- st_union(v30_DISC_Buffers500m)

v30_B4_Buffers500m <- st_buffer(v30_B4, 500)
v30_B4_Buffers500m <- st_union(v30_B4_Buffers500m)

#v23
v23_DISC_Buffers500m <- st_buffer(v23_DISC, 500)
v23_DISC_Buffers500m <- st_union(v23_DISC_Buffers500m)

v23_B4_Buffers500m <- st_buffer(v23_B4, 500)
v23_B4_Buffers500m <- st_union(v23_B4_Buffers500m)

#v3
v3_DISC_Buffers500m <- st_buffer(v3_DISC, 500)
v3_DISC_Buffers500m <- st_union(v3_DISC_Buffers500m)

v3_B4_Buffers500m <- st_buffer(v3_B4, 500)
v3_B4_Buffers500m <- st_union(v3_B4_Buffers500m)

#Extract PWC points covered by buffers
#v30
v30_DISC_PWC <- st_filter(PWC_demo, v30_DISC_Buffers500m)
v30_B4_PWC <- st_filter(PWC_demo, v30_B4_Buffers500m)

#v23
v23_DISC_PWC <- st_filter(PWC_demo, v23_DISC_Buffers500m)
v23_B4_PWC <- st_filter(PWC_demo, v23_B4_Buffers500m)

#v3
v3_DISC_PWC <- st_filter(PWC_demo, v3_DISC_Buffers500m)
v3_B4_PWC <- st_filter(PWC_demo, v3_B4_Buffers500m)

# converting sf object to data frames for comparison

#v30
v30_DISC_PWC <- sf_to_df(v30_DISC_PWC, fill = TRUE)
v30_B4_PWC <- sf_to_df(v30_B4_PWC, fill = TRUE)

#v23
v23_DISC_PWC <- sf_to_df(v23_DISC_PWC, fill = TRUE)
v23_B4_PWC <- sf_to_df(v23_B4_PWC, fill = TRUE)

#v3
v3_DISC_PWC <- sf_to_df(v3_DISC_PWC, fill = TRUE)
v3_B4_PWC <- sf_to_df(v3_B4_PWC, fill = TRUE)

# remove first column as it doesn't contain numeric data and messes up the statistics

v30_DISC_PWC <- v30_DISC_PWC[ -c(1)]
v23_DISC_PWC <- v23_DISC_PWC[ -c(1)]
v3_DISC_PWC <- v3_DISC_PWC[ -c(1)]

v30_B4_PWC <- v30_B4_PWC[ -c(1)]
v23_B4_PWC <- v23_B4_PWC[ -c(1)]
v3_B4_PWC <- v3_B4_PWC[ -c(1)]

#compare basic statistical data, function allows the inclusion of all three versions on one data frame.

compare_them <- function(data1,data2,data3) {
  sum1 <- apply(data1,2,summary) %>% data.frame() 
  sum2 <- apply(data2,2,summary) %>% data.frame()
  sum3 <- apply(data3,2,summary) %>% data.frame()
  
  names(sum1) <- paste0(names(sum1),"v30")
  names(sum2) <- paste0(names(sum2),"v23")
  names(sum3) <- paste0(names(sum3),"v3")
  
  final <- cbind(sum1,sum2,sum3)
  
  final1 <- t(final) 
  
  final2 <- final1[order(row.names(final1)), ]
  
  final_1 <- t(final2) %>% data.frame()
  final_1
}

# Discounter basic statistical data comparison
vall_DISC_sum <- compare_them(v30_DISC_PWC,v23_DISC_PWC,v3_DISC_PWC)
# Big 4 basic statistical data comparison
vall_B4_sum <- compare_them(v30_B4_PWC,v23_B4_PWC,v3_B4_PWC)

###
#importing regional areas, using ITL1 classifications (Jan 2021), downloaded from https://geoportal.statistics.gov.uk/datasets/6acd1e2fc3d1482e9f5da27d935216dd_0/explore
ITL1 <- read_sf("ITL1_MAY_2021_UK_BGC.shp")
ITL1 <- subset(ITL1, select =-c(ITL121CD,BNG_E,BNG_N,LONG,LAT))
# breaking into polygons for each region
NE <- ITL1[(ITL1$ITL121NM == "North East (England)"),]
NW <- ITL1[(ITL1$ITL121NM == "North West (England)"),]
YO <- ITL1[(ITL1$ITL121NM == "Yorkshire and The Humber"),]
EM <- ITL1[(ITL1$ITL121NM == "East Midlands (England)"),]
WM <- ITL1[(ITL1$ITL121NM == "West Midlands (England)"),]
E <- ITL1[(ITL1$ITL121NM == "East"),]
LO <- ITL1[(ITL1$ITL121NM == "London"),]
SE <- ITL1[(ITL1$ITL121NM == "South East (England)"),]
SW <- ITL1[(ITL1$ITL121NM == "South West (England)"),]
WA <- ITL1[(ITL1$ITL121NM == "Wales"),]
SC <- ITL1[(ITL1$ITL121NM == "Scotland"),]

NE = st_as_sf(NE, crs="EPSG:27700")
NW = st_as_sf(NW, crs="EPSG:27700")
YO = st_as_sf(YO, crs="EPSG:27700")
EM = st_as_sf(EM, crs="EPSG:27700")
WM = st_as_sf(WM, crs="EPSG:27700")
E = st_as_sf(E, crs="EPSG:27700")
LO = st_as_sf(LO, crs="EPSG:27700")
SE = st_as_sf(SE, crs="EPSG:27700")
SW = st_as_sf(SW, crs="EPSG:27700")
WA = st_as_sf(WA, crs="EPSG:27700")
SC = st_as_sf(SC, crs="EPSG:27700")

# Filtering store locations using each version into the regions
# North East
v30_NE <- st_filter(v30, NE)
v23_NE <- st_filter(v23, NE)
v3_NE <- st_filter(v3, NE)
# North West
v30_NW <- st_filter(v30, NW)
v23_NW <- st_filter(v23, NW)
v3_NW <- st_filter(v3, NW)
# Yorkshire
v30_YO <- st_filter(v30, YO)
v23_YO <- st_filter(v23, YO)
v3_YO <- st_filter(v3, YO)
# East Midlands
v30_EM <- st_filter(v30, EM)
v23_EM <- st_filter(v23, EM)
v3_EM <- st_filter(v3, EM)
# West Midlands
v30_WM <- st_filter(v30, WM)
v23_WM <- st_filter(v23, WM)
v3_WM <- st_filter(v3, WM)
# East
v30_E <- st_filter(v30, E)
v23_E <- st_filter(v23, E)
v3_E <- st_filter(v3, E)
# London
v30_LO <- st_filter(v30, LO)
v23_LO <- st_filter(v23, LO)
v3_LO <- st_filter(v3, LO)
# South East
v30_SE <- st_filter(v30, SE)
v23_SE <- st_filter(v23, SE)
v3_SE <- st_filter(v3, SE)
# South West
v30_SW <- st_filter(v30, SW)
v23_SW <- st_filter(v23, SW)
v3_SW <- st_filter(v3, SW)
# Wales
v30_WA <- st_filter(v30, WA)
v23_WA <- st_filter(v23, WA)
v3_WA <- st_filter(v3, WA)
# Scotland
v30_SC <- st_filter(v30, SC)
v23_SC <- st_filter(v23, SC)
v3_SC <- st_filter(v3, SC)

#filtering discounters and Big 4 retailers, then making buffers for stores within each region
#North East
#v30
v30_AL_NE <- subset(v30_NE, fascia == "Aldi")
v30_LI_NE <- subset(v30_NE, fascia == "Lidl")
v30_DISC_NE <- rbind(v30_AL_NE, v30_LI_NE)
rm(v30_AL_NE)
rm(v30_LI_NE)

v30_SA_NE <- subset(v30_NE, fascia == "Sainsburys")
v30_MO_NE <- subset(v30_NE, fascia == "Morrisons")
v30_TE_NE <- subset(v30_NE, fascia == "Tesco")
v30_AS_NE <- subset(v30_NE, fascia == "Asda")
v30_B4_NE <- rbind(v30_SA_NE, v30_MO_NE, v30_TE_NE, v30_AS_NE)
rm(v30_SA_NE)
rm(v30_MO_NE)
rm(v30_TE_NE)
rm(v30_AS_NE)

#v23
v23_AL_NE <- subset(v23_NE, fascia == "Aldi")
v23_LI_NE <- subset(v23_NE, fascia == "Lidl")
v23_DISC_NE <- rbind(v23_AL_NE, v23_LI_NE)
rm(v23_AL_NE)
rm(v23_LI_NE)

v23_SA_NE <- subset(v23_NE, fascia == "Sainsburys")
v23_MO_NE <- subset(v23_NE, fascia == "Morrisons")
v23_TE_NE <- subset(v23_NE, fascia == "Tesco")
v23_AS_NE <- subset(v23_NE, fascia == "Asda")
v23_B4_NE <- rbind(v23_SA_NE, v23_MO_NE, v23_TE_NE, v23_AS_NE)
rm(v23_SA_NE)
rm(v23_MO_NE)
rm(v23_TE_NE)
rm(v23_AS_NE)

#v3
v3_AL_NE <- subset(v3_NE, Fascia == "Aldi")
v3_LI_NE <- subset(v3_NE, Fascia == "Lidl")
v3_DISC_NE <- rbind(v3_AL_NE, v3_LI_NE)
rm(v3_AL_NE)
rm(v3_LI_NE)

v3_SA_NE <- subset(v3_NE, Fascia == "Sainsburys")
v3_MO_NE <- subset(v3_NE, Fascia == "Morrisons")
v3_TE_NE <- subset(v3_NE, Fascia == "Tesco")
v3_AS_NE <- subset(v3_NE, Fascia == "Asda")
v3_B4_NE <- rbind(v3_SA_NE, v3_MO_NE, v3_TE_NE, v3_AS_NE)
rm(v3_SA_NE)
rm(v3_MO_NE)
rm(v3_TE_NE)
rm(v3_AS_NE)

# Buffers at 500m
#v30
v30_DISC_NE_Buffers500m <- st_buffer(v30_DISC_NE, 500)
v30_DISC_NE_Buffers500m <- st_union(v30_DISC_NE_Buffers500m)

v30_B4_NE_Buffers500m <- st_buffer(v30_B4_NE, 500)
v30_B4_NE_Buffers500m <- st_union(v30_B4_NE_Buffers500m)

#v23
v23_DISC_NE_Buffers500m <- st_buffer(v23_DISC_NE, 500)
v23_DISC_NE_Buffers500m <- st_union(v23_DISC_NE_Buffers500m)

v23_B4_NE_Buffers500m <- st_buffer(v23_B4_NE, 500)
v23_B4_NE_Buffers500m <- st_union(v23_B4_NE_Buffers500m)

#v3
v3_DISC_NE_Buffers500m <- st_buffer(v3_DISC_NE, 500)
v3_DISC_NE_Buffers500m <- st_union(v3_DISC_NE_Buffers500m)

v3_B4_NE_Buffers500m <- st_buffer(v3_B4_NE, 500)
v3_B4_NE_Buffers500m <- st_union(v3_B4_NE_Buffers500m)

#North West
#v30
v30_AL_NW <- subset(v30_NW, fascia == "Aldi")
v30_LI_NW <- subset(v30_NW, fascia == "Lidl")
v30_DISC_NW <- rbind(v30_AL_NW, v30_LI_NW)
rm(v30_AL_NW)
rm(v30_LI_NW)

v30_SA_NW <- subset(v30_NW, fascia == "Sainsburys")
v30_MO_NW <- subset(v30_NW, fascia == "Morrisons")
v30_TE_NW <- subset(v30_NW, fascia == "Tesco")
v30_AS_NW <- subset(v30_NW, fascia == "Asda")
v30_B4_NW <- rbind(v30_SA_NW, v30_MO_NW, v30_TE_NW, v30_AS_NW)
rm(v30_SA_NW)
rm(v30_MO_NW)
rm(v30_TE_NW)
rm(v30_AS_NW)

#v23
v23_AL_NW <- subset(v23_NW, fascia == "Aldi")
v23_LI_NW <- subset(v23_NW, fascia == "Lidl")
v23_DISC_NW <- rbind(v23_AL_NW, v23_LI_NW)
rm(v23_AL_NW)
rm(v23_LI_NW)

v23_SA_NW <- subset(v23_NW, fascia == "Sainsburys")
v23_MO_NW <- subset(v23_NW, fascia == "Morrisons")
v23_TE_NW <- subset(v23_NW, fascia == "Tesco")
v23_AS_NW <- subset(v23_NW, fascia == "Asda")
v23_B4_NW <- rbind(v23_SA_NW, v23_MO_NW, v23_TE_NW, v23_AS_NW)
rm(v23_SA_NW)
rm(v23_MO_NW)
rm(v23_TE_NW)
rm(v23_AS_NW)

#v3
v3_AL_NW <- subset(v3_NW, Fascia == "Aldi")
v3_LI_NW <- subset(v3_NW, Fascia == "Lidl")
v3_DISC_NW <- rbind(v3_AL_NW, v3_LI_NW)
rm(v3_AL_NW)
rm(v3_LI_NW)

v3_SA_NW <- subset(v3_NW, Fascia == "Sainsburys")
v3_MO_NW <- subset(v3_NW, Fascia == "Morrisons")
v3_TE_NW <- subset(v3_NW, Fascia == "Tesco")
v3_AS_NW <- subset(v3_NW, Fascia == "Asda")
v3_B4_NW <- rbind(v3_SA_NW, v3_MO_NW, v3_TE_NW, v3_AS_NW)
rm(v3_SA_NW)
rm(v3_MO_NW)
rm(v3_TE_NW)
rm(v3_AS_NW)

# Buffers at 500m
#v30
v30_DISC_NW_Buffers500m <- st_buffer(v30_DISC_NW, 500)
v30_DISC_NW_Buffers500m <- st_union(v30_DISC_NW_Buffers500m)

v30_B4_NW_Buffers500m <- st_buffer(v30_B4_NW, 500)
v30_B4_NW_Buffers500m <- st_union(v30_B4_NW_Buffers500m)

#v23
v23_DISC_NW_Buffers500m <- st_buffer(v23_DISC_NW, 500)
v23_DISC_NW_Buffers500m <- st_union(v23_DISC_NW_Buffers500m)

v23_B4_NW_Buffers500m <- st_buffer(v23_B4_NW, 500)
v23_B4_NW_Buffers500m <- st_union(v23_B4_NW_Buffers500m)

#v3
v3_DISC_NW_Buffers500m <- st_buffer(v3_DISC_NW, 500)
v3_DISC_NW_Buffers500m <- st_union(v3_DISC_NW_Buffers500m)

v3_B4_NW_Buffers500m <- st_buffer(v3_B4_NW, 500)
v3_B4_NW_Buffers500m <- st_union(v3_B4_NW_Buffers500m)

#Yorkshire
#v30
v30_AL_YO <- subset(v30_YO, fascia == "Aldi")
v30_LI_YO <- subset(v30_YO, fascia == "Lidl")
v30_DISC_YO <- rbind(v30_AL_YO, v30_LI_YO)
rm(v30_AL_YO)
rm(v30_LI_YO)

v30_SA_YO <- subset(v30_YO, fascia == "Sainsburys")
v30_MO_YO <- subset(v30_YO, fascia == "Morrisons")
v30_TE_YO <- subset(v30_YO, fascia == "Tesco")
v30_AS_YO <- subset(v30_YO, fascia == "Asda")
v30_B4_YO <- rbind(v30_SA_YO, v30_MO_YO, v30_TE_YO, v30_AS_YO)
rm(v30_SA_YO)
rm(v30_MO_YO)
rm(v30_TE_YO)
rm(v30_AS_YO)

#v23
v23_AL_YO <- subset(v23_YO, fascia == "Aldi")
v23_LI_YO <- subset(v23_YO, fascia == "Lidl")
v23_DISC_YO <- rbind(v23_AL_YO, v23_LI_YO)
rm(v23_AL_YO)
rm(v23_LI_YO)

v23_SA_YO <- subset(v23_YO, fascia == "Sainsburys")
v23_MO_YO <- subset(v23_YO, fascia == "Morrisons")
v23_TE_YO <- subset(v23_YO, fascia == "Tesco")
v23_AS_YO <- subset(v23_YO, fascia == "Asda")
v23_B4_YO <- rbind(v23_SA_YO, v23_MO_YO, v23_TE_YO, v23_AS_YO)
rm(v23_SA_YO)
rm(v23_MO_YO)
rm(v23_TE_YO)
rm(v23_AS_YO)

#v3
v3_AL_YO <- subset(v3_YO, Fascia == "Aldi")
v3_LI_YO <- subset(v3_YO, Fascia == "Lidl")
v3_DISC_YO <- rbind(v3_AL_YO, v3_LI_YO)
rm(v3_AL_YO)
rm(v3_LI_YO)

v3_SA_YO <- subset(v3_YO, Fascia == "Sainsburys")
v3_MO_YO <- subset(v3_YO, Fascia == "Morrisons")
v3_TE_YO <- subset(v3_YO, Fascia == "Tesco")
v3_AS_YO <- subset(v3_YO, Fascia == "Asda")
v3_B4_YO <- rbind(v3_SA_YO, v3_MO_YO, v3_TE_YO, v3_AS_YO)
rm(v3_SA_YO)
rm(v3_MO_YO)
rm(v3_TE_YO)
rm(v3_AS_YO)

# Buffers at 500m
#v30
v30_DISC_YO_Buffers500m <- st_buffer(v30_DISC_YO, 500)
v30_DISC_YO_Buffers500m <- st_union(v30_DISC_YO_Buffers500m)

v30_B4_YO_Buffers500m <- st_buffer(v30_B4_YO, 500)
v30_B4_YO_Buffers500m <- st_union(v30_B4_YO_Buffers500m)

#v23
v23_DISC_YO_Buffers500m <- st_buffer(v23_DISC_YO, 500)
v23_DISC_YO_Buffers500m <- st_union(v23_DISC_YO_Buffers500m)

v23_B4_YO_Buffers500m <- st_buffer(v23_B4_YO, 500)
v23_B4_YO_Buffers500m <- st_union(v23_B4_YO_Buffers500m)

#v3
v3_DISC_YO_Buffers500m <- st_buffer(v3_DISC_YO, 500)
v3_DISC_YO_Buffers500m <- st_union(v3_DISC_YO_Buffers500m)

v3_B4_YO_Buffers500m <- st_buffer(v3_B4_YO, 500)
v3_B4_YO_Buffers500m <- st_union(v3_B4_YO_Buffers500m)

#East Midlands
#v30
v30_AL_EM <- subset(v30_EM, fascia == "Aldi")
v30_LI_EM <- subset(v30_EM, fascia == "Lidl")
v30_DISC_EM <- rbind(v30_AL_EM, v30_LI_EM)
rm(v30_AL_EM)
rm(v30_LI_EM)

v30_SA_EM <- subset(v30_EM, fascia == "Sainsburys")
v30_MO_EM <- subset(v30_EM, fascia == "Morrisons")
v30_TE_EM <- subset(v30_EM, fascia == "Tesco")
v30_AS_EM <- subset(v30_EM, fascia == "Asda")
v30_B4_EM <- rbind(v30_SA_EM, v30_MO_EM, v30_TE_EM, v30_AS_EM)
rm(v30_SA_EM)
rm(v30_MO_EM)
rm(v30_TE_EM)
rm(v30_AS_EM)

#v23
v23_AL_EM <- subset(v23_EM, fascia == "Aldi")
v23_LI_EM <- subset(v23_EM, fascia == "Lidl")
v23_DISC_EM <- rbind(v23_AL_EM, v23_LI_EM)
rm(v23_AL_EM)
rm(v23_LI_EM)

v23_SA_EM <- subset(v23_EM, fascia == "Sainsburys")
v23_MO_EM <- subset(v23_EM, fascia == "Morrisons")
v23_TE_EM <- subset(v23_EM, fascia == "Tesco")
v23_AS_EM <- subset(v23_EM, fascia == "Asda")
v23_B4_EM <- rbind(v23_SA_EM, v23_MO_EM, v23_TE_EM, v23_AS_EM)
rm(v23_SA_EM)
rm(v23_MO_EM)
rm(v23_TE_EM)
rm(v23_AS_EM)

#v3
v3_AL_EM <- subset(v3_EM, Fascia == "Aldi")
v3_LI_EM <- subset(v3_EM, Fascia == "Lidl")
v3_DISC_EM <- rbind(v3_AL_EM, v3_LI_EM)
rm(v3_AL_EM)
rm(v3_LI_EM)

v3_SA_EM <- subset(v3_EM, Fascia == "Sainsburys")
v3_MO_EM <- subset(v3_EM, Fascia == "Morrisons")
v3_TE_EM <- subset(v3_EM, Fascia == "Tesco")
v3_AS_EM <- subset(v3_EM, Fascia == "Asda")
v3_B4_EM <- rbind(v3_SA_EM, v3_MO_EM, v3_TE_EM, v3_AS_EM)
rm(v3_SA_EM)
rm(v3_MO_EM)
rm(v3_TE_EM)
rm(v3_AS_EM)

# Buffers at 500m
#v30
v30_DISC_EM_Buffers500m <- st_buffer(v30_DISC_EM, 500)
v30_DISC_EM_Buffers500m <- st_union(v30_DISC_EM_Buffers500m)

v30_B4_EM_Buffers500m <- st_buffer(v30_B4_EM, 500)
v30_B4_EM_Buffers500m <- st_union(v30_B4_EM_Buffers500m)

#v23
v23_DISC_EM_Buffers500m <- st_buffer(v23_DISC_EM, 500)
v23_DISC_EM_Buffers500m <- st_union(v23_DISC_EM_Buffers500m)

v23_B4_EM_Buffers500m <- st_buffer(v23_B4_EM, 500)
v23_B4_EM_Buffers500m <- st_union(v23_B4_EM_Buffers500m)

#v3
v3_DISC_EM_Buffers500m <- st_buffer(v3_DISC_EM, 500)
v3_DISC_EM_Buffers500m <- st_union(v3_DISC_EM_Buffers500m)

v3_B4_EM_Buffers500m <- st_buffer(v3_B4_EM, 500)
v3_B4_EM_Buffers500m <- st_union(v3_B4_EM_Buffers500m)

#West Midlands
#v30
v30_AL_WM <- subset(v30_WM, fascia == "Aldi")
v30_LI_WM <- subset(v30_WM, fascia == "Lidl")
v30_DISC_WM <- rbind(v30_AL_WM, v30_LI_WM)
rm(v30_AL_WM)
rm(v30_LI_WM)

v30_SA_WM <- subset(v30_WM, fascia == "Sainsburys")
v30_MO_WM <- subset(v30_WM, fascia == "Morrisons")
v30_TE_WM <- subset(v30_WM, fascia == "Tesco")
v30_AS_WM <- subset(v30_WM, fascia == "Asda")
v30_B4_WM <- rbind(v30_SA_WM, v30_MO_WM, v30_TE_WM, v30_AS_WM)
rm(v30_SA_WM)
rm(v30_MO_WM)
rm(v30_TE_WM)
rm(v30_AS_WM)

#v23
v23_AL_WM <- subset(v23_WM, fascia == "Aldi")
v23_LI_WM <- subset(v23_WM, fascia == "Lidl")
v23_DISC_WM <- rbind(v23_AL_WM, v23_LI_WM)
rm(v23_AL_WM)
rm(v23_LI_WM)

v23_SA_WM <- subset(v23_WM, fascia == "Sainsburys")
v23_MO_WM <- subset(v23_WM, fascia == "Morrisons")
v23_TE_WM <- subset(v23_WM, fascia == "Tesco")
v23_AS_WM <- subset(v23_WM, fascia == "Asda")
v23_B4_WM <- rbind(v23_SA_WM, v23_MO_WM, v23_TE_WM, v23_AS_WM)
rm(v23_SA_WM)
rm(v23_MO_WM)
rm(v23_TE_WM)
rm(v23_AS_WM)

#v3
v3_AL_WM <- subset(v3_WM, Fascia == "Aldi")
v3_LI_WM <- subset(v3_WM, Fascia == "Lidl")
v3_DISC_WM <- rbind(v3_AL_WM, v3_LI_WM)
rm(v3_AL_WM)
rm(v3_LI_WM)

v3_SA_WM <- subset(v3_WM, Fascia == "Sainsburys")
v3_MO_WM <- subset(v3_WM, Fascia == "Morrisons")
v3_TE_WM <- subset(v3_WM, Fascia == "Tesco")
v3_AS_WM <- subset(v3_WM, Fascia == "Asda")
v3_B4_WM <- rbind(v3_SA_WM, v3_MO_WM, v3_TE_WM, v3_AS_WM)
rm(v3_SA_WM)
rm(v3_MO_WM)
rm(v3_TE_WM)
rm(v3_AS_WM)

# Buffers at 500m
#v30
v30_DISC_WM_Buffers500m <- st_buffer(v30_DISC_WM, 500)
v30_DISC_WM_Buffers500m <- st_union(v30_DISC_WM_Buffers500m)

v30_B4_WM_Buffers500m <- st_buffer(v30_B4_WM, 500)
v30_B4_WM_Buffers500m <- st_union(v30_B4_WM_Buffers500m)

#v23
v23_DISC_WM_Buffers500m <- st_buffer(v23_DISC_WM, 500)
v23_DISC_WM_Buffers500m <- st_union(v23_DISC_WM_Buffers500m)

v23_B4_WM_Buffers500m <- st_buffer(v23_B4_WM, 500)
v23_B4_WM_Buffers500m <- st_union(v23_B4_WM_Buffers500m)

#v3
v3_DISC_WM_Buffers500m <- st_buffer(v3_DISC_WM, 500)
v3_DISC_WM_Buffers500m <- st_union(v3_DISC_WM_Buffers500m)

v3_B4_WM_Buffers500m <- st_buffer(v3_B4_WM, 500)
v3_B4_WM_Buffers500m <- st_union(v3_B4_WM_Buffers500m)

#East
#v30
v30_AL_E <- subset(v30_E, fascia == "Aldi")
v30_LI_E <- subset(v30_E, fascia == "Lidl")
v30_DISC_E <- rbind(v30_AL_E, v30_LI_E)
rm(v30_AL_E)
rm(v30_LI_E)

v30_SA_E <- subset(v30_E, fascia == "Sainsburys")
v30_MO_E <- subset(v30_E, fascia == "Morrisons")
v30_TE_E <- subset(v30_E, fascia == "Tesco")
v30_AS_E <- subset(v30_E, fascia == "Asda")
v30_B4_E <- rbind(v30_SA_E, v30_MO_E, v30_TE_E, v30_AS_E)
rm(v30_SA_E)
rm(v30_MO_E)
rm(v30_TE_E)
rm(v30_AS_E)

#v23
v23_AL_E <- subset(v23_E, fascia == "Aldi")
v23_LI_E <- subset(v23_E, fascia == "Lidl")
v23_DISC_E <- rbind(v23_AL_E, v23_LI_E)
rm(v23_AL_E)
rm(v23_LI_E)

v23_SA_E <- subset(v23_E, fascia == "Sainsburys")
v23_MO_E <- subset(v23_E, fascia == "Morrisons")
v23_TE_E <- subset(v23_E, fascia == "Tesco")
v23_AS_E <- subset(v23_E, fascia == "Asda")
v23_B4_E <- rbind(v23_SA_E, v23_MO_E, v23_TE_E, v23_AS_E)
rm(v23_SA_E)
rm(v23_MO_E)
rm(v23_TE_E)
rm(v23_AS_E)

#v3
v3_AL_E <- subset(v3_E, Fascia == "Aldi")
v3_LI_E <- subset(v3_E, Fascia == "Lidl")
v3_DISC_E <- rbind(v3_AL_E, v3_LI_E)
rm(v3_AL_E)
rm(v3_LI_E)

v3_SA_E <- subset(v3_E, Fascia == "Sainsburys")
v3_MO_E <- subset(v3_E, Fascia == "Morrisons")
v3_TE_E <- subset(v3_E, Fascia == "Tesco")
v3_AS_E <- subset(v3_E, Fascia == "Asda")
v3_B4_E <- rbind(v3_SA_E, v3_MO_E, v3_TE_E, v3_AS_E)
rm(v3_SA_E)
rm(v3_MO_E)
rm(v3_TE_E)
rm(v3_AS_E)

# Buffers at 500m
#v30
v30_DISC_E_Buffers500m <- st_buffer(v30_DISC_E, 500)
v30_DISC_E_Buffers500m <- st_union(v30_DISC_E_Buffers500m)

v30_B4_E_Buffers500m <- st_buffer(v30_B4_E, 500)
v30_B4_E_Buffers500m <- st_union(v30_B4_E_Buffers500m)

#v23
v23_DISC_E_Buffers500m <- st_buffer(v23_DISC_E, 500)
v23_DISC_E_Buffers500m <- st_union(v23_DISC_E_Buffers500m)

v23_B4_E_Buffers500m <- st_buffer(v23_B4_E, 500)
v23_B4_E_Buffers500m <- st_union(v23_B4_E_Buffers500m)

#v3
v3_DISC_E_Buffers500m <- st_buffer(v3_DISC_E, 500)
v3_DISC_E_Buffers500m <- st_union(v3_DISC_E_Buffers500m)

v3_B4_E_Buffers500m <- st_buffer(v3_B4_E, 500)
v3_B4_E_Buffers500m <- st_union(v3_B4_E_Buffers500m)

#London
#v30
v30_AL_LO <- subset(v30_LO, fascia == "Aldi")
v30_LI_LO <- subset(v30_LO, fascia == "Lidl")
v30_DISC_LO <- rbind(v30_AL_LO, v30_LI_LO)
rm(v30_AL_LO)
rm(v30_LI_LO)

v30_SA_LO <- subset(v30_LO, fascia == "Sainsburys")
v30_MO_LO <- subset(v30_LO, fascia == "Morrisons")
v30_TE_LO <- subset(v30_LO, fascia == "Tesco")
v30_AS_LO <- subset(v30_LO, fascia == "Asda")
v30_B4_LO <- rbind(v30_SA_LO, v30_MO_LO, v30_TE_LO, v30_AS_LO)
rm(v30_SA_LO)
rm(v30_MO_LO)
rm(v30_TE_LO)
rm(v30_AS_LO)

#v23
v23_AL_LO <- subset(v23_LO, fascia == "Aldi")
v23_LI_LO <- subset(v23_LO, fascia == "Lidl")
v23_DISC_LO <- rbind(v23_AL_LO, v23_LI_LO)
rm(v23_AL_LO)
rm(v23_LI_LO)

v23_SA_LO <- subset(v23_LO, fascia == "Sainsburys")
v23_MO_LO <- subset(v23_LO, fascia == "Morrisons")
v23_TE_LO <- subset(v23_LO, fascia == "Tesco")
v23_AS_LO <- subset(v23_LO, fascia == "Asda")
v23_B4_LO <- rbind(v23_SA_LO, v23_MO_LO, v23_TE_LO, v23_AS_LO)
rm(v23_SA_LO)
rm(v23_MO_LO)
rm(v23_TE_LO)
rm(v23_AS_LO)

#v3
v3_AL_LO <- subset(v3_LO, Fascia == "Aldi")
v3_LI_LO <- subset(v3_LO, Fascia == "Lidl")
v3_DISC_LO <- rbind(v3_AL_LO, v3_LI_LO)
rm(v3_AL_LO)
rm(v3_LI_LO)

v3_SA_LO <- subset(v3_LO, Fascia == "Sainsburys")
v3_MO_LO <- subset(v3_LO, Fascia == "Morrisons")
v3_TE_LO <- subset(v3_LO, Fascia == "Tesco")
v3_AS_LO <- subset(v3_LO, Fascia == "Asda")
v3_B4_LO <- rbind(v3_SA_LO, v3_MO_LO, v3_TE_LO, v3_AS_LO)
rm(v3_SA_LO)
rm(v3_MO_LO)
rm(v3_TE_LO)
rm(v3_AS_LO)

# Buffers at 500m
#v30
v30_DISC_LO_Buffers500m <- st_buffer(v30_DISC_LO, 500)
v30_DISC_LO_Buffers500m <- st_union(v30_DISC_LO_Buffers500m)

v30_B4_LO_Buffers500m <- st_buffer(v30_B4_LO, 500)
v30_B4_LO_Buffers500m <- st_union(v30_B4_LO_Buffers500m)

#v23
v23_DISC_LO_Buffers500m <- st_buffer(v23_DISC_LO, 500)
v23_DISC_LO_Buffers500m <- st_union(v23_DISC_LO_Buffers500m)

v23_B4_LO_Buffers500m <- st_buffer(v23_B4_LO, 500)
v23_B4_LO_Buffers500m <- st_union(v23_B4_LO_Buffers500m)

#v3
v3_DISC_LO_Buffers500m <- st_buffer(v3_DISC_LO, 500)
v3_DISC_LO_Buffers500m <- st_union(v3_DISC_LO_Buffers500m)

v3_B4_LO_Buffers500m <- st_buffer(v3_B4_LO, 500)
v3_B4_LO_Buffers500m <- st_union(v3_B4_LO_Buffers500m)

#South East
#v30
v30_AL_SE <- subset(v30_SE, fascia == "Aldi")
v30_LI_SE <- subset(v30_SE, fascia == "Lidl")
v30_DISC_SE <- rbind(v30_AL_SE, v30_LI_SE)
rm(v30_AL_SE)
rm(v30_LI_SE)

v30_SA_SE <- subset(v30_SE, fascia == "Sainsburys")
v30_MO_SE <- subset(v30_SE, fascia == "Morrisons")
v30_TE_SE <- subset(v30_SE, fascia == "Tesco")
v30_AS_SE <- subset(v30_SE, fascia == "Asda")
v30_B4_SE <- rbind(v30_SA_SE, v30_MO_SE, v30_TE_SE, v30_AS_SE)
rm(v30_SA_SE)
rm(v30_MO_SE)
rm(v30_TE_SE)
rm(v30_AS_SE)

#v23
v23_AL_SE <- subset(v23_SE, fascia == "Aldi")
v23_LI_SE <- subset(v23_SE, fascia == "Lidl")
v23_DISC_SE <- rbind(v23_AL_SE, v23_LI_SE)
rm(v23_AL_SE)
rm(v23_LI_SE)

v23_SA_SE <- subset(v23_SE, fascia == "Sainsburys")
v23_MO_SE <- subset(v23_SE, fascia == "Morrisons")
v23_TE_SE <- subset(v23_SE, fascia == "Tesco")
v23_AS_SE <- subset(v23_SE, fascia == "Asda")
v23_B4_SE <- rbind(v23_SA_SE, v23_MO_SE, v23_TE_SE, v23_AS_SE)
rm(v23_SA_SE)
rm(v23_MO_SE)
rm(v23_TE_SE)
rm(v23_AS_SE)

#v3
v3_AL_SE <- subset(v3_SE, Fascia == "Aldi")
v3_LI_SE <- subset(v3_SE, Fascia == "Lidl")
v3_DISC_SE <- rbind(v3_AL_SE, v3_LI_SE)
rm(v3_AL_SE)
rm(v3_LI_SE)

v3_SA_SE <- subset(v3_SE, Fascia == "Sainsburys")
v3_MO_SE <- subset(v3_SE, Fascia == "Morrisons")
v3_TE_SE <- subset(v3_SE, Fascia == "Tesco")
v3_AS_SE <- subset(v3_SE, Fascia == "Asda")
v3_B4_SE <- rbind(v3_SA_SE, v3_MO_SE, v3_TE_SE, v3_AS_SE)
rm(v3_SA_SE)
rm(v3_MO_SE)
rm(v3_TE_SE)
rm(v3_AS_SE)

# Buffers at 500m
#v30
v30_DISC_SE_Buffers500m <- st_buffer(v30_DISC_SE, 500)
v30_DISC_SE_Buffers500m <- st_union(v30_DISC_SE_Buffers500m)

v30_B4_SE_Buffers500m <- st_buffer(v30_B4_SE, 500)
v30_B4_SE_Buffers500m <- st_union(v30_B4_SE_Buffers500m)

#v23
v23_DISC_SE_Buffers500m <- st_buffer(v23_DISC_SE, 500)
v23_DISC_SE_Buffers500m <- st_union(v23_DISC_SE_Buffers500m)

v23_B4_SE_Buffers500m <- st_buffer(v23_B4_SE, 500)
v23_B4_SE_Buffers500m <- st_union(v23_B4_SE_Buffers500m)

#v3
v3_DISC_SE_Buffers500m <- st_buffer(v3_DISC_SE, 500)
v3_DISC_SE_Buffers500m <- st_union(v3_DISC_SE_Buffers500m)

v3_B4_SE_Buffers500m <- st_buffer(v3_B4_SE, 500)
v3_B4_SE_Buffers500m <- st_union(v3_B4_SE_Buffers500m)

#South West
#v30
v30_AL_SW <- subset(v30_SW, fascia == "Aldi")
v30_LI_SW <- subset(v30_SW, fascia == "Lidl")
v30_DISC_SW <- rbind(v30_AL_SW, v30_LI_SW)
rm(v30_AL_SW)
rm(v30_LI_SW)

v30_SA_SW <- subset(v30_SW, fascia == "Sainsburys")
v30_MO_SW <- subset(v30_SW, fascia == "Morrisons")
v30_TE_SW <- subset(v30_SW, fascia == "Tesco")
v30_AS_SW <- subset(v30_SW, fascia == "Asda")
v30_B4_SW <- rbind(v30_SA_SW, v30_MO_SW, v30_TE_SW, v30_AS_SW)
rm(v30_SA_SW)
rm(v30_MO_SW)
rm(v30_TE_SW)
rm(v30_AS_SW)

#v23
v23_AL_SW <- subset(v23_SW, fascia == "Aldi")
v23_LI_SW <- subset(v23_SW, fascia == "Lidl")
v23_DISC_SW <- rbind(v23_AL_SW, v23_LI_SW)
rm(v23_AL_SW)
rm(v23_LI_SW)

v23_SA_SW <- subset(v23_SW, fascia == "Sainsburys")
v23_MO_SW <- subset(v23_SW, fascia == "Morrisons")
v23_TE_SW <- subset(v23_SW, fascia == "Tesco")
v23_AS_SW <- subset(v23_SW, fascia == "Asda")
v23_B4_SW <- rbind(v23_SA_SW, v23_MO_SW, v23_TE_SW, v23_AS_SW)
rm(v23_SA_SW)
rm(v23_MO_SW)
rm(v23_TE_SW)
rm(v23_AS_SW)

#v3
v3_AL_SW <- subset(v3_SW, Fascia == "Aldi")
v3_LI_SW <- subset(v3_SW, Fascia == "Lidl")
v3_DISC_SW <- rbind(v3_AL_SW, v3_LI_SW)
rm(v3_AL_SW)
rm(v3_LI_SW)

v3_SA_SW <- subset(v3_SW, Fascia == "Sainsburys")
v3_MO_SW <- subset(v3_SW, Fascia == "Morrisons")
v3_TE_SW <- subset(v3_SW, Fascia == "Tesco")
v3_AS_SW <- subset(v3_SW, Fascia == "Asda")
v3_B4_SW <- rbind(v3_SA_SW, v3_MO_SW, v3_TE_SW, v3_AS_SW)
rm(v3_SA_SW)
rm(v3_MO_SW)
rm(v3_TE_SW)
rm(v3_AS_SW)

# Buffers at 500m
#v30
v30_DISC_SW_Buffers500m <- st_buffer(v30_DISC_SW, 500)
v30_DISC_SW_Buffers500m <- st_union(v30_DISC_SW_Buffers500m)

v30_B4_SW_Buffers500m <- st_buffer(v30_B4_SW, 500)
v30_B4_SW_Buffers500m <- st_union(v30_B4_SW_Buffers500m)

#v23
v23_DISC_SW_Buffers500m <- st_buffer(v23_DISC_SW, 500)
v23_DISC_SW_Buffers500m <- st_union(v23_DISC_SW_Buffers500m)

v23_B4_SW_Buffers500m <- st_buffer(v23_B4_SW, 500)
v23_B4_SW_Buffers500m <- st_union(v23_B4_SW_Buffers500m)

#v3
v3_DISC_SW_Buffers500m <- st_buffer(v3_DISC_SW, 500)
v3_DISC_SW_Buffers500m <- st_union(v3_DISC_SW_Buffers500m)

v3_B4_SW_Buffers500m <- st_buffer(v3_B4_SW, 500)
v3_B4_SW_Buffers500m <- st_union(v3_B4_SW_Buffers500m)

#Wales
#v30
v30_AL_WA <- subset(v30_WA, fascia == "Aldi")
v30_LI_WA <- subset(v30_WA, fascia == "Lidl")
v30_DISC_WA <- rbind(v30_AL_WA, v30_LI_WA)
rm(v30_AL_WA)
rm(v30_LI_WA)

v30_SA_WA <- subset(v30_WA, fascia == "Sainsburys")
v30_MO_WA <- subset(v30_WA, fascia == "Morrisons")
v30_TE_WA <- subset(v30_WA, fascia == "Tesco")
v30_AS_WA <- subset(v30_WA, fascia == "Asda")
v30_B4_WA <- rbind(v30_SA_WA, v30_MO_WA, v30_TE_WA, v30_AS_WA)
rm(v30_SA_WA)
rm(v30_MO_WA)
rm(v30_TE_WA)
rm(v30_AS_WA)

#v23
v23_AL_WA <- subset(v23_WA, fascia == "Aldi")
v23_LI_WA <- subset(v23_WA, fascia == "Lidl")
v23_DISC_WA <- rbind(v23_AL_WA, v23_LI_WA)
rm(v23_AL_WA)
rm(v23_LI_WA)

v23_SA_WA <- subset(v23_WA, fascia == "Sainsburys")
v23_MO_WA <- subset(v23_WA, fascia == "Morrisons")
v23_TE_WA <- subset(v23_WA, fascia == "Tesco")
v23_AS_WA <- subset(v23_WA, fascia == "Asda")
v23_B4_WA <- rbind(v23_SA_WA, v23_MO_WA, v23_TE_WA, v23_AS_WA)
rm(v23_SA_WA)
rm(v23_MO_WA)
rm(v23_TE_WA)
rm(v23_AS_WA)

#v3
v3_AL_WA <- subset(v3_WA, Fascia == "Aldi")
v3_LI_WA <- subset(v3_WA, Fascia == "Lidl")
v3_DISC_WA <- rbind(v3_AL_WA, v3_LI_WA)
rm(v3_AL_WA)
rm(v3_LI_WA)

v3_SA_WA <- subset(v3_WA, Fascia == "Sainsburys")
v3_MO_WA <- subset(v3_WA, Fascia == "Morrisons")
v3_TE_WA <- subset(v3_WA, Fascia == "Tesco")
v3_AS_WA <- subset(v3_WA, Fascia == "Asda")
v3_B4_WA <- rbind(v3_SA_WA, v3_MO_WA, v3_TE_WA, v3_AS_WA)
rm(v3_SA_WA)
rm(v3_MO_WA)
rm(v3_TE_WA)
rm(v3_AS_WA)

# Buffers at 500m
#v30
v30_DISC_WA_Buffers500m <- st_buffer(v30_DISC_WA, 500)
v30_DISC_WA_Buffers500m <- st_union(v30_DISC_WA_Buffers500m)

v30_B4_WA_Buffers500m <- st_buffer(v30_B4_WA, 500)
v30_B4_WA_Buffers500m <- st_union(v30_B4_WA_Buffers500m)

#v23
v23_DISC_WA_Buffers500m <- st_buffer(v23_DISC_WA, 500)
v23_DISC_WA_Buffers500m <- st_union(v23_DISC_WA_Buffers500m)

v23_B4_WA_Buffers500m <- st_buffer(v23_B4_WA, 500)
v23_B4_WA_Buffers500m <- st_union(v23_B4_WA_Buffers500m)

#v3
v3_DISC_WA_Buffers500m <- st_buffer(v3_DISC_WA, 500)
v3_DISC_WA_Buffers500m <- st_union(v3_DISC_WA_Buffers500m)

v3_B4_WA_Buffers500m <- st_buffer(v3_B4_WA, 500)
v3_B4_WA_Buffers500m <- st_union(v3_B4_WA_Buffers500m)

#Scotland
#v30
v30_AL_SC <- subset(v30_SC, fascia == "Aldi")
v30_LI_SC <- subset(v30_SC, fascia == "Lidl")
v30_DISC_SC <- rbind(v30_AL_SC, v30_LI_SC)
rm(v30_AL_SC)
rm(v30_LI_SC)

v30_SA_SC <- subset(v30_SC, fascia == "Sainsburys")
v30_MO_SC <- subset(v30_SC, fascia == "Morrisons")
v30_TE_SC <- subset(v30_SC, fascia == "Tesco")
v30_AS_SC <- subset(v30_SC, fascia == "Asda")
v30_B4_SC <- rbind(v30_SA_SC, v30_MO_SC, v30_TE_SC, v30_AS_SC)
rm(v30_SA_SC)
rm(v30_MO_SC)
rm(v30_TE_SC)
rm(v30_AS_SC)

#v23
v23_AL_SC <- subset(v23_SC, fascia == "Aldi")
v23_LI_SC <- subset(v23_SC, fascia == "Lidl")
v23_DISC_SC <- rbind(v23_AL_SC, v23_LI_SC)
rm(v23_AL_SC)
rm(v23_LI_SC)

v23_SA_SC <- subset(v23_SC, fascia == "Sainsburys")
v23_MO_SC <- subset(v23_SC, fascia == "Morrisons")
v23_TE_SC <- subset(v23_SC, fascia == "Tesco")
v23_AS_SC <- subset(v23_SC, fascia == "Asda")
v23_B4_SC <- rbind(v23_SA_SC, v23_MO_SC, v23_TE_SC, v23_AS_SC)
rm(v23_SA_SC)
rm(v23_MO_SC)
rm(v23_TE_SC)
rm(v23_AS_SC)

#v3
v3_AL_SC <- subset(v3_SC, Fascia == "Aldi")
v3_LI_SC <- subset(v3_SC, Fascia == "Lidl")
v3_DISC_SC <- rbind(v3_AL_SC, v3_LI_SC)
rm(v3_AL_SC)
rm(v3_LI_SC)

v3_SA_SC <- subset(v3_SC, Fascia == "Sainsburys")
v3_MO_SC <- subset(v3_SC, Fascia == "Morrisons")
v3_TE_SC <- subset(v3_SC, Fascia == "Tesco")
v3_AS_SC <- subset(v3_SC, Fascia == "Asda")
v3_B4_SC <- rbind(v3_SA_SC, v3_MO_SC, v3_TE_SC, v3_AS_SC)
rm(v3_SA_SC)
rm(v3_MO_SC)
rm(v3_TE_SC)
rm(v3_AS_SC)

# Buffers at 500m
#v30
v30_DISC_SC_Buffers500m <- st_buffer(v30_DISC_SC, 500)
v30_DISC_SC_Buffers500m <- st_union(v30_DISC_SC_Buffers500m)

v30_B4_SC_Buffers500m <- st_buffer(v30_B4_SC, 500)
v30_B4_SC_Buffers500m <- st_union(v30_B4_SC_Buffers500m)

#v23
v23_DISC_SC_Buffers500m <- st_buffer(v23_DISC_SC, 500)
v23_DISC_SC_Buffers500m <- st_union(v23_DISC_SC_Buffers500m)

v23_B4_SC_Buffers500m <- st_buffer(v23_B4_SC, 500)
v23_B4_SC_Buffers500m <- st_union(v23_B4_SC_Buffers500m)

#v3
v3_DISC_SC_Buffers500m <- st_buffer(v3_DISC_SC, 500)
v3_DISC_SC_Buffers500m <- st_union(v3_DISC_SC_Buffers500m)

v3_B4_SC_Buffers500m <- st_buffer(v3_B4_SC, 500)
v3_B4_SC_Buffers500m <- st_union(v3_B4_SC_Buffers500m)

#Extract PWC points covered by buffers
#v30
v30_DISC_NE_PWC <- st_filter(PWC_demo, v30_DISC_NE_Buffers500m)
v30_DISC_NW_PWC <- st_filter(PWC_demo, v30_DISC_NW_Buffers500m)
v30_DISC_YO_PWC <- st_filter(PWC_demo, v30_DISC_YO_Buffers500m)
v30_DISC_EM_PWC <- st_filter(PWC_demo, v30_DISC_EM_Buffers500m)
v30_DISC_WM_PWC <- st_filter(PWC_demo, v30_DISC_WM_Buffers500m)
v30_DISC_E_PWC <- st_filter(PWC_demo, v30_DISC_E_Buffers500m)
v30_DISC_LO_PWC <- st_filter(PWC_demo, v30_DISC_LO_Buffers500m)
v30_DISC_SE_PWC <- st_filter(PWC_demo, v30_DISC_SE_Buffers500m)
v30_DISC_SW_PWC <- st_filter(PWC_demo, v30_DISC_SW_Buffers500m)
v30_DISC_WA_PWC <- st_filter(PWC_demo, v30_DISC_WA_Buffers500m)
v30_DISC_SC_PWC <- st_filter(PWC_demo, v30_DISC_SC_Buffers500m)

v30_B4_NE_PWC <- st_filter(PWC_demo, v30_B4_NE_Buffers500m)
v30_B4_NW_PWC <- st_filter(PWC_demo, v30_B4_NW_Buffers500m)
v30_B4_YO_PWC <- st_filter(PWC_demo, v30_B4_YO_Buffers500m)
v30_B4_EM_PWC <- st_filter(PWC_demo, v30_B4_EM_Buffers500m)
v30_B4_WM_PWC <- st_filter(PWC_demo, v30_B4_WM_Buffers500m)
v30_B4_E_PWC <- st_filter(PWC_demo, v30_B4_E_Buffers500m)
v30_B4_LO_PWC <- st_filter(PWC_demo, v30_B4_LO_Buffers500m)
v30_B4_SE_PWC <- st_filter(PWC_demo, v30_B4_SE_Buffers500m)
v30_B4_SW_PWC <- st_filter(PWC_demo, v30_B4_SW_Buffers500m)
v30_B4_WA_PWC <- st_filter(PWC_demo, v30_B4_WA_Buffers500m)
v30_B4_SC_PWC <- st_filter(PWC_demo, v30_B4_SC_Buffers500m)

#v23
v23_DISC_NE_PWC <- st_filter(PWC_demo, v23_DISC_NE_Buffers500m)
v23_DISC_NW_PWC <- st_filter(PWC_demo, v23_DISC_NW_Buffers500m)
v23_DISC_YO_PWC <- st_filter(PWC_demo, v23_DISC_YO_Buffers500m)
v23_DISC_EM_PWC <- st_filter(PWC_demo, v23_DISC_EM_Buffers500m)
v23_DISC_WM_PWC <- st_filter(PWC_demo, v23_DISC_WM_Buffers500m)
v23_DISC_E_PWC <- st_filter(PWC_demo, v23_DISC_E_Buffers500m)
v23_DISC_LO_PWC <- st_filter(PWC_demo, v23_DISC_LO_Buffers500m)
v23_DISC_SE_PWC <- st_filter(PWC_demo, v23_DISC_SE_Buffers500m)
v23_DISC_SW_PWC <- st_filter(PWC_demo, v23_DISC_SW_Buffers500m)
v23_DISC_WA_PWC <- st_filter(PWC_demo, v23_DISC_WA_Buffers500m)
v23_DISC_SC_PWC <- st_filter(PWC_demo, v23_DISC_SC_Buffers500m)

v23_B4_NE_PWC <- st_filter(PWC_demo, v23_B4_NE_Buffers500m)
v23_B4_NW_PWC <- st_filter(PWC_demo, v23_B4_NW_Buffers500m)
v23_B4_YO_PWC <- st_filter(PWC_demo, v23_B4_YO_Buffers500m)
v23_B4_EM_PWC <- st_filter(PWC_demo, v23_B4_EM_Buffers500m)
v23_B4_WM_PWC <- st_filter(PWC_demo, v23_B4_WM_Buffers500m)
v23_B4_E_PWC <- st_filter(PWC_demo, v23_B4_E_Buffers500m)
v23_B4_LO_PWC <- st_filter(PWC_demo, v23_B4_LO_Buffers500m)
v23_B4_SE_PWC <- st_filter(PWC_demo, v23_B4_SE_Buffers500m)
v23_B4_SW_PWC <- st_filter(PWC_demo, v23_B4_SW_Buffers500m)
v23_B4_WA_PWC <- st_filter(PWC_demo, v23_B4_WA_Buffers500m)
v23_B4_SC_PWC <- st_filter(PWC_demo, v23_B4_SC_Buffers500m)

#v3
v3_DISC_NE_PWC <- st_filter(PWC_demo, v3_DISC_NE_Buffers500m)
v3_DISC_NW_PWC <- st_filter(PWC_demo, v3_DISC_NW_Buffers500m)
v3_DISC_YO_PWC <- st_filter(PWC_demo, v3_DISC_YO_Buffers500m)
v3_DISC_EM_PWC <- st_filter(PWC_demo, v3_DISC_EM_Buffers500m)
v3_DISC_WM_PWC <- st_filter(PWC_demo, v3_DISC_WM_Buffers500m)
v3_DISC_E_PWC <- st_filter(PWC_demo, v3_DISC_E_Buffers500m)
v3_DISC_LO_PWC <- st_filter(PWC_demo, v3_DISC_LO_Buffers500m)
v3_DISC_SE_PWC <- st_filter(PWC_demo, v3_DISC_SE_Buffers500m)
v3_DISC_SW_PWC <- st_filter(PWC_demo, v3_DISC_SW_Buffers500m)
v3_DISC_WA_PWC <- st_filter(PWC_demo, v3_DISC_WA_Buffers500m)
v3_DISC_SC_PWC <- st_filter(PWC_demo, v3_DISC_SC_Buffers500m)

v3_B4_NE_PWC <- st_filter(PWC_demo, v3_B4_NE_Buffers500m)
v3_B4_NW_PWC <- st_filter(PWC_demo, v3_B4_NW_Buffers500m)
v3_B4_YO_PWC <- st_filter(PWC_demo, v3_B4_YO_Buffers500m)
v3_B4_EM_PWC <- st_filter(PWC_demo, v3_B4_EM_Buffers500m)
v3_B4_WM_PWC <- st_filter(PWC_demo, v3_B4_WM_Buffers500m)
v3_B4_E_PWC <- st_filter(PWC_demo, v3_B4_E_Buffers500m)
v3_B4_LO_PWC <- st_filter(PWC_demo, v3_B4_LO_Buffers500m)
v3_B4_SE_PWC <- st_filter(PWC_demo, v3_B4_SE_Buffers500m)
v3_B4_SW_PWC <- st_filter(PWC_demo, v3_B4_SW_Buffers500m)
v3_B4_WA_PWC <- st_filter(PWC_demo, v3_B4_WA_Buffers500m)
v3_B4_SC_PWC <- st_filter(PWC_demo, v3_B4_SC_Buffers500m)

# converting sf object to data frames for comparison

#v30
v30_DISC_NE_PWC <- sf_to_df(v30_DISC_NE_PWC, fill = TRUE)
v30_DISC_NW_PWC <- sf_to_df(v30_DISC_NW_PWC, fill = TRUE)
v30_DISC_YO_PWC <- sf_to_df(v30_DISC_YO_PWC, fill = TRUE)
v30_DISC_EM_PWC <- sf_to_df(v30_DISC_EM_PWC, fill = TRUE)
v30_DISC_WM_PWC <- sf_to_df(v30_DISC_WM_PWC, fill = TRUE)
v30_DISC_E_PWC <- sf_to_df(v30_DISC_E_PWC, fill = TRUE)
v30_DISC_LO_PWC <- sf_to_df(v30_DISC_LO_PWC, fill = TRUE)
v30_DISC_SE_PWC <- sf_to_df(v30_DISC_SE_PWC, fill = TRUE)
v30_DISC_SW_PWC <- sf_to_df(v30_DISC_SW_PWC, fill = TRUE)
v30_DISC_WA_PWC <- sf_to_df(v30_DISC_WA_PWC, fill = TRUE)
v30_DISC_SC_PWC <- sf_to_df(v30_DISC_SC_PWC, fill = TRUE)

v30_B4_NE_PWC <- sf_to_df(v30_B4_NE_PWC, fill = TRUE)
v30_B4_NW_PWC <- sf_to_df(v30_B4_NW_PWC, fill = TRUE)
v30_B4_YO_PWC <- sf_to_df(v30_B4_YO_PWC, fill = TRUE)
v30_B4_EM_PWC <- sf_to_df(v30_B4_EM_PWC, fill = TRUE)
v30_B4_WM_PWC <- sf_to_df(v30_B4_WM_PWC, fill = TRUE)
v30_B4_E_PWC <- sf_to_df(v30_B4_E_PWC, fill = TRUE)
v30_B4_LO_PWC <- sf_to_df(v30_B4_LO_PWC, fill = TRUE)
v30_B4_SE_PWC <- sf_to_df(v30_B4_SE_PWC, fill = TRUE)
v30_B4_SW_PWC <- sf_to_df(v30_B4_SW_PWC, fill = TRUE)
v30_B4_WA_PWC <- sf_to_df(v30_B4_WA_PWC, fill = TRUE)
v30_B4_SC_PWC <- sf_to_df(v30_B4_SC_PWC, fill = TRUE)

#v23
v23_DISC_NE_PWC <- sf_to_df(v23_DISC_NE_PWC, fill = TRUE)
v23_DISC_NW_PWC <- sf_to_df(v23_DISC_NW_PWC, fill = TRUE)
v23_DISC_YO_PWC <- sf_to_df(v23_DISC_YO_PWC, fill = TRUE)
v23_DISC_EM_PWC <- sf_to_df(v23_DISC_EM_PWC, fill = TRUE)
v23_DISC_WM_PWC <- sf_to_df(v23_DISC_WM_PWC, fill = TRUE)
v23_DISC_E_PWC <- sf_to_df(v23_DISC_E_PWC, fill = TRUE)
v23_DISC_LO_PWC <- sf_to_df(v23_DISC_LO_PWC, fill = TRUE)
v23_DISC_SE_PWC <- sf_to_df(v23_DISC_SE_PWC, fill = TRUE)
v23_DISC_SW_PWC <- sf_to_df(v23_DISC_SW_PWC, fill = TRUE)
v23_DISC_WA_PWC <- sf_to_df(v23_DISC_WA_PWC, fill = TRUE)
v23_DISC_SC_PWC <- sf_to_df(v23_DISC_SC_PWC, fill = TRUE)

v23_B4_NE_PWC <- sf_to_df(v23_B4_NE_PWC, fill = TRUE)
v23_B4_NW_PWC <- sf_to_df(v23_B4_NW_PWC, fill = TRUE)
v23_B4_YO_PWC <- sf_to_df(v23_B4_YO_PWC, fill = TRUE)
v23_B4_EM_PWC <- sf_to_df(v23_B4_EM_PWC, fill = TRUE)
v23_B4_WM_PWC <- sf_to_df(v23_B4_WM_PWC, fill = TRUE)
v23_B4_E_PWC <- sf_to_df(v23_B4_E_PWC, fill = TRUE)
v23_B4_LO_PWC <- sf_to_df(v23_B4_LO_PWC, fill = TRUE)
v23_B4_SE_PWC <- sf_to_df(v23_B4_SE_PWC, fill = TRUE)
v23_B4_SW_PWC <- sf_to_df(v23_B4_SW_PWC, fill = TRUE)
v23_B4_WA_PWC <- sf_to_df(v23_B4_WA_PWC, fill = TRUE)
v23_B4_SC_PWC <- sf_to_df(v23_B4_SC_PWC, fill = TRUE)

#v3
v3_DISC_NE_PWC <- sf_to_df(v3_DISC_NE_PWC, fill = TRUE)
v3_DISC_NW_PWC <- sf_to_df(v3_DISC_NW_PWC, fill = TRUE)
v3_DISC_YO_PWC <- sf_to_df(v3_DISC_YO_PWC, fill = TRUE)
v3_DISC_EM_PWC <- sf_to_df(v3_DISC_EM_PWC, fill = TRUE)
v3_DISC_WM_PWC <- sf_to_df(v3_DISC_WM_PWC, fill = TRUE)
v3_DISC_E_PWC <- sf_to_df(v3_DISC_E_PWC, fill = TRUE)
v3_DISC_LO_PWC <- sf_to_df(v3_DISC_LO_PWC, fill = TRUE)
v3_DISC_SE_PWC <- sf_to_df(v3_DISC_SE_PWC, fill = TRUE)
v3_DISC_SW_PWC <- sf_to_df(v3_DISC_SW_PWC, fill = TRUE)
v3_DISC_WA_PWC <- sf_to_df(v3_DISC_WA_PWC, fill = TRUE)
v3_DISC_SC_PWC <- sf_to_df(v3_DISC_SC_PWC, fill = TRUE)

v3_B4_NE_PWC <- sf_to_df(v3_B4_NE_PWC, fill = TRUE)
v3_B4_NW_PWC <- sf_to_df(v3_B4_NW_PWC, fill = TRUE)
v3_B4_YO_PWC <- sf_to_df(v3_B4_YO_PWC, fill = TRUE)
v3_B4_EM_PWC <- sf_to_df(v3_B4_EM_PWC, fill = TRUE)
v3_B4_WM_PWC <- sf_to_df(v3_B4_WM_PWC, fill = TRUE)
v3_B4_E_PWC <- sf_to_df(v3_B4_E_PWC, fill = TRUE)
v3_B4_LO_PWC <- sf_to_df(v3_B4_LO_PWC, fill = TRUE)
v3_B4_SE_PWC <- sf_to_df(v3_B4_SE_PWC, fill = TRUE)
v3_B4_SW_PWC <- sf_to_df(v3_B4_SW_PWC, fill = TRUE)
v3_B4_WA_PWC <- sf_to_df(v3_B4_WA_PWC, fill = TRUE)
v3_B4_SC_PWC <- sf_to_df(v3_B4_SC_PWC, fill = TRUE)

# remove first column as it doesn't contain numeric data and messes up the statistics

v30_DISC_NE_PWC <- v30_DISC_NE_PWC[ -c(1)]
v30_DISC_NW_PWC <- v30_DISC_NW_PWC[ -c(1)]
v30_DISC_YO_PWC <- v30_DISC_YO_PWC[ -c(1)]
v30_DISC_EM_PWC <- v30_DISC_EM_PWC[ -c(1)]
v30_DISC_WM_PWC <- v30_DISC_WM_PWC[ -c(1)]
v30_DISC_E_PWC <- v30_DISC_E_PWC[ -c(1)]
v30_DISC_LO_PWC <- v30_DISC_LO_PWC[ -c(1)]
v30_DISC_SE_PWC <- v30_DISC_SE_PWC[ -c(1)]
v30_DISC_SW_PWC <- v30_DISC_SW_PWC[ -c(1)]
v30_DISC_WA_PWC <- v30_DISC_WA_PWC[ -c(1)]
v30_DISC_SC_PWC <- v30_DISC_SC_PWC[ -c(1)]
v23_DISC_NE_PWC <- v23_DISC_NE_PWC[ -c(1)]
v23_DISC_NW_PWC <- v23_DISC_NW_PWC[ -c(1)]
v23_DISC_YO_PWC <- v23_DISC_YO_PWC[ -c(1)]
v23_DISC_EM_PWC <- v23_DISC_EM_PWC[ -c(1)]
v23_DISC_WM_PWC <- v23_DISC_WM_PWC[ -c(1)]
v23_DISC_E_PWC <- v23_DISC_E_PWC[ -c(1)]
v23_DISC_LO_PWC <- v23_DISC_LO_PWC[ -c(1)]
v23_DISC_SE_PWC <- v23_DISC_SE_PWC[ -c(1)]
v23_DISC_SW_PWC <- v23_DISC_SW_PWC[ -c(1)]
v23_DISC_WA_PWC <- v23_DISC_WA_PWC[ -c(1)]
v23_DISC_SC_PWC <- v23_DISC_SC_PWC[ -c(1)]
v3_DISC_NE_PWC <- v3_DISC_NE_PWC[ -c(1)]
v3_DISC_NW_PWC <- v3_DISC_NW_PWC[ -c(1)]
v3_DISC_YO_PWC <- v3_DISC_YO_PWC[ -c(1)]
v3_DISC_EM_PWC <- v3_DISC_EM_PWC[ -c(1)]
v3_DISC_WM_PWC <- v3_DISC_WM_PWC[ -c(1)]
v3_DISC_E_PWC <- v3_DISC_E_PWC[ -c(1)]
v3_DISC_LO_PWC <- v3_DISC_LO_PWC[ -c(1)]
v3_DISC_SE_PWC <- v3_DISC_SE_PWC[ -c(1)]
v3_DISC_SW_PWC <- v3_DISC_SW_PWC[ -c(1)]
v3_DISC_WA_PWC <- v3_DISC_WA_PWC[ -c(1)]
v3_DISC_SC_PWC <- v3_DISC_SC_PWC[ -c(1)]

v30_B4_NE_PWC <- v30_B4_NE_PWC[ -c(1)]
v30_B4_NW_PWC <- v30_B4_NW_PWC[ -c(1)]
v30_B4_YO_PWC <- v30_B4_YO_PWC[ -c(1)]
v30_B4_EM_PWC <- v30_B4_EM_PWC[ -c(1)]
v30_B4_WM_PWC <- v30_B4_WM_PWC[ -c(1)]
v30_B4_E_PWC <- v30_B4_E_PWC[ -c(1)]
v30_B4_LO_PWC <- v30_B4_LO_PWC[ -c(1)]
v30_B4_SE_PWC <- v30_B4_SE_PWC[ -c(1)]
v30_B4_SW_PWC <- v30_B4_SW_PWC[ -c(1)]
v30_B4_WA_PWC <- v30_B4_WA_PWC[ -c(1)]
v30_B4_SC_PWC <- v30_B4_SC_PWC[ -c(1)]
v23_B4_NE_PWC <- v23_B4_NE_PWC[ -c(1)]
v23_B4_NW_PWC <- v23_B4_NW_PWC[ -c(1)]
v23_B4_YO_PWC <- v23_B4_YO_PWC[ -c(1)]
v23_B4_EM_PWC <- v23_B4_EM_PWC[ -c(1)]
v23_B4_WM_PWC <- v23_B4_WM_PWC[ -c(1)]
v23_B4_E_PWC <- v23_B4_E_PWC[ -c(1)]
v23_B4_LO_PWC <- v23_B4_LO_PWC[ -c(1)]
v23_B4_SE_PWC <- v23_B4_SE_PWC[ -c(1)]
v23_B4_SW_PWC <- v23_B4_SW_PWC[ -c(1)]
v23_B4_WA_PWC <- v23_B4_WA_PWC[ -c(1)]
v23_B4_SC_PWC <- v23_B4_SC_PWC[ -c(1)]
v3_B4_NE_PWC <- v3_B4_NE_PWC[ -c(1)]
v3_B4_NW_PWC <- v3_B4_NW_PWC[ -c(1)]
v3_B4_YO_PWC <- v3_B4_YO_PWC[ -c(1)]
v3_B4_EM_PWC <- v3_B4_EM_PWC[ -c(1)]
v3_B4_WM_PWC <- v3_B4_WM_PWC[ -c(1)]
v3_B4_E_PWC <- v3_B4_E_PWC[ -c(1)]
v3_B4_LO_PWC <- v3_B4_LO_PWC[ -c(1)]
v3_B4_SE_PWC <- v3_B4_SE_PWC[ -c(1)]
v3_B4_SW_PWC <- v3_B4_SW_PWC[ -c(1)]
v3_B4_WA_PWC <- v3_B4_WA_PWC[ -c(1)]
v3_B4_SC_PWC <- v3_B4_SC_PWC[ -c(1)]

# combine versions into one sheet for analysis

#North East
# Discounter basic statistical data comparison
vall_DISC_NE_sum <- compare_them(v30_DISC_NE_PWC,v23_DISC_NE_PWC,v3_DISC_NE_PWC)
# Big 4 basic statistical data comparison
vall_B4_NE_sum <- compare_them(v30_B4_NE_PWC,v23_B4_NE_PWC,v3_B4_NE_PWC)
#North West
# Discounter basic statistical data comparison
vall_DISC_NW_sum <- compare_them(v30_DISC_NW_PWC,v23_DISC_NW_PWC,v3_DISC_NW_PWC)
# Big 4 basic statistical data comparison
vall_B4_NW_sum <- compare_them(v30_B4_NW_PWC,v23_B4_NW_PWC,v3_B4_NW_PWC)
#Yorkshire
# Discounter basic statistical data comparison
vall_DISC_YO_sum <- compare_them(v30_DISC_YO_PWC,v23_DISC_YO_PWC,v3_DISC_YO_PWC)
# Big 4 basic statistical data comparison
vall_B4_YO_sum <- compare_them(v30_B4_YO_PWC,v23_B4_YO_PWC,v3_B4_YO_PWC)
#East Midlands
# Discounter basic statistical data comparison
vall_DISC_EM_sum <- compare_them(v30_DISC_EM_PWC,v23_DISC_EM_PWC,v3_DISC_EM_PWC)
# Big 4 basic statistical data comparison
vall_B4_EM_sum <- compare_them(v30_B4_EM_PWC,v23_B4_EM_PWC,v3_B4_EM_PWC)
#West Midlands
# Discounter basic statistical data comparison
vall_DISC_WM_sum <- compare_them(v30_DISC_WM_PWC,v23_DISC_WM_PWC,v3_DISC_WM_PWC)
# Big 4 basic statistical data comparison
vall_B4_WM_sum <- compare_them(v30_B4_WM_PWC,v23_B4_WM_PWC,v3_B4_WM_PWC)
#East
# Discounter basic statistical data comparison
vall_DISC_E_sum <- compare_them(v30_DISC_E_PWC,v23_DISC_E_PWC,v3_DISC_E_PWC)
# Big 4 basic statistical data comparison
vall_B4_E_sum <- compare_them(v30_B4_E_PWC,v23_B4_E_PWC,v3_B4_E_PWC)
#London
# Discounter basic statistical data comparison
vall_DISC_LO_sum <- compare_them(v30_DISC_LO_PWC,v23_DISC_LO_PWC,v3_DISC_LO_PWC)
# Big 4 basic statistical data comparison
vall_B4_LO_sum <- compare_them(v30_B4_LO_PWC,v23_B4_LO_PWC,v3_B4_LO_PWC)
#South East
# Discounter basic statistical data comparison
vall_DISC_SE_sum <- compare_them(v30_DISC_SE_PWC,v23_DISC_SE_PWC,v3_DISC_SE_PWC)
# Big 4 basic statistical data comparison
vall_B4_SE_sum <- compare_them(v30_B4_SE_PWC,v23_B4_SE_PWC,v3_B4_SE_PWC)
#South West
# Discounter basic statistical data comparison
vall_DISC_SW_sum <- compare_them(v30_DISC_SW_PWC,v23_DISC_SW_PWC,v3_DISC_SW_PWC)
# Big 4 basic statistical data comparison
vall_B4_SW_sum <- compare_them(v30_B4_SW_PWC,v23_B4_SW_PWC,v3_B4_SW_PWC)
#Wales
# Discounter basic statistical data comparison
vall_DISC_WA_sum <- compare_them(v30_DISC_WA_PWC,v23_DISC_WA_PWC,v3_DISC_WA_PWC)
# Big 4 basic statistical data comparison
vall_B4_WA_sum <- compare_them(v30_B4_WA_PWC,v23_B4_WA_PWC,v3_B4_WA_PWC)
#Scotland
# Discounter basic statistical data comparison
vall_DISC_SC_sum <- compare_them(v30_DISC_SC_PWC,v23_DISC_SC_PWC,v3_DISC_SC_PWC)
# Big 4 basic statistical data comparison
vall_B4_SC_sum <- compare_them(v30_B4_SC_PWC,v23_B4_SC_PWC,v3_B4_SC_PWC)

# save all to csv
write.csv(vall_DISC_NE_sum, "vall_DISC_NE_sum.csv")
write.csv(vall_DISC_NW_sum, "vall_DISC_NW_sum.csv")
write.csv(vall_DISC_YO_sum, "vall_DISC_YO_sum.csv")
write.csv(vall_DISC_EM_sum, "vall_DISC_EM_sum.csv")
write.csv(vall_DISC_WM_sum, "vall_DISC_WM_sum.csv")
write.csv(vall_DISC_E_sum, "vall_DISC_E_sum.csv")
write.csv(vall_DISC_LO_sum, "vall_DISC_LO_sum.csv")
write.csv(vall_DISC_SE_sum, "vall_DISC_SE_sum.csv")
write.csv(vall_DISC_SW_sum, "vall_DISC_SW_sum.csv")
write.csv(vall_DISC_WA_sum, "vall_DISC_WA_sum.csv")
write.csv(vall_DISC_SC_sum, "vall_DISC_SC_sum.csv")
write.csv(vall_B4_NE_sum, "vall_B4_NE_sum.csv")
write.csv(vall_B4_NW_sum, "vall_B4_NW_sum.csv")
write.csv(vall_B4_YO_sum, "vall_B4_YO_sum.csv")
write.csv(vall_B4_EM_sum, "vall_B4_EM_sum.csv")
write.csv(vall_B4_WM_sum, "vall_B4_WM_sum.csv")
write.csv(vall_B4_E_sum, "vall_B4_E_sum.csv")
write.csv(vall_B4_LO_sum, "vall_B4_LO_sum.csv")
write.csv(vall_B4_SE_sum, "vall_B4_SE_sum.csv")
write.csv(vall_B4_SW_sum, "vall_B4_SW_sum.csv")
write.csv(vall_B4_WA_sum, "vall_B4_WA_sum.csv")
write.csv(vall_B4_SC_sum, "vall_B4_SC_sum.csv")
write.csv(vall_DISC_sum, "vall_DISC_sum.csv")
write.csv(vall_B4_sum, "vall_B4_sum.csv")
