rm(list = ls())
###############
# Script Info #
###############
# PURPOSE:ICES map of ecoregion and area overlap
# AUTHOR: Scott Large 2015
# REVIEWED BY:
# VERSION: 0.1
# 
######################
# CHANGES/ ADDITIONS #
######################
# Need to add:

# Done:
#
############
# PACKAGES #
############
# Required packages
needList <- c("plyr", "reshape2", "RColorBrewer",
              "extrafont", "rgdal", "doParallel",
              "foreach", "data.table", "sp",
              "maptools", "rgeos", "ggplot2")
new.packages <- needList[!(needList %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(needList, require, character.only = TRUE)
#
#############
# LOAD DATA #
#############
# Create a directory for the data
homeFolder <- "D:/Profiles/Scott/My Documents/rCode/data/"

# D:\Profiles\Scott\My Documents\rCode\data\mapData
localDir <- paste0(homeFolder, 'mapData')
# 
load(paste0(homeFolder,"rectAreaICES.Rdat"))
# 
# Unzipped shapefile without file type extensions
areaExt <- "ices_areas"
rectExt <- "ices_rectangles"
ecoExt <- "ices_ecoregions"

# ICES statistical area and rectangles data.frame
# load("ICESareaName_v001.Rdata")
# ICES DATRAS survey data
# load(paste0(homeFolder,"rectAreaICES.Rdat"))

# Read in the shapefiles
areaData <- readOGR(dsn = localDir, layer = areaExt)
rectData <- readOGR(dsn = localDir, layer = rectExt)
ecoData <- readOGR(dsn = localDir, layer = ecoExt)

# Establish the CRS, and make sure area areaData and recData are the same
areaCRS <- proj4string(areaData)
rectCRS <- proj4string(rectData)
ecoCRS <- proj4string(ecoData)
# if(areaCRS != rectCRS) stop("Projections are not the same for ICES Areas and rectangles")

# right now they should be the same so we will just use the areaCRS
areaData <- spTransform(areaData, CRS = CRS(areaCRS))
rectData <- spTransform(rectData, CRS = CRS(areaCRS))
ecoData <- spTransform(ecoData, CRS = CRS(areaCRS))
#
ecoRegions <- levels(ecoData@data$Ecoregion)
areaList <- levels(areaData@data$ICES_area)
#
# ecoRegions <- ecoData@data$Ecoregion[levels(ecoData@data$Ecoregion) %in% unique(areaName$ICES_ECO)]
#
# Select the areas of interest
areaList <- areaName$ICES_NAME[areaName$ICES_ECO == "Greater North Sea"]
getArea <- areaData[areaData$ICES_area %in% areaList,]
getEco <- ecoData[ecoData$Ecoregion %in% "Celtic Seas",]
# getEco <- ecoData
# 
# unique(areaList)
getArea@data$id <- rownames(getArea@data)
areaFort <- fortify(getArea, region = "ICES_area")
ecoFort <- fortify(getEco, region = "Ecoregion")
# 
cnames <- aggregate(cbind(long, lat) ~ id, data = areaFort, 
                    FUN=function(x)mean(range(x)))
# 
ggplot() +
  geom_polygon(data = ecoFort,
               aes(x = long, y = lat, group = group), fill = "pink", alpha = .2) +
  geom_polygon(data = areaFort, 
               aes(x=long, y=lat, group=group),
               fill = "white",
               col = "grey80") +
    geom_text(data = cnames, aes(long, lat, label = id), color = "gray50") +
    coord_equal() +
    theme_bw() +
    theme(legend.position= "bottom")

  
  

########
# DATA #
########
#
fontTable <- fonttable()
if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#
url <- "http://www.ices.dk/marine-data/Documents/CatchStats/OfficialLandings.zip"
tmpFile <- tempfile(fileext = ".zip")
download.file(url, destfile = tmpFile, mode = "wb", quiet = T)
catchDat <- read.csv(unz(tmpFile,
                         "ICESCatchDataset2006-2013.csv"),
                     stringsAsFactors = F)
# 
########
# AREA #
########
# Select ICES areas 7abcefghijk, 6a, 6b2
ecoArea <- c("27_7_a",
             "27_7_b", 
             "27_7_c_2", 
             "27_7_e", 
             "27_7_f", 
             "27_7_g", 
             "27_7_h",
             "27_7_j_2", 
             "27_7_k_2", 
             "27_6_a",
             "27_6_b_2")