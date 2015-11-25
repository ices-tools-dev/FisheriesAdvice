rm(list = ls())
###############
# Script Info #
###############
# PURPOSE:Figure 1. ICES official landings by nation of species within the Celtic Seas ecoregion, 2006-2013.
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
needList <- c("plyr", "reshape2", "RColorBrewer", "extrafont")
new.packages <- needList[!(needList %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#
library(plyr)
library(reshape2)
library(RColorBrewer)
library(extrafont)
#

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
#
#MSFD subregions: "Celtic Seas" 
# 
dataArea <- catchDat[catchDat$Area %in% toupper(ecoArea),]
#
# Aggregate by species and country
dataArea <- melt(dataArea, 
                 id.vars = c("Species", "Area", "Units", "Country"),
                 variable.name = "YEAR",
                 value.name = "VALUE")
dataArea$YEAR <- as.numeric(gsub("X", "", dataArea$YEAR))
dataCountry <- ddply(dataArea, .(Country, YEAR), 
                     summarize,
                     "CATCH" = sum(VALUE))
#
dataCountry <- dcast(dataCountry, YEAR ~ Country, value.var = "CATCH")
row.names(dataCountry) <- unique(dataCountry$YEAR)
dataCountry <- dataCountry[,-1]
# orderCountry <- names(sort(apply(dataCountry, 2, sd)))
orderCountry <- names(sort(colSums(dataCountry)))
dataCountry <- dataCountry[, rev(orderCountry)]
dataOthers <- data.frame(dataCountry[,9:1],
                         "OTHER" = rowSums(dataCountry[,10:ncol(dataCountry)]))
#
# Stacked line graph
colList <- c(brewer.pal(n = ncol(dataOthers) - 1, name = 'Spectral'), "grey40")

ylim = c(0, max(rowSums(dataOthers, na.rm = T)))
# 
png(filename = "~/Fig1.png",
    width = 172.4,
    height = 172.4,
    units = "mm",
    res = 600)
    #
par(xpd = T,
    mar=c(2.15, 2.25, 0.45, 3.5),
    oma = c(0, 0, 0, 0),
    mgp = c(1.25, 0.35, 0),
    tck = -0.01,
    family = "Calibri")
# 
xval = as.numeric(row.names(dataOthers))
summary = rep(0, nrow(dataOthers))
recent = summary
#    
# Create empty plot
plot(c(-100), 
     c(-100),
     xlim = range(xval, na.rm = T),
     ylim = ylim,
     ylab = "Official catch",
     xlab = "Year")
  
# One polygon per column
cols = names(dataOthers)

for(c in 1:length(cols)) {
    current = dataOthers[[cols[[c]]]]
    summary = summary + current
    polygon(
      x = c(xval, rev(xval)),
      y = c(summary, rev(recent)),
      col = colList[[c]],
      border = "grey90"
    )
    recent = summary
  }
# 
legend(x = 2013.25, y = max(ylim), 
       rev(cols),
       col = rev(colList),
       cex = 0.8,
       pch = 15,
       xpd = TRUE,
       bty = "n")
#
dev.off()

