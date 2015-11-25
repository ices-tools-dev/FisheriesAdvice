# Figure 8. Proportion of Celtic Seas stocks by category: (Left) fished at or below Fmsy (green), 
# above Fmsy (red) and of unknown status in relation to fishing mortality reference points. 
# (Right) with biomass above B trigger (green), below B trigger (red) and of unknown status in 
# relation to biomass reference points. (CURRENTLY DOES NOT INCLUDE ALL CELTIC SEAS STOCKS)
# 
# * Notes: try to include the summary numbers/tables with this
# * Current format: pie charts
# * Data source: ?
# * Data needed: ?
# * Responsible person: Scott?
# * Challenges: 
#   * Workload: 
#   * Format/presentation notes/suggestions: 
#   * Status: TBD
# 
rm(list = ls())
################
library(plyr)
library(reshape2)
library(RColorBrewer)
library(extrafont)
library(XML)
# 
plotDir = "output/"
# This may take a minute to get the correct font
fontTable <- fonttable()
colList <- c("#4DAF4A","#E41A1C", "#377EB8")
#
if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#
# Load data on ecosystem, books, guilds, etc.
load("allDataPlotOverview_v001.rdat")
#
# Load most recent data from Stock Assessment Graphs database
source("getTestStockSummary.R")
stockTable <- getTestSummaryTable(year = 2015)
# 
stockTable <- merge(stockTable, 
                    stockInfo[,c("speciesID", "Type")],
                    by = c("speciesID"), all.x = F, all.y = F)
stockTable <- stockTable[!duplicated(stockTable),]
# 
stockTable <- stockTable[stockTable$EcoRegion == "Celtic Sea and West of Scotland" &
                         stockTable$Year == 2014,]
# 
ddt <- stockTable[,colnames(stockTable) %in% c("STOCKID", "Type", "F", "FMSY", "SSB", "MSYBtrigger")]

# Summarize F #
FlowFMSY <- ddply(ddt[!is.na(ddt$FMSY) & 
                  ddt$F <= ddt$FMSY,], .(Type), summarize,
                  METRIC = "FlowFMSY",
                  LAB = "F < FMSY",
                  COL = colList[1],
                  VALUE = length(F))

FupFMSY <- ddply(ddt[!is.na(ddt$FMSY) & ddt$F > ddt$FMSY,], .(Type), summarize,
                      METRIC = "FupFMSY",
                      LAB = "F > FMSY",
                      COL = colList[2],
                      VALUE = length(F))

noFMSY <- ddply(ddt[is.na(ddt$FMSY),], .(Type), summarize,
                METRIC = "noFMSY",
                LAB = "Unknown FMSY",
                COL = colList[3],
                VALUE = length(F))
datF <- rbind(FlowFMSY, FupFMSY, noFMSY)

# Summarize SSB #
SSBlowBtrig <- ddply(ddt[!is.na(ddt$MSYBtrigger) & 
                        ddt$SSB <= ddt$MSYBtrigger,], .(Type), summarize,
                  METRIC = "SSBlowBtrig",
                  LAB = "SSB < MSYBtrigger",
                  COL = colList[2],
                  VALUE = length(SSB))

SSBupBtrig <- ddply(ddt[!is.na(ddt$MSYBtrigger) &
                       ddt$SSB > ddt$MSYBtrigger,], .(Type), summarize,
                 METRIC = "SSBupBtrig",
                 LAB = "SSB > MSYBtrigger",
                 COL = colList[1],
                 VALUE = length(SSB))

noBtrig <- ddply(ddt[is.na(ddt$MSYBtrigger),], .(Type), summarize,
                METRIC = "noMSYBtrigger",
                LAB = "Unknown MSYBtrigger",
                COL = colList[3],
                VALUE = length(SSB))
datB <- rbind(SSBlowBtrig, SSBupBtrig, noBtrig)

dat <- rbind(datF, datB)
#plots
for(i in unique(dat$Type)) {
  png(filename = paste0("~/Fig8_", i, ".png"),
      width = 172.4,
      height = 172.4/2,
      units = "mm",
      res = 600)
  #
  par(mfrow = c(1, 2),
      mar=c(3.75, 3.75, 3.75, 3.75),
      oma = c(0, 0, 0, 0),
      cex = .7,
      family = "Calibri")

  dat.b <- dat[dat$Type == i &
               dat$METRIC %in% c("SSBlowBtrig", "SSBupBtrig", "noBtrig"),]
  dat.f <- dat[dat$Type == i &
               dat$METRIC %in% c("FlowFMSY", "FupFMSY", "noFMSY"),]
  
  pie(dat.f$VALUE, main = paste0(i, "\n F : FMSY"), col = dat.f$COL,
      labels = dat.f$LAB)
  
  pie(dat.b$VALUE, main = paste0(" \n SSB : Btrigger"), col = dat.b$COL,
      labels = dat.b$LAB)
  dev.off()
}