# Figure 6. Stock trends of F/Fmsy and total landings volumes removed in relation to Fmsy status, 
# separated by demersal and pelagic stocks within the Celtic Seas ecoregion, 1990-2013. 
# Currently only contains the stocks available in the ICES standard graphs database of 2014 advice.
# 
# * Notes: ACOM sub-group does not like the ”spaghetti plots” at the top as they 
# are difficult to make sense of. Is there another visual that could be used? 
# Alternatively, Secretariat staff thought that and Favg could be added as a line to the landings bar graphs.
# * Current format: spaghetti plots and stacked graph.
# * Data source: ? and DLS landings data are available from Anne
# * Data needed: ?
# * Responsible person: Scott
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

plotDir = "output/"
# This may take a minute to get the correct font
fontTable <- fonttable()
colList <- brewer.pal(n = 9, name = 'Set1')
ltyList <- c(1,3:6)
#
if(!"Calibri" %in% fontTable$FamilyName) font_import(pattern="[C/c]alibri", prompt = FALSE)
#
# Load data on ecosystem, books, guilds, etc.
load("allDataPlotOverview_v001.rdat")
stockInfo <- allDat[,c("ICES.Book", "Species", "Area", 
                       "Stock.code", "Type")]
stockInfo <- stockInfo[!duplicated(stockInfo),]
#
# Load most recent data from Stock Assessment Graphs database
source("getTestStockSummary.R")
stockTable <- getTestSummaryTable(year = 2015)
# 
stockInfo$speciesID <- tolower(gsub( "-.*$", "", as.character(stockInfo$Stock.code)))
# 
stockTable <- merge(stockTable, 
                    stockInfo[,c("speciesID", "Type")],
                    by = c("speciesID"), all.x = T, all.y = F)
# 
stockTable$Type[stockTable$STOCKID %in% c("hom-west", "mac-nea", "whb-comb")] <- "Pelagic"
stockTable$Type[stockTable$STOCKID %in% c("usk-icel", "bss-47")] <- "Demersal"
#
unique(stockTable$EcoRegion)
stockTable <- stockTable[stockTable$EcoRegion == "Celtic Sea and West of Scotland" &
                         stockTable$Type %in% c("Demersal", "Pelagic"),]

ddt <- stockTable[,colnames(stockTable) %in% c("Year", "Type", "F", "FMSY", "landings")]
#
# Aggregate according to Fmsy
FlowFMSY <- ddply(ddt[ddt$F < ddt$FMSY,], .(Year, Type), summarize,
                  METRIC = "FlowFMSY",
                  VALUE = sum(landings, na.rm = T))
FupFMSY <- ddply(ddt[ddt$F > ddt$FMSY,], .(Year, Type), summarize,
                 METRIC = "FupFMSY",
                 VALUE = sum(landings, na.rm = T))
noFMSY <- ddply(ddt[is.na(ddt$FMSY),], .(Year, Type), summarize,
                METRIC = "noFMSY",
               VALUE = sum(landings, na.rm = T))
dat <- rbind(FlowFMSY, FupFMSY, noFMSY)
#
dd <- dcast(dat, Type + Year ~ METRIC, value.var = "VALUE")
# 
# reduce values to thousand tonnes
dd[,c(3,4,5)] <- dd[,c(3,4,5)] / 1000
png(filename = paste0("~/CS_Figure6.png"),
                      width = 172.4 * 2,
                      height = 172.4,
                      units = "mm",
                      res = 600)
par(mfrow = c(1, 2),
    xpd = T,
    mar=c(2.15, 2.25, 2.45, 3.75),
    oma = c(0, 0, 0, 0),
    mgp = c(1.25, 0.35, 0),
    tck = -0.01,
    family = "Calibri")
# 
for(i in c("Demersal", "Pelagic")){
  #   
  ddDat <- dd[dd$Type == i &
             dd$Year %in% c(1990:2014),]
  #
  sequential <- brewer.pal(3, "RdYlGn")
  ddDat[is.na(ddDat)] <- 0
  # 
  barplot(height = t(ddDat[,c(5,4,3)]),
          names.arg = ddDat$Year,
          beside = F,
          cex.names = 0.7,
          col = sequential,
          xlab = "Year",
          ylab = "Landings (thousand tonnes)",
          width = 1)
  #
  legend(x = nrow(ddDat) + 4.5, 
         y = max(ddDat[,c(5,4,3)]),
         xpd = TRUE,
         bty = "n",
         legend = c(expression("F < F"[MSY]),
                    expression("F > F"[MSY]),
                    expression(paste(F[MSY], " unk."))),
         fill = sequential[3:1])
  mtext(i, 3)
} # Close i loop
dev.off()