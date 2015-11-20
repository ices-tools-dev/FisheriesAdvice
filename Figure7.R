# Figure 7. Kobe-plots of Celtic Seas stocks by category (demersal, pelagic, elasmobranch, Nephrops). 
# (Left) Relative fishing pressure (F/Fmsy) and biomass (SSB/Btrig) for stocks, which have SSB and F 
# related against reference points (msy where available, otherwise pa or qualitative. Stocks in the green 
# region are exploited below Fmsy and have an SSB that is above Btrigger. (Right) Stocks of unknown status 
# in relation to reference points. The size of each bubble corresponds to the landings in 2013. 
# (CURRENTLY DOES NOT INCLUDE ALL CELTIC SEAS STOCKS)#
# * Notes: 
#+ ACOM sub-group does not like the use of stock codes. 
#+ How can we get around this? 
#+ The yellow shading is a problem because it should be red. 
#+ Can the circles be colored rather than the background? 
#+ Could this resolve some of the challenges with these plots?
#* Current format: kobe-plots
#* Data source: ?
#* Data needed: ?
#* Responsible person: Scott?
#* Challenges:
#* Format/presentation notes/suggestions: 
#Status: TBD
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
colList <- brewer.pal(n = 9, name = 'Set1')
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

ddt <- stockTable[,colnames(stockTable) %in% c("STOCKID", "Type", "F", "FMSY", "SSB", "MSYBtrigger", "landings")]
ddt$FMSY[ddt$FMSY == 0] <- NA
ddt$MSYBtrigger[ddt$MSYBtrigger == 0] <- NA
# 
ddt$F.FMSY[!is.na(ddt$FMSY)] <- ddt$F[!is.na(ddt$FMSY)] / ddt$FMSY[!is.na(ddt$FMSY)]
ddt$SSB.Btrig[!is.na(ddt$MSYBtrigger)] <- ddt$SSB[!is.na(ddt$MSYBtrigger)] / ddt$MSYBtrigger[!is.na(ddt$MSYBtrigger)]
# 
#determine size of bubbles
ddt$cex <- 10 * sqrt(as.numeric(ddt$landings)/ max(as.numeric(ddt$landings), na.rm=T))
ddt$cex[is.na(ddt$landings)] <- min(ddt$cex, na.rm = T)

ddt$colList <- NA
ddt$colList[ddt$F.FMSY >= 1 &
              ddt$SSB.Btrig >= 1] <- "gold"

ddt$colList[ddt$F.FMSY < 1 &
              ddt$SSB.Btrig >= 1] <- "lawngreen"

ddt$colList[ddt$F.FMSY > 1 &
              ddt$SSB.Btrig < 1] <- "orangered"

ddt$colList[ddt$F.FMSY < 1 &
              ddt$SSB.Btrig < 1] <- "gold"

textPoints <- (ddt$F.FMSY) + sqrt(cex)*0.1

png(filename = "~/Fig7.png",
      width = 172.4 * 2,
      height = 172.4,
      units = "mm",
      res = 600)

# set up main plot
par(mfrow = c(1,2),
    mar=c(2.15, 2.25, 0.45, 3.5),
    oma = c(0, 0, 0, 0),
    mgp = c(1.25, 0.35, 0),
    tck = -0.01,
    family = "Calibri")
# 
plot(NA,xlim=c(0,5),
     ylim=c(0,5),
     xlab = expression('F / F'[MSY]), 
     ylab = expression('SSB / B'[trigger]),
     xaxs='i',
     yaxs='i')
# add coloured regions
abline(h = 1, col = "grey60", lty = 2)
abline(v = 1, col = "grey60", lty = 2)
#plot bubbles
points(ddt$F.FMSY, ddt$SSB.Btrig, cex = ddt$cex, pch=21, bg=ddt$colList, col=ddt$colList)
text(textPoints, ddt$SSB.Btrig, ddt$STOCKID, col = "grey30")

ddt$colList[is.na(ddt$colList)] <- "grey50"
df <- ddt[order(ddt$landings),]

# set up main plot
par(mar=c(2.5, 4.25, 0.45, 3.5),
    oma = c(0, 0, 0, 0),
    mgp = c(1.25, 0.35, 0),
    tck = -0.01,
    family = "Calibri",
    new = T)

barplot(log10(df$landings), 
        axes = T, 
        col = df$colList,
        space = .5, 
        border = "grey80",
        horiz = TRUE, 
        xlab = "log(Landings) (tonnes)",
        names.arg = df$STOCKID, 
        las = 1)
box()
dev.off()

# By type
# i = "Demersal"
for(i in unique(ddt$Type)) {
png(filename = paste0("~/Fig7_", i, ".png"),
    width = 172.4 * 2,
    height = 172.4,
    units = "mm",
    res = 600)

# set up main plot
par(mfrow = c(1,2),
    mar=c(2.15, 2.25, 1.45, 3.5),
    oma = c(0, 0, 0, 0),
    mgp = c(1.25, 0.35, 0),
    tck = -0.01,
    family = "Calibri")
# 
plot(NA, 
     xlim=c(0,5),
     ylim=c(0,5),
     xlab = expression('F / F'[MSY]), 
     ylab = expression('SSB / B'[trigger]),
     xaxs='i',
     yaxs='i',
     main = i)
# add coloured regions
abline(h = 1, col = "grey60", lty = 2)
abline(v = 1, col = "grey60", lty = 2)
#plot bubbles
ddt.i <- ddt[ddt$Type == i,]
points(ddt.i$F.FMSY, ddt.i$SSB.Btrig, cex = ddt.i$cex, pch = 21, bg=ddt.i$colList, col=ddt.i$colList)
text(textPoints, ddt.i$SSB.Btrig, ddt.i$STOCKID, col = "grey30")

ddt.i$colList[is.na(ddt.i$colList)] <- "grey50"
df <- ddt.i[order(ddt.i$landings),]

# set up main plot
par(mar=c(2.5, 4.25, 1.45, 3.5),
    oma = c(0, 0, 0, 0),
    mgp = c(1.25, 0.35, 0),
    tck = -0.01,
    family = "Calibri",
    new = T)

barplot(log10(df$landings), 
        axes = T, 
        col = df$colList,
        space = .5, 
        border = "grey80",
        horiz = TRUE, 
        xlab = "log(Landings) (tonnes)",
        names.arg = df$STOCKID, 
        las = 1)
box()
dev.off()
} # close i loop