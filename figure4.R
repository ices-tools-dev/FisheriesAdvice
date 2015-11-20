
rm(list = ls())
###############
# Script Info #
###############
# PURPOSE:Figure 4. Top 10 3 species within each species group for the Celtic Seas ecoregion as reported in the ICES official landings, 2006-2013.
# AUTHOR: Scott Large 2015
# REVIEWED BY: Thomas Bech-thomassen 20-11-15
# VERSION: 0.2
# 
######################
# CHANGES/ ADDITIONS #
######################
# Need to add: Looping capabilities for producing graphs for each advice group (demersal/pelagic/shellfish/elasmobranchs)

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
#########
# PATHS #
#########
#

dataPath    <- "~/r/FisheriesAdvice/data"
outPath     <- "~/r/FisheriesAdvice/output"
# codePath    <- "~/r/FisheriesAdvice/code"

pathList <- as.list(c(dataPath, outPath))

for (p in pathList) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }
}

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

#- get liste of guilds and scientific speciesnames
guilds <-
  read.delim(
    "~/r/keys-lists/guild.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )

speciesNames <-
  read.csv(
    "~/r/keys-lists/species.list", header = TRUE, stringsAsFactors = FALSE
  )
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

#- Append scientific names, then guild names (this may need to be optimised -- it works, but it's slow, espc. for larger datasets)
sciname.apply <- c(speciesNames$scientific_name)
names(sciname.apply) <- c(speciesNames$species)
catchDat$sci.name <- sciname.apply[catchDat$Species]

guild.apply <- c(guilds$feeding.guild)
names(guild.apply) <- c(guilds$scientific.name)
catchDat$feeding.guild <- guild.apply[catchDat$sci.name]

advgroup.apply <- c(guilds$adv.group)
names(advgroup.apply) <- c(guilds$scientific.name)
catchDat$adv.group <- advgroup.apply[catchDat$sci.name]

#-------- find species missing group assignments --------

# get number of missing values pr. missing species
missing.guild.summary <-
  as.data.frame(unique(subset.data.frame(catchDat, c(is.na(adv.group)), select = c("sci.name", "adv.group"))))

write.csv(missing.guild.summary,file=file.path(outPath,"missingspecies.csv"))

#--------

dataArea.f <- catchDat[catchDat$Area %in% toupper(ecoArea),]

dataArea.s <- subset.data.frame(dataArea.f, subset = c(adv.group == "elasmobranchs"), select = c(colnames(dataArea.f)))

dataArea <- dataArea.s




#
# Aggregate by species and country
dataArea <- melt(dataArea, 
                 id.vars = c("Species", "Area", "Units", "Country","sci.name", "adv.group"),
                 variable.name = "YEAR",
                 value.name = "VALUE")
dataArea$YEAR <- as.numeric(gsub("X", "", dataArea$YEAR))
dataArea$VALUE <- as.numeric(dataArea$VALUE)
dataGroup <- ddply(dataArea, .(sci.name, adv.group, YEAR), 
                     summarize,
                     "CATCH" = sum(VALUE))
#
dataGroup <- na.omit(dcast(dataGroup, YEAR ~ sci.name, value.var = "CATCH"))
row.names(dataGroup) <- unique(dataGroup$YEAR)
dataGroup <- dataGroup[,-1]
# orderCountry <- names(sort(apply(dataGroup, 2, sd)))
orderGroup <- names(sort(colSums(dataGroup)))
dataGroup <- dataGroup[, rev(orderGroup)]
# dataOthers <- data.frame(dataGroup[,2:1],
#                          "OTHER" = rowSums(dataGroup[,3:ncol(dataGroup)]))

dataOthers <- data.frame(dataGroup[,2:1])
#
# Stacked line graph
colList <- c(brewer.pal(n = ncol(dataOthers) - 1, name = 'Spectral'), "grey40")

ylim = c(0, max(rowSums(dataOthers, na.rm = T)))
# 
png(filename = "~/fig4ELST10rev1.png",
    width = 172.4,
    height = 172.4,
    units = "mm",
    res = 600)
#
par(xpd = T,
    mar=c(2.15, 2.25, 0.45, 6.5),
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
     ylab = "Official catch, Elasmobranchs",
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
legend(x = 2013, y = max(ylim), 
       rev(cols),
       col = rev(colList),
       cex = 0.7,
       pch = 15,
       xpd = TRUE,
       bty = "n")
#
dev.off()



