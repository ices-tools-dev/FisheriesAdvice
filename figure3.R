# 
# 
# Figure 3. ICES official landings by species groups within the Celtic Seas ecoregion, 2006-2013.
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
data <- read.csv(unz(tmpFile, 
                     "ICESCatchDataset2006-2013.csv"),
                 stringsAsFactors = F)

# tt <- read.table("http://vocab.ices.dk/?CodeTypeRelID=357&CodeID=137494")

# Get lookuptables for guilds & species:

guilds <-
  read.delim(
    "~/MEGA/ices/r/ices/keys-lists/guild.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )

speciesNames <-
  read.csv(
    "~/MEGA/ices/r/ices/keys-lists/species.list", header = TRUE, stringsAsFactors = FALSE
  )

#
########
# AREA #
########
#
# Select ICES areas 7abcefghijk, 6a, 6b2
ecoArea <- c("27_7_a",
             "27_7_b", 
             "27_7_c", 
             "27_7_bc_NK",
             "27_7_e", 
             "27_7_f", 
             "27_7_g", 
             "27_7_j", 
             "27_7_k", 
             "27_7_g-k_NK", 
             "27_7_NK", 
             "27_4",
             "27_6_a",
             "27_6_b_2")


#MSFD subregions: "Celtic Seas" 

dataArea <- data[data$Area %in% ecoArea,]

# Append scientific names, then guild names (this may need to be optimised -- it works, but it's slow)

sciname.apply <- c(speciesNames$scientific_name)
names(sciname.apply) <- c(speciesNames$species)
dataArea$sci.name <- sciname.apply[dataArea$Species]

guild.apply <- c(guilds$feeding.guild)
names(guild.apply) <- c(guilds$scientific.name)
dataArea$feeding.guild <- guild.apply[dataArea$sci.name]

#
# Aggregate by species and country
dataArea <- melt(dataArea, 
                 id.vars = c("Species", "Area", "Units", "Country","sci.name", "feeding.guild"),
                 variable.name = "YEAR",
                 value.name = "VALUE")
dataArea$YEAR <- as.numeric(gsub("X", "", dataArea$YEAR))
dataGuild <- ddply(dataArea, .(feeding.guild, YEAR), 
                     summarize,
                     "CATCH" = sum(VALUE))
#
dataGuild <- dcast(dataGuild, YEAR ~ feeding.guild, value.var = "CATCH")
row.names(dataGuild) <- unique(dataGuild$YEAR)
dataGuild <- dataGuild[,-1]
# orderCountry <- names(sort(apply(dataCountry, 2, mean)))
orderGuild <- names(sort(colSums(dataGuild)))
dataGuild <- dataGuild[, rev(orderGuild)]
dataOthers <- data.frame(dataGuild[,9:1],
                         "OTHER" = rowSums(dataGuild[,10:ncol(dataGuild)]))





#
# Stacked line graph
#
# colList <- brewer.pal(n = 9, name = 'Set1')
colList <- c(brewer.pal(n = ncol(dataOthers) - 1, name = 'Spectral'), "grey40")

# stackplot = function(data, ylim = NA, main = NA, colors = NULL, xlab = NA, ylab = NA, ...) {
#   if (is.na(ylim)) {
ylim = c(0, max(rowSums(dataOthers, na.rm = T)))
#   }
#   if (is.null(colors)) {
#     colors = c("green","red","lightgray","blue","orange","purple", "yellow")


png(filename = "~/MEGA/ices/r/ices/testfig3.png",
    width = 172.4,
    height = 172.4,
    units = "mm",
    res = 600)
#
par(xpd = T,
    mar=c(2.15, 2.25, 0.45, 8.25),
    #     yaxs = "i",
    #     xaxs = "i",
    oma = c(0, 0, 1.25, 0),
    #     usr = c(0, 1, 0, 1),
    mgp = c(1.25, 0.35, 0),
    tck = -0.01,
    family = "Calibri")

xval = as.numeric(row.names(dataOthers))
summary = rep(0, nrow(dataOthers))
recent = summary

# Create empty plot
plot(c(-100), 
     c(-100),
     xlim = range(xval, na.rm = T),
     ylim = ylim,
     ylab = "Official landings",
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

legend(x = 2014, y = max(ylim), 
       rev(cols),
       col = rev(colList),
       cex = 0.8,
       lwd = 1, 
       lty = 1)
dev.off()
# par(mar=c(5, 4, 4, 2) + 0.1)
# }
# 
# 
# stackplot(dataOthers, colors = colList)