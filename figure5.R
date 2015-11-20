#-
#-
#- Fisheries advice example for the Celtic Seas
#-
#- Figure 5. EU landings by gear within the Celtic Seas ecoregion as available from STECF data. Note: Spanish data only available for 2012-13.
#-
#-
#------------> Filepaths <----------

dataPath    <- "~/r/advice/data/"
outPath     <- "~/r/advice/output/"
codePath    <- "~/r/advice/code/"

pathList <- as.list(c(dataPath, outPath, codePath))

for (p in pathList) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }
  
}

#------------||

 
# LOAD NEEDED LIBS+PACKAGAGES
library(scales)
library(plyr)
library(reshape2)
library(foreach)
library(RColorBrewer)
library(extrafont)
# library(data.table)

#--- simple data load -- should be made fancier when time permits (i.e. after deadline of advice example 2015)
fao27 <-
    read.delim(
      "~/r/advice/data/stecf-economic-fleet.data", header = TRUE, fill = TRUE, stringsAsFactors = FALSE, dec = "."
    )
#   
fishtech.list <-
    read.csv(
      "~/r/keys-lists/stecf-fishingtech.list", header = TRUE, stringsAsFactors = FALSE
    )

gear.list <-
  read.csv(
    "~/r/keys-lists/stecf-gear.list", header = TRUE, stringsAsFactors = FALSE
  )

#   
# reg.list <-
#     read.delim(
#       "~/r/ices/keys-lists/ices.regions.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
#     )
#   
# guild.list <-
#     read.delim(
#       "~/r/ices/keys-lists/guild.list", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8"
#     )
#   
# species.list <-
#     read.csv(
#       "~/r/ices/keys-lists/species.list", header = TRUE, stringsAsFactors = FALSE
#     )
#   
# 
#
# # append subregion key:
#   
#   reg.apply <- c(reg.list$eco.reg.shrt)
#   names(reg.apply) <- c(reg.list$ices.code)
#   ices27.df$ecoregion <- reg.apply[ices27.df$sub_reg]
#   
#   sciname.apply <- c(species.list$scientific_name)
#   names(sciname.apply) <- c(species.list$species)
#   ices27.df$sci.name <- sciname.apply[ices27.df$species_code]
#   
#   guild.apply <- c(guild.list$feeding.guild)
#   names(guild.apply) <- c(guild.list$scientific.name)
#   ices27.df$feeding.guild <- guild.apply[ices27.df$sci.name]

#- Append indicator of active/passive gears:
#   activegear.apply <- c(fishtech.list$towed_static)
#   names(activegear.apply) <- c(fishtech.list$fishing_tech)
#   fao27$towed_static <- activegear.apply[fao27$fishing_tech]

  geartype.apply <- c(gear.list$type)
  names(geartype.apply) <- c(gear.list$code)
  fao27$gear_group <- geartype.apply[fao27$gear_type]
  
  
ecoArea <- c("27.7.a",
             "27.7.b", 
             "27.7.c", 
             "27.7.e", 
             "27.7.f", 
             "27.7.g", 
             "27.7.j", 
             "27.7.k", 
             "27.6.a",
             "27.6.b")

#- select sub areas in CEL-region:
dataArea <- fao27[fao27$sub_reg %in% ecoArea,]

dataLandings <- subset.data.frame(
  dataArea, subset = c(variable_name == "Landings weight"), select = c("country_name", "year","variable_name","value", "gear_group")
)
dataLandings$value <-  as.numeric(dataLandings$value)
#--- End of subsetting, file check

#--get effort, kWdays

# Aggregate by gear, year and country

p.dataGear <- ddply(dataLandings, .(year, gear_group), 
                     summarize,
                     "landed_weight" = sum(value))

#- convert to wide format, and sort individually small => large per column.
dataGear <- dcast(p.dataGear, year ~ gear_group, value.var = "landed_weight", fun.aggregate = sum)
row.names(dataGear) <- unique(dataGear$year)
dataGear <- dataGear[,-1]
# orderCountry <- names(sort(apply(dataGear, 2, sd)))
orderCountry <- names(sort(colSums(dataGear)))
dataGear <- dataGear[, (orderCountry)]

# this is somehow not working -- whyyyy
# if (ncol(dataGear > 10) {
#   dataOthers <-
#     data.frame(dataGear[,9:1], "Other" = rowSums(dataGear[,10:ncol(dataGear)]))
#   } else {
#   (dataOthers <- dataGear)
#   }
#

dataOthers <- data.frame(dataGear[,9:1],
                         "OTHER" = rowSums(dataGear[,10:ncol(dataGear)]))

# Stacked line graph
colList <- c(brewer.pal(n = ncol(dataOthers) - 1, name = 'Spectral'), "grey40")

ylim = c(0, max(rowSums(dataOthers, na.rm = T)))
# 
png(filename = "~/fig5rev4.png",
    width = 172.4,
    height = 172.4,
    units = "mm",
    res = 600)
#
par(xpd = T,
    mar=c(2.15, 2.25, 0.45, 5.5),
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
     ylab = "Landed Weight",
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
legend(x = 2014, y = max(ylim), 
       rev(cols),
       col = rev(colList),
       cex = 0.8,
       pch = 15,
       xpd = TRUE,
       bty = "n")
#
dev.off()


