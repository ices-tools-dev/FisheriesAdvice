rm(list = ls())
################
library(rmarkdown)
library(readxl)
library(dplyr)
library(RColorBrewer)
library(knitr)
library(reshape2)
library(ggplot2)
library(XML)
#
plotDir = "~/git/ices-dk/FisheriesAdvice/output"
dataDir = "~/git/ices-dk/FisheriesAdvice/"
options(scipen = 5)
colList <- brewer.pal(n = 9, name = 'Set1')
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Download, unzip, and load data from the source     # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
# ~~~ ICES official catch statistics ~~~ #
catchURL <- "http://ices.dk/marine-data/Documents/CatchStats/OfficialNominalCatches.zip"
tmpFileCatch <- tempfile(fileext = ".zip")
download.file(catchURL, destfile = tmpFileCatch, mode = "wb", quiet = TRUE)
catchDat <- read.csv(unz(tmpFileCatch,
                         "ICESCatchDataset2006-2014.csv"),
                     stringsAsFactors = FALSE, header = TRUE, fill = TRUE)
# 
# ~~~ STECF effort and catch statistics ~~~ #
# (NOTE: Will be replaced with RDB data- SL)
effortURL <- "http://stecf.jrc.ec.europa.eu/documents/43805/870977/2014_STECF+14-20+-+Fishing+Effort+Regimes+data+tables.zip"
tmpFileEffort <- tempfile(fileext = ".zip")
download.file(effortURL, destfile = tmpFileEffort, mode = "wb", quiet = TRUE)
effortDat <- read_excel(unzip(tmpFileEffort,
                              files = "Electronic_appendices/Effort_trends_by_country.xlsx"),
                        sheet = "kWdays at sea")
stecfCatchDat <- read_excel(unzip(tmpFileEffort,
                                  files = "Electronic_appendices/Landings_Discards_Discard-rates.xlsx"),
                            sheet = "Land_Disc_Disc-rate_by_Country")
# 
# ~~~ FAO species names and labels ~~~ #
spURL <- "ftp://ftp.fao.org/FI/STAT/DATA/ASFIS_sp.zip"
tmpFileSp <- tempfile(fileext = ".zip")
download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
spList <- read.table(unz(tmpFileSp,
                         "ASFIS_sp_Feb_2016.txt"),
                     stringsAsFactors = FALSE, header = TRUE, fill = TRUE)
#
# ~~~ ICES Stock assessment summary data  ~~~ #
stockTable <- rICES::getSummaryTable(year = 2015)
stockTable$speciesID <- tolower(stockTable$speciesID)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# "Helper" files for merging fishery guilds, areas, etc     # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
# ~~~ ICES areas from STECF effort and ICES catch databases ~~~ #
# (NOTE: with RDB this should not be necessary - SL)
areaID <- read.csv("~/git/ices-dk/FisheriesAdvice/areaList.csv", 
                   stringsAsFactors = FALSE)
#
# ~~~ FAO codes and ICES stock codes ~~~ #
# (NOTE: current ICES stock codes do not necessarily match with FAO 3-character
# codes, in the future this might not be necessary - SL)
speciesID <- read.csv("~/rCode/Data/ICESspeciesID_v1.csv",
                      stringsAsFactors = FALSE)
# 
# ~~~ Fishery guilds by ICES stock code ~~~ #
# (NOTE: These guilds should become a part of the RECO database - SL)
fisheryGuild <- read.csv("~/git/ices-dk/rStockOverview/fisheryGuild.csv",
                         stringsAsFactors = FALSE)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Aggregate data for plotting # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
# ~~~ Clean ICES stock assessment summary data ~~~ # 
areaNames<- c(
  "Oceanic north-east Atlantic",
  "Faroes",
  "Iceland Sea",
  "Ecoregions Barents Sea and Norwegian Sea",
  "Faroes",
  "Celtic Seas",
  "Greater North Sea",
  "Bay of Biscay and the Iberian Coast",
  "The Baltic Sea")
# 
levels(stockTable$EcoRegion) <- c(levels(stockTable$EcoRegion), areaNames)
stockTable$EcoRegion[stockTable$EcoRegion == "Barents Sea and Norwegian Sea"] <- "Ecoregions Barents Sea and Norwegian Sea"
stockTable$EcoRegion[stockTable$EcoRegion == "Iceland and East Greenland"] <- "Iceland Sea"
stockTable$EcoRegion[stockTable$EcoRegion == "Faroe Plateau Ecosystem"] <- "Faroes"
stockTable$EcoRegion[stockTable$EcoRegion == "Celtic Sea and West of Scotland"] <- "Celtic Seas"
stockTable$EcoRegion[stockTable$EcoRegion == "North Sea"] <- "Greater North Sea"
stockTable$EcoRegion[stockTable$EcoRegion == "Bay of Biscay and Iberian Sea"] <- "Bay of Biscay and the Iberian Coast"
# stockTable$EcoRegion[stockTable$EcoRegion == "Baltic Sea"] <- "Baltic Sea"
# 
# ~~~ Clean up Fishery Guild information ~~~ #
fisheryGuild <- fisheryGuild %>%
  mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code))) %>%
  select(Stock.code, Fisheries.Guild, speciesID) %>%
  inner_join(speciesID, c("speciesID" = "oldCode"))
# 
# ~~~ Merge Fishery Guild information with ICES stock codes and FAO codes ~~~ #
guildList <- fisheryGuild %>%
  # mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code))) %>%
  # inner_join(speciesID, c("speciesID" = "oldCode")) %>%
  rename(STOCKID = Stock.code,
         GUILD = Fisheries.Guild) %>%
  mutate(STOCKID = tolower(STOCKID),
         STOCKID = gsub("[[:space:]]", "", STOCKID)) %>%
  inner_join(stockTable, c("STOCKID" = "STOCKID")) %>%
  rename(ECOREGION = EcoRegion,
         YEAR = Year) %>%
  filter(SpeciesName != "Psetta maxima (historic name)") %>%
  mutate(fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription),
         fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
         stockSizeDescription = ifelse(stockSizeDescription == "NA", "Stock Size: Relative", stockSizeDescription),
         FmsyDescription = "FMSY",
         stockSizeDescription = gsub("Stock Size: ", "", stockSizeDescription)) %>%
  filter(!stockSizeDescription %in% c("B/BMSY", "Relative") |
         !fishingPressureDescription %in% c("F/FMSY", "Relative"))
  
# guildMSY <- guildList %>%
#   group_by(STOCKID) %>%
#   mutate(F_FMSY = F / FMSY,
#          SSB_MSYBtrigger = SSB / MSYBtrigger) %>%
#   melt(id.vars = c("AssessmentYear",  "ECOREGION", "GUILD","STOCKID", "YEAR"),
#        measure.vars = c("F_FMSY", "SSB_MSYBtrigger", "F", "FMSY"),
#        variable.name = "METRIC",
#        value.name = "stockValue") %>%
#   filter(!is.na(stockValue)) %>% # remove NA VALUE
#   group_by(ECOREGION, GUILD, METRIC, YEAR) %>%
#   mutate(ecoGuildMean = mean(stockValue, na.rm = TRUE)) %>%
#   group_by(ECOREGION, METRIC, YEAR) %>%
#   mutate(ecoValue = mean(stockValue, na.rm = TRUE)) %>%
#   group_by(METRIC, GUILD, YEAR) %>%
#   mutate(allMean = mean(stockValue, na.rm = TRUE))
#

# guildMSY <- guildList %>%
#   filter(stockSizeDescription != "B/BMSY" |
#            fishingPressureDescription != "F/FMSY") %>% # Filters "relative" stocks
#   group_by(STOCKID) %>%
#   mutate(F_FMSY = F / FMSY,
#          SSB_MSYBtrigger = SSB / MSYBtrigger) %>% # Calculate F/FMSY and SSB/MSYBtrigger 
#   melt(id.vars = c("AssessmentYear",  "ECOREGION", "GUILD","STOCKID", "YEAR"),
#        measure.vars = c("F_FMSY", "SSB_MSYBtrigger"),
#        variable.name = "METRIC",
#        value.name = "stockValue") %>%
#   filter(!is.na(stockValue)) %>% # remove NA VALUE
#   group_by(ECOREGION, GUILD, METRIC, YEAR) %>%
#   mutate(ecoGuildMean = mean(stockValue)) %>% # Mean F and SSB by ecoregion, guild, and year
#   group_by(ECOREGION, METRIC, YEAR) %>%
#   mutate(ecoValue = mean(stockValue)) %>% # Mean F and SSB by ecoregion and year
#   group_by(METRIC, GUILD, YEAR) %>%
#   mutate(allMean = mean(stockValue)) # Mean F and SSB by guild, and year
# 

# guildList <- fisheryGuild %>%
#   mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code))) %>%
#   inner_join(speciesID, c("speciesID" = "oldCode")) %>%
#   rename(STOCKID = Stock.code,
#          GUILD = Fisheries.Guild) %>%
#   mutate(STOCKID = tolower(STOCKID),
#          STOCKID = gsub("[[:space:]]", "", STOCKID))
# 
# guildMSY <- guildList %>%
#   inner_join(stockTable, c("STOCKID" = "STOCKID")) %>%
#   rename(ECOREGION = EcoRegion,
#          YEAR = Year) %>%
#   filter(SpeciesName != "Psetta maxima (historic name)") %>% 
#   mutate(fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription), 
#          fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
#          stockSizeDescription = ifelse(stockSizeDescription == "NA", "Stock Size: Relative", stockSizeDescription),
#          FmsyDescription = "FMSY",
#          stockSizeDescription = gsub("Stock Size: ", "", stockSizeDescription)) %>%
#   filter(stockSizeDescription != "B/BMSY" | 
#            fishingPressureDescription != "F/FMSY") %>% # Filters "relative" stocks
#   group_by(STOCKID) %>%
#   mutate(F_FMSY = F / FMSY,
#          SSB_MSYBtrigger = SSB / MSYBtrigger) %>% # Calculate F/FMSY and SSB/MSYBtrigger 
#   melt(id.vars = c("AssessmentYear",  "ECOREGION", "GUILD","STOCKID", "YEAR"),
#        measure.vars = c("F_FMSY", "SSB_MSYBtrigger"),
#        variable.name = "METRIC",
#        value.name = "stockValue") %>%
#   filter(!is.na(stockValue)) %>% # remove NA values
#   group_by(ECOREGION, GUILD, METRIC, YEAR) %>%
#   mutate(ecoGuildMean = mean(stockValue)) %>% # Mean F and SSB by ecoregion, guild, and year
#   group_by(ECOREGION, METRIC, YEAR) %>%
#   mutate(ecoValue = mean(stockValue)) %>% # Mean F and SSB by ecoregion and year
#   group_by(METRIC, GUILD, YEAR) %>%
#   mutate(allMean = mean(stockValue)) # Mean F and SSB by guild, and year
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Other functions used for plotting #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#
# Allows for rotating text on the right margin
mtexti <- function(text, side, off = 0.25,
                   srt = if(side == 2) 90  else
                     if(side == 4) 270 else 0, ...) {
  # dimensions of plotting region in user units
  usr <- par('usr')
  # dimensions of plotting region in inches
  pin <- par('pin')
  # user units per inch
  upi <- c(usr[2]-usr[1],
           usr[4]-usr[3]) / pin
  # default x and y positions
  xpos <- (usr[1] + usr[2])/2
  ypos <- (usr[3] + usr[4])/2
  if(1 == side)
    ypos <- usr[3] - upi[2] * off
  if(2 == side)
    xpos <- usr[1] - upi[1] * off
  if(3 == side)
    ypos <- usr[4] + upi[2] * off
  if(4 == side)
    xpos <- usr[2] + upi[1] * off
  text(x=xpos, y=ypos, text, xpd=NA, srt=srt, ...)
}
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Prepare subsets for R Markdown rendering #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

ecoregionID <- "Celtic Seas"
plotList <- c("Baltic Sea", "Greater North Sea", "Celtic Seas")

renderFisheryOverview(ecoregionID = "Celtic Seas")

lapply(plotList, renderFisheryOverview)

renderFisheryOverview <- function(ecoregionID) {
# in a single for loop
#  1. define subgroup
#  2. render output
  # 
  ecoPath <- gsub(" ", "_", ecoregionID)
  ifelse(!dir.exists(file.path(plotDir, ecoPath)), dir.create(file.path(plotDir, ecoPath)), FALSE)
  # 
  icesID <- areaID$value[areaID$Ecoregion == ecoregionID &
                           areaID$areaType == "ICESarea"]
  stecfID <- areaID$value[areaID$Ecoregion == ecoregionID &
                            areaID$areaType == "STECFarea"]
  # 
  catchDatECO <- catchDat %>%
    Filter(f = function(x)!all(is.na(x))) %>%
    filter(Area %in% icesID) %>%
    melt(id.vars = c("Species", "Area", "Units", "Country"),
         variable.name = "YEAR",
         value.name = "VALUE") %>%
    inner_join(spList, c("Species" = "X3A_CODE")) %>%
    full_join(fisheryGuild, c("Species" = "newCode")) %>%
    mutate(YEAR = as.numeric(gsub("X", "", YEAR)))
  #
  effortDatECO <-
    effortDat %>%
    Filter(f = function(x)!all(is.na(x))) %>%
    filter(reg_area_cod %in% stecfID) %>%
    melt(id.vars = c("annex", "reg_area_cod", "reg_gear_cod", "Specon_calc", "country", "vessel_length"), 
         variable.name = "YEAR",
         value.name = "VALUE") %>%
    mutate(YEAR = as.numeric(levels(YEAR))[YEAR])
  # 
  stecfCatchDatECO <-
    stecfCatchDat %>%
    filter(reg_area %in% stecfID) %>%
    melt(id.vars = c("annex", "reg_area", "country", "reg_gear", "specon", "species"), 
         variable.name = "YEAR",
         value.name = "VALUE") %>%
    mutate(METRIC = as.character(gsub(".*\\s", "", YEAR)),
           YEAR = as.numeric(gsub("\\s.+$", "", YEAR))) %>%
    filter(METRIC == "L")
  # 
    guildListECO <- guildList %>%
    filter(ECOREGION == ecoregionID)

    #
  rmarkdown::render(paste0(dataDir, "fisheriesAdvice_template.rmd"),
                    output_dir = file.path(plotDir, ecoPath),
                    output_file = paste0('FisheriesAdvice_', ecoregionID, '.html'),
                    params = list(set_title = as.character(ecoregionID)))    
} 


