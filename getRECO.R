# rm(list = ls())
###############
# Script Info #
###############
# PURPOSE: Load ICES Reference Code (RECO) vocabulary for stock code area information.
# AUTHOR: Scott Large 2015
# REVIEWED BY:
# VERSION: 0.1
#
# example:
# tt <- getRECO(ICES_Book = "E")
#
getRECO <- function(ICES_Book) {
  if(!toupper(ICES_Book) %in% LETTERS[c(1:8,10)]) {
    stop("Please refer to the ICES_Book by the letter")
  } 
  #############
  # ICES_Book #
  #############
  #A: Baltic Sea
  #B: Barents Sea and Norwegian Sea
  #C: Bay of Biscay and Iberian Sea
  #D: Celtic Sea
  #E: Celtic Sea and West of Scotland
  #F: Faroe Plateau Ecosystem
  #G: Iceland and East Greenland
  #H: North Sea
  #K: Widely distributed and migratory stocks

  ######################
  # CHANGES/ ADDITIONS #
  ######################
  # Need to add: 
  # 
  # Done:
  #
  ############
  # PACKAGES #
  ############
  # Required packages
  needList <- c("XML")
  new.packages <- needList[!(needList %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  #
  library(XML)
  #
  ##################
  # Parse XML data #
  ##################
  #
  bookInfo <- NULL
  #
  # First identify the stocks in the Celtic Sea (Book E)
  nameURL <- "http://vocab.ices.dk/services/rdf/collection/ICES_Book/E"
  nameXML <- xmlRoot(xmlTreeParse(nameURL, isURL = T, options = HUGE, useInternalNodes =  T))
  #
  nameRelated <- attr(names(nameXML[["concept"]]) == "related", "names")
  nameRelatedSeq <- which(nameRelated == "related")
  # 
  stockCode <- NULL
  for(i in nameRelatedSeq) {
    tr <- as.character(xmlAttrs(nameXML[["concept"]][[i]]))
    tr <- gsub(".*ICES_StockCode/", "", tr)
    stockCode <- c(stockCode, tr)
  } # close i loop
  #
  bookInfo <- NULL
  # Next, create a list holding the stock area, stock code, name, etc.
  for(j in 1:length(stockCode)) {
    stockInfo <- NULL
  #   stockCode <- NULL
    deprecated <- NULL
    stockName <- NULL
    stockArea <- NULL
    ICES_Book <- NULL
    WoRMS <- NULL
    Year <- NULL
    #
    areaURL <- data.frame("stockCode" = stockCode[j], 
                          "URL" = paste0("http://vocab.ices.dk/services/rdf/collection/ICES_StockCode/", stockCode[j]),
                          row.names = NULL)
    areaXML.j <- xmlRoot(xmlTreeParse(areaURL$URL, isURL = T, options = HUGE, useInternalNodes =  T))
    areaRelated <- attr(names(areaXML.j[["concept"]]) == "related", "names")
    areaRelatedSeq <- which(areaRelated == "related")
    # 
    stockName <- xmlValue(areaXML.j[["concept"]][["prefLabel"]])
    deprecated <- xmlValue(areaXML.j[["concept"]][["deprecated"]])
    # 
    for(k in areaRelatedSeq) {
      tr <- as.character(xmlAttrs(areaXML.j[["concept"]][[k]]))
      trSplit <- unlist(strsplit(gsub(".*collection/", "", tr), "/"))
      #
      if(trSplit[1] == "ICES_Area") {
        stockArea <- c(stockArea, trSplit[2])   
      }
      if(trSplit[1] == "ICES_Book") {
        ICES_Book <- c(ICES_Book, trSplit[2])
      }
      if(trSplit[1] == "SpecWoRMS") {
        WoRMS <- c(WoRMS, trSplit[2])
      }
      if(trSplit[1] == "Year") {
        Year <- c(Year, trSplit[2])
      }
    } # close k loop
    #
    stockInfo <- list(stockCode = as.character(areaURL$stockCode),
                      deprecated = deprecated,
                      stockName = stockName,
                      stockArea = stockArea,
                      ICES_Book = ICES_Book,
                      WoRMS = WoRMS,
                      Year = as.numeric(as.character(Year)))
    # 
    name <- gsub("-", "_", stockInfo$stockCode)
    bookInfo[[name]] <- stockInfo
    cat(paste0("Now serving ", as.character(areaURL$stockCode), "\n"))
  # 
  } # close j loop
  return(bookInfo)
} # Closes getRECO function
