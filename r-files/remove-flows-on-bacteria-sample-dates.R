# get obs flow from Yaquina River gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/get-obs-flow-data.R")

# estimate flow for Big Elk Creek from Yaquina River Gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/estimate-flow.R")

## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## file name of big elk flow data with the flows for dates of bacteria samples removed
chr.file.flow.est.removed <- "obs-flow-removed.RData"

## bacteria data path
chr.dir.bac.obs <- paste0(chr.dir.prime, "/ObsData")

## obs bacteria data file
chr.file.obs.bac <- "obs.RData"

## load obs bacteria data
load(file = paste0(chr.dir.bac.obs, "/", chr.file.obs.bac))

## get all dates
chr.dates.bac.all <-  strftime(obs.data$date, format = "%Y-%m-%d")

## remove duplicated dates
chr.dates.bac.unique <- unique(chr.dates.bac.all)
chr.dates.bac.unique <- chr.dates.bac.unique[order(chr.dates.bac.unique)]

## find rows in flow data.frame for dates of bacteria samples
lng.bac.flow.rows <- grep(pattern = paste0(chr.dates.bac.unique, collapse = "|"),
                          strftime(df.flow.est$date, format = "%Y-%m-%d"))

## remove flows for dates of bacteria samples
df.flow.est.reduced <- df.flow.est[-1 * lng.bac.flow.rows, ]

## save a copy of the big elk flow data with the flows for dates of bacteria samples removed
save(df.flow.est.reduced, file = paste0(chr.dir.bac.obs, "/", chr.file.flow.est.removed))

