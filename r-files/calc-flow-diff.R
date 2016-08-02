## load packages

## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## file name of big elk flow data with the flows for dates of bacteria samples removed
chr.file.flow.est.removed <- "obs-flow-removed.RData"

## bacteria data path
chr.dir.bac.obs <- paste0(chr.dir.prime, "/ObsData")

## load obs flow data
load(file = paste0(chr.dir.bac.obs, "/", chr.file.flow.est.removed))

## only using df.flow.est for flow
rm(df.flow.est.reduced, lng.bac.flow.rows)

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

## clean up
rm(chr.dir.bac.obs, obs.data, chr.dates.bac.all)

## get simulation period
## using the uci file for the earlier calibration that used the updated 
## simulation period
chr.uci <- scan(paste0(chr.dir.prime, "/pest-hspf-files/bigelk.uci"), sep = "\n", 
                what = "character", quiet = TRUE)
dt.sim.period <- as.POSIXct(
  sapply(
    strsplit(
      gsub(" {1,}", ",", 
           gsub("(^ {1,})|( {1,}$)", "", 
                gsub("[^0-9/ ]|(00\\:00)|(24\\:00)", "",
                     chr.uci[grep("START", chr.uci)]))),
      split = ","), cbind))

## clean up
rm(chr.uci)

## get flow within simulation period
df.tmp <- df.flow.est[df.flow.est$date >= dt.sim.period[1] &
                      df.flow.est$date <= dt.sim.period[2], ]
names(df.tmp) <- c("date", "flow")
df.flow.est <- df.tmp

## clean up
rm(list = grep("^df.flow.est$", ls(pattern = "df\\.*"), invert = TRUE, 
               value = TRUE))

## get rows in df.flow.est of the dates of the samples
lng.bac.flow.rows <- grep(paste0(chr.dates.bac.unique, collapse = "|"),
                       df.flow.est$date)

## calculate differences, this is a bacwarddifference, so I add a NA at
## the beginning of the 
df.flow.dif <- data.frame(date = df.flow.est$date,
                          dif = c(NA,diff(df.flow.est$flow, lag = 1, 
                                          differences = 1)))

## removing the flow diffs related to days bacteria samples taken
## Two-diffs removed for each sample day, the one on the day of the sample and
## the one after the day of the sample. The diff function calculates the 
## backward difference
lng.bac.flow.dif.rows <- c(lng.bac.flow.rows, lng.bac.flow.rows + 1)
## re-ordering the rows
lng.bac.flow.dif.rows <- lng.bac.flow.dif.rows[order(lng.bac.flow.dif.rows)]
## removing duplicates that occured when samples collected on sucessive days
lng.bac.flow.dif.rows <- unique(
  lng.bac.flow.dif.rows[order(lng.bac.flow.dif.rows)])

## get diffs for days that samples not collected
## this data.frame is the source for the differnces in the flows ti use in the
## PEST control file and to get from the model output
df.flow.dif.rm <- df.flow.dif[-1 * lng.bac.flow.dif.rows, ]
