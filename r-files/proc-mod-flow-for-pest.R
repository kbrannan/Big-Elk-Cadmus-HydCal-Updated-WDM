## load packages
library(DVstats, quietly = TRUE) # USGS-HySep R version in DVstats
library(doBy, quietly = TRUE) # need doBy package to sums for annual, summer and winter

## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## bacteria data path
chr.dir.bac.obs <- paste0(chr.dir.prime, "/ObsData")

## pest-hspf path
chr.dir.pest.hspf <- paste0(chr.dir.prime, "/pest-hspf-files")

## storm dates path
chr.dir.stm.dates <- "M:/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal"

## hspf output file
chr.file.hspf.out <- "modflow.out"

## obs bacteria data file
chr.file.obs.bac <- "obs.RData"

## load obs bacteria data
load(file = paste0(chr.dir.bac.obs, "/", chr.file.obs.bac))

## get all dates
chr.dates.bac.all <-  strftime(obs.data$date, format = "%Y-%m-%d")

## clean up
rm(obs.data)

## remove duplicated dates
chr.dates.bac.unique <- unique(chr.dates.bac.all)
chr.dates.bac.unique <- chr.dates.bac.unique[order(chr.dates.bac.unique)]

## read hspf output
df.out.raw <- read.pltgen(paste0(chr.dir.pest.hspf, "/", chr.file.hspf.out))

## convert af-ft/day to cfs
## 43559.9 cu-ft = 1 ac-ft, 1 day = 24 hr, 1 hr = 3600 sec
df.out.raw <- cbind(df.out.raw, 
                    flow.cfs = df.out.raw$rovol * 43559.9 / (24*3600))

## find rows in flow data.frame for dates of bacteria samples
lng.bac.flow.rows <- grep(pattern = paste0(chr.dates.bac.unique, collapse = "|"),
                          strftime(df.out.raw$date, format = "%Y-%m-%d"))

## drainage area in sqr mi for Big Elk Creek at outlet
da.be <- 88.8

## calculate model groups
## mbaseind, base flow index 
## baseflow seperation using USGS-HySep R version in DVstats
## need continuous time-series to use hysep, so need to break flow
## data into multiple continuous time-series

## setup data.frame to check how many days in the flow series to the previous 
## bacteria sample date (bck) and to the next (fwd) for each bacteria sample date
## and a variable to indicate that sample date has more than 1 day of flow series
## bewteen previous (bck) or next (fwd), chk == 1 is yes and chk == 0 in no
df.chk <- data.frame(fwd = rep(-1000, length(lng.bac.flow.rows)),
                      bck = -1000, chk = 0)
## first sample date only has forward
df.chk$fwd[1] <- lng.bac.flow.rows[1] - 1
## loop to do the remeaining sample dates except the last
for(ii in 2:(length(lng.bac.flow.rows) - 1)) {
  df.chk$fwd[ii] <- lng.bac.flow.rows[ii + 1] - lng.bac.flow.rows[ii]
  df.chk$bck[ii] <- lng.bac.flow.rows[ii] - lng.bac.flow.rows[ii - 1]
}
## clean up
rm(ii)
## last sample date only has a backward
df.chk$bck[length(lng.bac.flow.rows)] <- length(df.out.raw$date) -
  lng.bac.flow.rows[length(lng.bac.flow.rows)]

## identify sample dates that have more than one day of flow series
df.chk$chk[df.chk$fwd > 1 & df.chk$bck > 1] <- 1
## first only has fwd
if(df.chk$fwd[1] > 1) df.chk$chk[1] <- 1
## last only has bwd
if(df.chk$bck[length(lng.bac.flow.rows)] > 1) df.chk$chk[length(lng.bac.flow.rows)] <- 1

## setup data.frame for the bounds of the flow series segments used to estimate
## baseflow index
df.bnds <- cbind(start = -1,
                 end = -1, 
                 bac.rows = lng.bac.flow.rows,
                 df.chk,
                 len = -1,
                 bfi = -1)
## first start = 1
if(df.bnds$chk[1] == 1) {
  df.bnds$start[1] <- 1
  df.bnds$end[1] <- lng.bac.flow.rows[1] - 1
}
## do remaining sample dates that have chk == 1
for(ii in 2:(length(df.bnds$bac.rows) - 1)) {
  if(df.bnds$chk[ii] == 1) {
    df.bnds$start[ii] <- df.bnds$bac.rows[ii - 1] + 1
    df.bnds$end[ii] <- df.bnds$bac.rows[ii] - 1
  }
}
## last end is the length of the flow series
if(df.bnds$chk[length(df.bnds$chk)] == 1) {
  df.bnds$start[length(lng.bac.flow.rows)] <- lng.bac.flow.rows[length(lng.bac.flow.rows)] + 1
  df.bnds$end[length(lng.bac.flow.rows)] <- length(df.out.raw$date)
}
## get the length of the segements
df.bnds$len <- df.bnds$end - df.bnds$start

## calculate baseflow idex for each segment
tmp.wn <- options("warn")[[1]]
options(warn = -1)
for(ii in 1:length(df.bnds$bfi)) {
  if(df.bnds$chk[ii] == 1) {
    tmp.seq <- seq.int(from = df.bnds$start[ii], to = df.bnds$end[ii])
    ## use try function becuase can get error from hysep if there is only
    ## one minima (i think this means a constant slope)
    tmp.hysep88.8 <- try(hysep(Flow = df.out.raw$flow.cfs[tmp.seq], 
                               Dates = as.Date(df.out.raw$date[tmp.seq]), da = da.be,
                               select = "sliding"), silent = TRUE)
    if(class(tmp.hysep88.8)[1] == "baseflow") {
      ## only calculate baseflow index for error free hysep
      ## can use the sums of the baseflow and flow even though the units are cfs
      ## the conversion coefficients cancel out in the ratio
      df.bnds$bfi[ii] <- sum(tmp.hysep88.8$BaseQ) / sum(tmp.hysep88.8$Flow)
    }
    ## clean up
    rm(tmp.seq, tmp.hysep88.8)
  }
}
options(warn = tmp.wn)
rm(tmp.wn)
## get rows where baseflow calculated 
lng.bfi <- grep("[^-1]", df.bnds$bfi)

## calculate baseflow in the same way as calculated from the observed data
## as a wieght average. The weight is the length of the segment
mbaseind <- sum(df.bnds$len[lng.bfi] * df.bnds$bfi[lng.bfi]) / sum(df.bnds$len[lng.bfi])

## clean up
rm(ii, df.chk,lng.bfi, lng.bac.flow.rows)

## mvol_ann - annual volumes in ac-ft
## create factor for year
df.out.raw <- cbind(df.out.raw, 
                fac.ann  = as.factor(
                  strftime(df.out.raw$date, format = "%Y")))
mvol_ann <- as.numeric(
  summaryBy(rovol ~ fac.ann, data = df.out.raw, FUN = sum)[ ,2])

## create factor for month used in mvol_smr and mvol_wtr calculations
df.out.raw <- cbind(df.out.raw, 
                fac.mon  = as.factor(
                  strftime(df.out.raw$date, format = "%b")))

## create factor for month used in mvol_smr and mvol_wtr calculations
df.tmp <- data.frame(mon=as.character(df.out.raw$fac.mon), season = "none", 
                     stringsAsFactors = FALSE)

## summer season, summer is Jun, Jul and Aug
lng.smr <- grep("Jun|Jul|Aug", df.tmp$mon)

## winter season
lng.wtr <- grep("Dec|Jan|Feb", df.tmp$mon)

## assign summer and winter values to season. leave spring and fall as none
df.tmp$season[lng.smr] <- "summer"
df.tmp$season[lng.wtr] <- "winter"

## add season as factor to df.mod 
df.out.raw <- data.frame(df.out.raw, fac.season = as.factor(df.tmp$season))

## clean up
rm(df.tmp, lng.smr, lng.wtr)

df.vol.seasons <- summaryBy(rovol ~ fac.ann + fac.season , data = df.out.raw,
                            FUN = sum)


## mvol_smr - summer volumes in ac-ft
mvol_smr <- as.numeric(df.vol.seasons[as.character(df.vol.seasons$fac.season) == "summer", 
                           "rovol.sum"])

## mvol_wtr
mvol_wtr <- as.numeric(df.vol.seasons[as.character(df.vol.seasons$fac.season) == "winter", 
                           "rovol.sum"])
## storm information
## get storm dates from text file Information in this file from 
## Select_Storm_HydCal repo
## column 2 is the begin date of storm and column 8 is the end date of storm
df.strm.dates.raw <- read.delim(file = paste0(chr.dir.stm.dates, "/dates_stm.dat"),
                            header = FALSE, sep = " ", 
                            stringsAsFactors = FALSE)[ , c(2, 8)]
## convert to POSIXct dates
df.strm.dates <- data.frame(apply(df.strm.dates.raw, MARGIN = 2, strptime, 
                                  format = "%m/%d/%Y"))
names(df.strm.dates) <- c("begin", "end")

## get dates on bacteria samples
df.bac.dates <- data.frame(date = df.out.raw$date[df.bnds$bac.rows])

## check if bacteria samples are within storms
tmp.strm.dates <- cbind(df.strm.dates[, 1:2], keep = TRUE, bac.date = as.POSIXct("1967-07-02 00:00"))
## brute force not elegant
for(ii in 1:length(tmp.strm.dates$keep)) {
  for(jj in 1:length(df.bac.dates$date)) {
    if(as.numeric(df.bac.dates$date[jj]) >=  as.numeric(df.strm.dates$begin[ii]) & 
       as.numeric(df.bac.dates$date[jj]) <= as.numeric(df.strm.dates$end[ii])) {
      tmp.strm.dates$keep[ii] <- FALSE
      tmp.strm.dates$bac.date[ii] <- as.Date(df.bac.dates$date[jj])
      break
    }
    else tmp.strm.dates$bac.date[ii] <- NA
  }
}

df.strm.dates.reduced <- data.frame(begin = tmp.strm.dates$begin[grep("TRUE",tmp.strm.dates$keep)],
                                    end = tmp.strm.dates$end[grep("TRUE",tmp.strm.dates$keep)])

## clean up
rm(df.strm.dates.raw, ii, jj, tmp.strm.dates)

## storm durations in days
df.strm.dur <- floor(as.numeric(df.strm.dates.reduced$end - df.strm.dates.reduced$begin))

## mpeak
mpeak <- rep(-1, length(df.strm.dates.reduced$begin))

for(ii in 1:length(mpeak)) {
  mpeak[ii] <- max(
    df.out.raw$flow.cfs[df.out.raw$date >= df.strm.dates.reduced$begin[ii] & 
                        df.out.raw$date <= df.strm.dates.reduced$end[ii]])
}
rm(ii)

## mvol_stm in cu-ft for storm convert cu-ft/sec to cu-ft/day 
## using 1 day = 86400 s
mvol_stm <- rep(-1, length(df.strm.dates.reduced$begin))

for(ii in 1:length(mvol_stm)) {
  mvol_stm[ii] <- sum(
    df.out.raw$flow.cfs[df.out.raw$date >= df.strm.dates.reduced$begin[ii] &
                        df.out.raw$date <= df.strm.dates.reduced$end[ii]]) *
    (df.strm.dur[ii] * 86400)
}
## mtime - % exceedance for flow, using 0.01%, 1%, 5%, 25%, 50%, 75%, 95%, 99%
## this is different than what Cadmus using in tsproc which is the fraction
## of time the flow is above some value. I am not going to use tsproc when
## doinmg the calculations. I will use R script

## percents used
tmp.per <- c(0.0001, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)
mtime <- as.numeric(quantile(x = df.out.raw$flow.cfs, probs = tmp.per))

## clean up
rm(tmp.per)

## get mlog
mlog <- log10(df.flow.est.sim.period$flow + 1E-04)

## get mflow
mflow <- df.flow.est.sim.period$flow


str.obs.grp.names[order(str.obs.grp.names)] == ls(pattern = "^m[a-z]")

## save obs data
save(list = str.obs.grp.names, file = paste0(chr.dir.prime, "/ObsData/obs-group-data-flow-removed.RData"))
