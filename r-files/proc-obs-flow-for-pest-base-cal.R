## load packages
library(DVstats, quietly = TRUE) # USGS-HySep R version in DVstats
library(doBy, quietly = TRUE) # need doBy package to sums for annual, summer and winter

## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## storm dates path
chr.dir.stm.dates <- "M:/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal"

# get obs flow from Yaquina River gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/get-obs-flow-data.R")

# estimate flow for Big Elk Creek from Yaquina River Gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/estimate-flow.R")

names(df.flow.est) <- c("date", "flow")

## clean up
rm(df.flow.obs)

## pest-hspf path
chr.dir.pest.hspf <- paste0(chr.dir.prime, "/pest-hspf-files")

## get simulation period
## using the uci file for the earlier calibration that used the updated 
## simulation period
chr.uci <- scan(paste0(chr.dir.pest.hspf, "/bigelk.uci"), sep = "\n", 
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

## sub-set the flow data to the simulation period
df.flow.est <- df.flow.est[df.flow.est$date >= min(dt.sim.period) & 
                             df.flow.est$date <= max(dt.sim.period), ]

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

## clean up
rm(chr.dir.bac.obs, chr.file.obs.bac, chr.dates.bac.all, obs.data)

## flow data with the values on days when bacteria samples collected removed
df.flow.est.rm <- df.flow.est[-1 * lng.bac.flow.rows, ]

## calculate observations for pest obs groups
##
## mlog - log10 of daily flow + 1E-04
## Note: 1E-04 added to protect against log10(0)
mlog <- log10(df.flow.est.rm$flow)

##
## mbaseind <- baseflow / total flow
##
## calculate model groups
## mbaseind, base flow index 
## baseflow seperation using USGS-HySep R version in DVstats
## need continuous time-series to use hysep, so need to break flow
## data into multiple continuous time-series
## drainage area in sqr mi for Big Elk Creek at outlet, used in HySep
da.be <- 88.8
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
## last sample date only has a backward, use the entire flow data set for this
## because the rows for the bacteria samples correspond to this data.frame
df.chk$bck[length(lng.bac.flow.rows)] <- length(df.flow.est$date) -
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
  df.bnds$end[length(lng.bac.flow.rows)] <- length(df.flow.est$date)
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
    tmp.hysep88.8 <- try(hysep(Flow = df.flow.est$flow[tmp.seq], 
                               Dates = as.Date(df.flow.est$date[tmp.seq]), da = da.be,
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
rm(ii, df.chk,lng.bfi, df.bnds, da.be)
options(warn = tmp.wn)
rm(tmp.wn)
## get rows where baseflow calculated 
lng.bfi <- grep("[^-1]", df.bnds$bfi)

##
## mdiff - backward difference in daily flow
## calculate differences, this is a bacwarddifference, so I add a NA at
## the beginning of the 
mdiff <- c(NA,diff(df.flow.est$flow, lag = 1, differences = 1))

## clean up
rm(df.flow.dif, df.flow.dif.rm)

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
mdiff <- mdiff[-1 * lng.bac.flow.dif.rows]
## get rid of first row becuase can't calc diff
mdiff <- mdiff[-1]

## clean up
rm(lng.bac.flow.dif.rows)

## calculate the flow annual, winter and summer volumes 
## convert stream flow from cu ft / sec to ac-ft / day for use in volumes
## (1 cu ft / sec) * (86400 sec / day) * (1 ac-ft / 43559.9 cu ft)
df.vol <- cbind(df.flow.est.rm, 
                flow.ac.ft =  86400 * (1 / 43559.9) * df.flow.est.rm$flow)
## mvol_ann - annual volumes in ac-ft
## create factor for year
df.vol <- cbind(df.vol, 
                fac.ann  = as.factor(
                  strftime(df.vol$date, format = "%Y")))
## create factor for month used in mvol_smr and mvol_wtr calculations
df.vol <- cbind(df.vol, 
                fac.mon  = as.factor(
                  strftime(df.vol$date, format = "%b")))
## summer season, summer is Jun, Jul and Aug
lng.smr <- grep("Jun|Jul|Aug", df.vol$fac.mon)
## winter season
lng.wtr <- grep("Dec|Jan|Feb", df.vol$fac.mon)
## add season column
df.vol <- data.frame(df.vol, fac.season = "none", stringsAsFactors = FALSE)
## assign summer and winter values to season. leave spring and fall as none
df.vol$fac.season[lng.smr] <- "summer"
df.vol$fac.season[lng.wtr] <- "winter"
## convert season from character to factor
df.vol$fac.season <- as.factor(df.vol$fac.season)

## clean up
rm(lng.smr, lng.wtr)

## annual flow volume
mvol_ann <- as.numeric(
  summaryBy(flow.ac.ft ~ fac.ann, data = df.vol, FUN = sum)[ ,2])

## season fow volume
df.tmp <- summaryBy(flow.ac.ft ~ fac.ann + fac.season , data = df.vol, FUN = sum)

## mvol_smr - summer flow volumes
mvol_smr <- as.numeric(df.tmp[as.character(df.tmp$fac.season) == "summer", 
                                      "flow.ac.ft.sum"])

## mvol_wtr - winter flow volumes
mvol_wtr <- as.numeric(df.tmp[as.character(df.tmp$fac.season) == "winter", 
                                      "flow.ac.ft.sum"])
## clean up
rm(df.vol,df.tmp)

## mtime - % exceedance for flow, using 0.01%, 1%, 5%, 25%, 50%, 75%, 95%, 99%
## this is different than what Cadmus using in tsproc which is the fraction
## of time the flow is above some value. I am not going to use tsproc when
## doinmg the calculations. I will use R script

## percents used
tmp.per <- c(0.0001, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)

## calculate mtime from quantiles
mtime <- as.numeric(quantile(x = df.flow.est.rm$flow, probs = tmp.per))

## clean up
rm(tmp.per)

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
df.bac.dates <- data.frame(date = df.flow.est$date[lng.bac.flow.rows])

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
    df.flow.est.rm$flow[df.flow.est.rm$date >= df.strm.dates.reduced$begin[ii] & 
                          df.flow.est.rm$date <= df.strm.dates.reduced$end[ii]])
}
rm(ii)

## mvol_stm in cu-ft for storm convert cu-ft/sec to cu-ft/day 
## using 1 day = 86400 s
mvol_stm <- rep(-1, length(df.strm.dates.reduced$begin))

for(ii in 1:length(mvol_stm)) {
  mvol_stm[ii] <- sum(
    df.flow.est.rm$flow[df.flow.est.rm$date >= df.strm.dates.reduced$begin[ii] &
                          df.flow.est.rm$date <= df.strm.dates.reduced$end[ii]]) *
    (df.strm.dur[ii] * 86400)
}

## clean up 
rm(ii, df.strm.dur, df.strm.dates, df.strm.dates.raw, df.strm.dates.reduced,
   df.bac.dates)
