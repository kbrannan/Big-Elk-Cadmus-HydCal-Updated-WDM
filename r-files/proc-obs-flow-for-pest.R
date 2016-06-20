## load packages
library(DVstats, quietly = TRUE) # USGS-HySep R version in DVstats
library(doBy, quietly = TRUE) # need doBy package to sums for annual, summer and winter

## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## file name of big elk flow data with the flows for dates of bacteria samples removed
chr.file.flow.est.removed <- "obs-flow-removed.RData"

## bacteria data path
chr.dir.bac.obs <- paste0(chr.dir.prime, "/ObsData")

## load obs flow data
load(file = paste0(chr.dir.bac.obs, "/", chr.file.flow.est.removed))


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
for(ii in 1:length(df.bnds$bfi)) {
  if(df.bnds$chk[ii] == 1) {
    tmp.seq <- seq.int(from = df.bnds$start[ii], to = df.bnds$end[ii])
    ## use try function becuase can get error from hysep if there is only
    ## one minima (i think this means a constant slope)
    tmp.hysep88.8 <- try(hysep(Flow = df.flow.est$mean_daily_flow_cfs[tmp.seq], 
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
## get rows where baseflow not calculated 
lng.bfi <- grep("[^-1]", df.bnds$bfi)

## calculate baseflow in the same way as calculated from the observed data
## as a wieght average. The weight is the length of the segment
mbaseind <- sum(df.bnds$len[lng.bfi] * df.bnds$bfi[lng.bfi]) / sum(df.bnds$len[lng.bfi])

## clean up
rm(ii, df.chk,lng.bfi, lng.bac.flow.rows)

## convert stream flow from cu ft / sec to ac-ft / day for use in volumes
## (1 cu ft / sec) * (86400 sec / day) * (1 ac-ft / 43559.9 cu ft)
df.mod <- cbind(df.mod, 
                flow.ac.ft =  86400 * (1 / 43559.9) * df.mod$Rch18.flow)

## mvol_ann - annual volumes in ac-ft
## create factor for year
df.mod <- cbind(df.mod, 
                fac.ann  = as.factor(
                  strftime(df.mod$tmp.date, format = "%Y")))
mvol_ann <- as.numeric(
  summaryBy(flow.ac.ft ~ fac.ann, data = df.mod, FUN = sum)[ ,2])

## create factor for month used in mvol_smr and mvol_wtr calculations
df.mod <- cbind(df.mod, 
                fac.mon  = as.factor(
                  strftime(df.mod$tmp.date, format = "%b")))

## create factor for month used in mvol_smr and mvol_wtr calculations

df.tmp <- data.frame(mon=as.character(df.mod$fac.mon), season = "none", 
                     stringsAsFactors = FALSE)

## summer season, summer is Jun, Jul and Aug
lng.smr <- grep("Jun|Jul|Aug", df.tmp$mon)

## winter season
lng.wtr <- grep("Dec|Jan|Feb", df.tmp$mon)

## assign summer and winter values to season. leave spring and fall as none
df.tmp$season[lng.smr] <- "summer"
df.tmp$season[lng.wtr] <- "winter"

## add season as factor to df.mod 
df.mod <- data.frame(df.mod, fac.season = as.factor(df.tmp$season))

## clean up
rm(df.tmp, lng.smr, lng.wtr)

df.vol.seasons <- summaryBy(flow.ac.ft ~ fac.ann + fac.season , data = df.mod, FUN = sum)


## mvol_smr - summer volumes in ac-ft
mvol_smr <- as.numeric(df.vol.seasons[as.character(df.vol.seasons$fac.season) == "summer", 
                           "flow.ac.ft.sum"])

## mvol_wtr
mvol_wtr <- as.numeric(df.vol.seasons[as.character(df.vol.seasons$fac.season) == "winter", 
                           "flow.ac.ft.sum"])
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
## set names
names(df.strm.dates) <- c("begin", "end")

## clean up
rm(df.strm.dates.raw)

## storm durations in days
df.strm.dur <- as.numeric(df.strm.dates$end - df.strm.dates$begin)

## mpeak
mpeak <- rep(-1, length(df.strm.dates$begin))

for(ii in 1:length(mpeak)) {
  mpeak[ii] <- max(df.mod[df.mod$tmp.date >= df.strm.dates$begin[ii] & 
               df.mod$tmp.date <= df.strm.dates$end[ii], ]$Rch18.flow)
}
rm(ii)

## mvol_stm in cu-ft for storm convert cu-ft/sec to cu-ft/day 
## using 1 day = 86400 s
mvol_stm <- rep(-1, length(df.strm.dates$begin))

for(ii in 1:length(mvol_stm)) {
  mvol_stm[ii] <- sum(df.mod[df.mod$tmp.date >= df.strm.dates$begin[ii] & 
                            df.mod$tmp.date <= 
                              df.strm.dates$end[ii], ]$flow) * 
    (df.strm.dur[ii] * 86400)
}
## mtime - % exceedance for flow, using 0.01%, 1%, 5%, 25%, 50%, 75%, 95%, 99%
## this is different than what Cadmus using in tsproc which is the fraction
## of time the flow is above some value. I am not going to use tsproc when
## doinmg the calculations. I will use R script

## percents used
tmp.per <- c(0.0001, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)

mtime <- as.numeric(quantile(x = df.mod$Rch18.flow, probs = tmp.per))

## clean up
rm(tmp.per)


## get observational groups names

# get rows where the block names are
chr.dir.pst <- "m:/models/bacteria/hspf/bigelkhydrocal201601/pest-files"
str.control <- scan(paste0(chr.dir.pst,"/control.pst"), sep = "\n", 
                    what = "character", quiet = TRUE)
tmp.blk.hd <- grep("\\*", str.control)
str.obs.grp.names <- 
  str.control[(tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                               str.control[tmp.blk.hd])] + 1):
                (tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                                 str.control[tmp.blk.hd]) + 1] - 1)]

lng.0.pd <- nchar(as.character(max(sapply(mget(str.obs.grp.names), length))))

lng.0.pd <- nchar(as.character(sapply(mget(str.obs.grp.names), length)))

to.df.cur.data <- function(x) {
  n.pad <- nchar(as.character(length(get(x))))
  y <- data.frame(
    name = paste0(x, "_", 
                  sprintf(fmt = paste0("%0", n.pad, "d"), 
                          1:length(get(x)))), 
    val = get(x), stringsAsFactors = FALSE)
  return(y)
} 

tmp.blk.data <- do.call(rbind, lapply(str.obs.grp.names, FUN = to.df.cur.data))


##
## write output to filed format text file

## get length of longest variable name and add 5
#lng.name <- max(nchar(attr(tmp.blk.data, "names"))) + 5
lng.name <- max(nchar(tmp.blk.data$name)) + 5



## write output to chracater vector. the format is 2s, variable name left 
## justified and width 5 plus length of longest variable name and value as 
## 1.5E+00. 
## total width of variable name and value is length of longest variable name + 5
## + 11 = length of longest variable name + 16
chr.mod.output <- paste0(
  sprintf(paste0("  %-", lng.name, "s"), tmp.blk.data$name), 
  sprintf("%.5E", tmp.blk.data$val))

write.table(data.frame(out=chr.mod.output), 
            file = paste0(chr.dir.pst, "/model.out"), 
            quote = FALSE, col.names = FALSE, row.names = FALSE,
            sep = "\n")


##
## write model.ins file
lng.var.val <- nchar(chr.mod.output[1])

chr.mod.ins <- c("pif $", paste0(
  sprintf("l1  [%s]",tmp.blk.data$name), lng.name, ":",lng.var.val + 1))

write.table(data.frame(ins=chr.mod.ins), 
            file = paste0(chr.dir.pst, "/model.ins"), 
            quote = FALSE, col.names = FALSE, row.names = FALSE,
            sep = "\n")


