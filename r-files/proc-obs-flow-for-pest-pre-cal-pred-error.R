## purpose of this script is to process observed flow data for use in pest 
## prediction error analaysis for the pre-calibration of HSPF model for BigElk 
## Creek. For the predictive error analysis, only obs flow flow on days when 
## bacteria samples were collected are used.
## 
## load packages
library(stringr)

## paths
## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"
chr.dir.pred.pre.cal <- paste0(chr.dir.prime,
                               "/pest-hspf-files/pred-unc-pre-cal")

## new prest control file name
chr.file.pest.new <- "control-pred-err-pre-cal.pst"


## get simulation period
## using the uci file for the earlier calibration that used the updated 
## simulation period
chr.uci <- scan(paste0(chr.dir.pred.pre.cal, "/bigelk.uci"), sep = "\n", 
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

## file name of big elk flow data with the flows for dates of bacteria 
## samples removed
chr.file.flow.est.removed <- "obs-flow-removed.RData"

## bacteria data path
chr.dir.bac.obs <- paste0(chr.dir.prime, "/ObsData")

## load obs flow data
load(file = paste0(chr.dir.bac.obs, "/", chr.file.flow.est.removed))

## sub-set reduced flow data for simulation period
df.flow.est.sim.period <- data.frame(date = df.flow.est$date,
                                             flow = df.flow.est$mean_daily_flow_cfs)
df.flow.est.sim.period <- 
  df.flow.est.sim.period[
    df.flow.est.sim.period$date >= dt.sim.period[1] &
      df.flow.est.sim.period$date <= dt.sim.period[2], ]

## get rows of bacteria samples dates in sim period flow
lng.bac.flow.rows.sim.period <- grep(
  pattern = paste0(chr.dates.bac.unique, collapse = "|"),
  strftime(df.flow.est.sim.period$date, format = "%Y-%m-%d"))

## get flow data for days bacteria samples collected
df.flow.est.pred <- df.flow.est.sim.period[lng.bac.flow.rows.sim.period, ]

## save opbs flow data to file for use in processing of model output
save(df.flow.est.pred, file = paste0(chr.dir.pred.pre.cal, "/obs-flow.RData"))

## clean up
rm(df.flow.est, df.flow.est.reduced, df.flow.est.sim.period)


# get template pest control file for predictive uncertainty
chr.control.tpl <- scan(paste0(chr.dir.pred.pre.cal,"/control-pred-unc-tpl.pst"), sep = "\n", 
                    what = "character", quiet = TRUE)

## new control
chr.control.new <- chr.control.tpl


## set number of observations
chr.n.obs <- do.call(rbind,str_split(chr.control.new[4], pattern = "( ){1,}"))
chr.n.obs[3] <- as.character(length(df.flow.est.pred$flow))
chr.control.new[4] <- paste0(chr.n.obs, collapse = "     ")

## get rows where observed data will go
lng.blk.hd <- grep("\\*", chr.control.new)
lng.obs <- grep("\\* observation data", chr.control.new[lng.blk.hd])

## create observed data block
## create string of obs for pest control file
chr.obs.blk <- ""
chr.col.spc <- "     "
## name of observation
tmp.grp <- "prediction"
## get number of digits for counter of observations
lng.num.obs.dgt <- nchar(length(df.flow.est.pred$flow)) + 1
## get max number of charcters for obs name
lng.max.nchar <- nchar(paste0(tmp.grp, "_0", length(df.flow.est.pred$flow)))
## create temporary block, using same code used in the write-pst.R
## so creating the same variables used there
tmp.data <- df.flow.est.pred$flow
tmp.blk <- ""  
for(jj in 1:length(tmp.data)) {
  tmp.nme <- sprintf(paste0("%-",lng.max.nchar,"s"),sprintf(paste0(tmp.grp, paste0("_%0", lng.num.obs.dgt, "i")), jj))
##  tmp.val <- sprintf("%8.4E", tmp.data[jj])
  tmp.val <- sprintf("%8.4E", 0) ## val is zero for control file
##  tmp.wtg <- sprintf("%8.4E", tmp.wt * abs(1/tmp.data[jj])) ## initial weight set to inverse of value
  tmp.wtg <- sprintf("%8.4E", 1) ## weight same for all obs used in prediction
  tmp.blk <- c(tmp.blk,
               paste0(tmp.nme, chr.col.spc, tmp.val, chr.col.spc, tmp.wtg,
                      chr.col.spc, tmp.grp))
  rm(tmp.nme, tmp.val, tmp.wtg)
}
tmp.blk <- tmp.blk[-1]
chr.obs.blk <- c(chr.obs.blk, tmp.blk)
chr.obs.blk <- chr.obs.blk[-1]
## clean up
rm(tmp.grp, tmp.data, tmp.blk, jj, chr.col.spc)

## add obd data to pest control file
chr.control.new <- 
  c(chr.control.new[1:lng.blk.hd[lng.obs]],
    chr.obs.blk,
    chr.control.new[lng.blk.hd[lng.obs + 1]:length(chr.control.new)])

## write updated control file
write.table(chr.control.new, 
            file = paste0(chr.dir.pred.pre.cal,"/", chr.file.pest.new), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
