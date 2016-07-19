## purpose of this script is to process modelled flow data for use in pest 
## prediction error analaysis for the pre-calibration of HSPF model for BigElk 
## Creek. For the predictive error analysis, only obs flow flow on days when 
## bacteria samples were collected are used.
## 
## load packages
##library(stringr)

## paths
## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"
chr.dir.pred.pre.cal <- paste0(chr.dir.prime,
                               "/pest-hspf-files/pred-unc-pre-cal")

## get read-pltgen function
source(file = paste0(chr.dir.prime, "/r-files/read-pltgen.R"))

## model output for pest file name
chr.file.model.in <- "modflow.out"
## model output for pest file name
chr.file.model.out <- "model.out"
## name for instruction file for reading model output to pest
chr.file.model.ins <- "model.ins"

## get observed flow
load(file = paste0(chr.dir.pred.pre.cal, "/obs-flow.RData"))

## read hspf output
df.flow.mod.raw <- read.pltgen(paste0(chr.dir.pred.pre.cal, "/", 
                                 chr.file.model.in))


## get rows of bacteria samples dates in sim period flow
lng.bac.flow.rows.sim.period <- grep(
  pattern = paste0(strftime(df.flow.est.pred$date , format = "%Y-%m-%d"), collapse = "|"),
  strftime(df.flow.mod.raw$date, format = "%Y-%m-%d"))

## get flow data for days bacteria samples collected
df.flow.mod.pred <- df.flow.mod.raw[lng.bac.flow.rows.sim.period, ]

## convert af-ft/day to cfs
## 43559.9 cu-ft = 1 ac-ft, 1 day = 24 hr, 1 hr = 3600 sec
df.flow.mod.pred <- cbind(df.flow.mod.pred, 
                    flow.cfs = df.flow.mod.pred$rovol * 43559.9 / (24*3600))


## add residual to model data.frame
df.flow.mod.pred <- cbind(df.flow.mod.pred, 
                          res = df.flow.est.pred$flow - df.flow.mod.pred$flow.cfs)

## clean up
rm(df.flow.mod.raw, df.flow.est.pred)

## get length of variable names and set column spacing
## column spacing
chr.col.spc <- "     "
## name of observation
tmp.grp <- "prediction"
## get number of digits for counter of observations
lng.num.obs.dgt <- nchar(length(df.flow.mod.pred$res)) + 1
## get max number of charcters for obs name
lng.max.nchar <- nchar(paste0(tmp.grp, "_0", length(df.flow.mod.pred$res)))

## model output block
chr.mod.out <- paste0(sprintf(
  paste0("%-",lng.max.nchar,"s"),
  sprintf(paste0(tmp.grp, paste0("_%0", lng.num.obs.dgt, "i")), 
          1:length(df.flow.mod.pred$res))),
  chr.col.spc,
  sprintf("% 8.4E", df.flow.mod.pred$res))

## contruct model.ins file
## get starting column
lng.ncol.start <- 
 nchar(
   paste0(
     sprintf(
       paste0("%-",lng.max.nchar,"s"),
       sprintf(
         paste0(tmp.grp, paste0("_%0", lng.num.obs.dgt, "i")), 1)),
     chr.col.spc)) + 1
## get end column
lng.ncol.end <- lng.ncol.start +
  nchar(sprintf("% 8.4E", df.flow.mod.pred$res[1]))
## write file contents
chr.mod.ins <- c("pif $",
  paste0("l1  [",
         sprintf(paste0(tmp.grp, paste0("_%0", lng.num.obs.dgt, "i")),
                 1:length(df.flow.mod.pred$res)), 
         "]", lng.ncol.start, ":", lng.ncol.end))

## write model.out file
write.table(chr.mod.out, 
            file = paste0(chr.dir.pred.pre.cal,"/",chr.file.model.out), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

## write model.ins file
write.table(chr.mod.ins, 
            file = paste0(chr.dir.pred.pre.cal,"/",chr.file.model.ins), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
