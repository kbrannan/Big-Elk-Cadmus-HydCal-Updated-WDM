## load packages
#library(ggplot2, quietly = TRUE)
#library(reshape, quietly = TRUE)
#library(gtable, quietly = TRUE)

## load functions
source("M:/Models/Bacteria/LDC/Calculations/Rscripts/LDC Functions.R")

## estimate fdc
## stations at outlet is LASAR 34453
tmp.one.station <- 34453
tmp.ss.est.fn <- paste0("st",tmp.one.station,".xml")
df.fdc.ss.est <- fdc.ss.estimate(ss.fn=tmp.ss.est.fn, ss.path=get.path("StreamStatsBacteria"))

## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert/uncert-rerun"

## get sub-dirs for the uncert runs
chr.sub.dirs <- grep("^uncert", list.dirs(path = chr.uncert.rerun.dir, 
                          full.names = FALSE, recursive = FALSE),
                     value = TRUE)
## create empty vector for names of runs to keep
chr.kept.runs <- c()

## loop through sub-dirs
for(ii in 1:length(chr.sub.dirs)) {
## get the file names for the residual (res) files for the current sub-dir
  chr.res.files <- list.files(
    path = paste0(chr.uncert.rerun.dir, "/", chr.sub.dirs[ii]),
    pattern = "*.\\.res$")
## process files for the current sub-dir
  for(jj in 1:length(chr.res.files)) {
## open connection to the current res file
    con.res <- file(paste0(chr.uncert.rerun.dir, "/", chr.sub.dirs[ii], 
                           "/", chr.res.files[jj]))

## get modeled flow values
    tmp.res.file <- readLines(con.res)
    tmp.mflow <- as.numeric(
      substr(tmp.res.file[min(grep("mflow", tmp.res.file)):
                            max(grep("mflow", tmp.res.file))], 46, 62))
## close the file connection
    close(con.res)
## calc quantile (flows) for the %-exceed of USGS equation
    tmp.quant <- quantile(tmp.mflow, 1 - df.fdc.ss.est$FDPercent, names = FALSE)
## chec if current fdc flows are within the USGS error bars 
    if(sum(df.fdc.ss.est$lower <= tmp.quant & 
           tmp.quant <= df.fdc.ss.est$upper) >= 
       floor(0.75*length(tmp.quant))) {
## if yes keep file name append to list of files
      chr.kept.runs <- c(chr.kept.runs, 
                         paste0(chr.uncert.rerun.dir, "/", 
                                chr.sub.dirs[ii], "/", chr.res.files[jj]))
      
    }
      rm(list=ls(pattern="^tmp//.*")) ## clean up
  }
}



