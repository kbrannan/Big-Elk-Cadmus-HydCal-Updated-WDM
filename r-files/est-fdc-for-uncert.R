## load functions
source("M:/Models/Bacteria/LDC/Calculations/Rscripts/LDC Functions.R")

## estimate fdc
## stations at outlet is LASAR 34453
tmp.one.station <- 34453
tmp.ss.est.fn <- paste0("st",tmp.one.station,".xml")
df.fdc.ss.est <- fdc.ss.estimate(ss.fn=tmp.ss.est.fn, ss.path=get.path("StreamStatsBacteria"))

## create data.frame of FDCs
df.fdc <- data.frame(src = "eq", x = df.fdc.ss.est$FDPercent,
                     y = df.fdc.ss.est$FDEst, par = "est", 
                     stringsAsFactors = FALSE)
df.fdc <- rbind(df.fdc, data.frame(src = "eq", x = df.fdc.ss.est$FDPercent,
                     y = df.fdc.ss.est$lower, par = "lower", 
                     stringsAsFactors = FALSE))

df.fdc <- rbind(df.fdc, data.frame(src = "eq", x = df.fdc.ss.est$FDPercent,
                                   y = df.fdc.ss.est$upper, par = "upper", 
                                   stringsAsFactors = FALSE))

## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/uncert-upd"

## get sub-dirs for the uncert runs
chr.sub.dirs <- grep("^uncert[^-]", list.dirs(path = chr.uncert.rerun.dir, 
                          full.names = FALSE, recursive = FALSE),
                     value = TRUE)

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
                           "/", chr.res.files[jj]), open = "r")
## check connection
    if(isOpen(con.res)) {
      ## get modeled flow values
      tmp.res.file <- readLines(con.res)
      tmp.mflow <- as.numeric(
        substr(tmp.res.file[min(grep("mflow", tmp.res.file)):
                              max(grep("mflow", tmp.res.file))], 46, 62))
      ## close the file connection
      close(con.res)
      ## calc quantile (flows) for the %-exceed of USGS equation
      tmp.quant <- quantile(tmp.mflow, 1 - df.fdc.ss.est$FDPercent, names = FALSE)
      ## add current fdc to data.frame
      df.fdc <- rbind(df.fdc, 
                      data.frame(src = gsub("\\.res","",chr.res.files[jj]), 
                                 x = df.fdc.ss.est$FDPercent,
                                 y = tmp.quant, par = "est", 
                                 stringsAsFactors = FALSE))
      
      }
      rm(list=ls(pattern="^tmp//.*")) ## clean up
  }
}

save(list = c("df.fdc", "df.fdc.ss.est"), 
     file = paste0(chr.uncert.rerun.dir, "/uncert-fdc-est.RData"))



