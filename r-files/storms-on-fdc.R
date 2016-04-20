## main path
chr.main.dir <- "m:/models/bacteria/hspf/big-elk-cadmus-hydcal-updated-wdm/pest-hspf-files/upd-calib"

## fdc path 
chr.fdc.dir <- "m:/models/bacteria/hspf/big-elk-cadmus-hydcal-updated-wdm/pest-hspf-files/upd-uncert"

## load fdc RData
load(file = paste0(chr.fdc.dir, "/uncert-fdc-est.RData"))

## esitmate fdc for obs storms
## open connection to the current res file
con.res <- file(paste0(chr.main.dir, "/calib.res"), open = "r")
## check connection
if(isOpen(con.res)) {
  ## get flow data
  tmp.res.file <- readLines(con.res)
  ## close the file connection
  close(con.res)
  ## get storm peak data
  tmp.peak.rows <- grep("mpeak([0-9]){1,}_max", tmp.res.file)
  tmp.stm.peak.chr <- do.call(rbind,
                              strsplit(
                                gsub("( ){1,}",",", 
                                     tmp.res.file[tmp.peak.rows]), 
                                split = ","))
  gsub("[^0-9]","", tmp.stm.peak.chr[, 2])
  as.numeric(tmp.stm.peak.chr[, 4])
  as.numeric(tmp.stm.peak.chr[, 5])
  tmp.stm.peak <- data.frame(src = "calib", x = NA, 
                             y = as.numeric(tmp.stm.peak.chr[, 4]), 
                             par = "peak", 
                             run = paste0("stm", 
                                          gsub("[^0-9]","", 
                                               tmp.stm.peak.chr[, 2])))
  tmp.stm.peak <- rbind(tmp.stm.peak,
                        data.frame(src = "obs", x = NA, 
                             y = as.numeric(tmp.stm.peak.chr[, 5]), 
                             par = "peak", 
                             run = paste0("stm", 
                                          gsub("[^0-9]","", 
                                               tmp.stm.peak.chr[, 2]))))
  
  ## get rows for storm volumes
  tmp.vol.rows <- grep("mvol_stm_([0-9]){1,}", tmp.res.file)
  tmp.stm.vol.chr <- do.call(rbind,
                              strsplit(
                                gsub("( ){1,}",",", 
                                     tmp.res.file[tmp.vol.rows]), 
                                split = ","))
  tmp.stm.vol <- data.frame(src = "calib", x = NA, 
                             y = as.numeric(tmp.stm.vol.chr[, 4]), 
                             par = "vol", 
                             run = paste0("stm", 
                                          gsub("[^0-9]","", 
                                               tmp.stm.vol.chr[, 2])))
  tmp.stm.vol <- rbind(tmp.stm.vol,
                        data.frame(src = "obs", x = NA, 
                                   y = as.numeric(tmp.stm.vol.chr[, 5]), 
                                   par = "peak", 
                                   run = paste0("stm", 
                                                gsub("[^0-9]","", 
                                                     tmp.stm.vol.chr[, 2]))))
  ## get storm dates
  tmp.stm.chr <- scan(file=paste0(chr.main.dir, "/dates_stm.dat"),
                      sep = "\n", what = "character")
  tmp.stm.date.chr <- 
    do.call(
      rbind,
      strsplit(gsub("( ){1,}",",",tmp.stm.chr), split = ","))[, c(1,3)]
  tmp.stm.date <- data.frame(
    bgn =strptime(tmp.stm.date.chr[, 1], format = "%m/%d/%Y"),
    end = strptime(tmp.stm.date.chr[, 2], format = "%m/%d/%Y"))
  tmp.stm.date <- cbind(tmp.stm.date,
                        days = round(as.numeric(tmp.stm.date[, 2] - tmp.stm.date[, 1])))

  ## ave storm flow
  tmp.stm.ave.flow <- tmp.stm.vol
  tmp.stm.ave.flow$y <- (tmp.stm.vol$y / rep(tmp.stm.date$days, 2)) * 43560 / ( 24 * 3600)
  
  
  ## estimate ecdf for obs and model flows, then use ecdfs to get probs for 
  ## storm flows
  
  tmp.mflow <- as.numeric(
    substr(tmp.res.file[min(grep("mflow", tmp.res.file)):
                          max(grep("mflow", tmp.res.file))], 46, 62))
  ## calc quantile (flows) for the %-exceed of USGS equation
  tmp.quant <- quantile(tmp.mflow, 1 - df.fdc.ss.est$FDPercent, names = FALSE)
  ## add current fdc to data.frame
  df.fdc <- rbind(df.fdc, 
                  data.frame(src = "calib", 
                             x = df.fdc.ss.est$FDPercent,
                             y = tmp.quant, par = "est",
                             run = "calib",
                             stringsAsFactors = FALSE))
}
rm(list=ls(pattern="^tmp//.*.")) ## clean up