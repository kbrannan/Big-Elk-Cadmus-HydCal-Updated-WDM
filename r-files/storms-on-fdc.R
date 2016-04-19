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
  ## get modeled flow values
  tmp.res.file <- readLines(con.res)
  ## close the file connection
  close(con.res)
  ## get rows for storm peaks
  tmp.peak.rows <- grep("mpeak([0-9]){1,}_max", tmp.res.file)
  ## get rows for storm volumes
  tmp.vol.rows <- grep("mvol_stm_([0-9]){1,}", tmp.res.file)
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

  ## estimate ecdf for obs and model flows, then use ecdfs to get probs for 
  ## storm flows
    
  tmp.mflow <- as.numeric(
    substr(tmp.res.file[min(grep("mflow", tmp.res.file)):
                          max(grep("mflow", tmp.res.file))], 46, 62))
  ## close the file connection
  close(con.res)
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