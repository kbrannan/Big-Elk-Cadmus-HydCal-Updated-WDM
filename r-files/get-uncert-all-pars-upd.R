

## main path for uncert
##chr.uncert.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files"
chr.uncert.dir <- "c:/temp"

## main path for uncert-rerun
chr.uncert.rerun.dir <- paste0(chr.uncert.dir, "/upd-uncert")

## get all the par files used in uncert-rerun
##chr.par.files <- list.files(path = chr.uncert.rerun.dir, pattern = "\\.par$",
##                            recursive = TRUE, full.names = TRUE)
chr.par.files <- list.files(path = paste0(chr.uncert.rerun.dir, "/randpar-files"), pattern = "\\.par$",
                            recursive = TRUE, full.names = TRUE)

## get calibrated par values
##tmp.pars <- scan(file = paste0(chr.uncert.dir, "/upd-calib/calib.par"), 
##                 what = "character", sep = "\n", quiet = TRUE, skip = 1)
tmp.pars <- scan(file = "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-calib/calib.par", 
                 what = "character", sep = "\n", quiet = TRUE, skip = 1)

tmp.par <- do.call(rbind, 
       strsplit(gsub(" {1,}", ",", gsub("(^( ){1,})|(( ){1,}$)", "", tmp.pars)),
                split = ","))[ , c(-3,-4)]
## create par data.frame
df.pars <- data.frame(par = tmp.par[, 1], run = "cal", val = tmp.par[, 2], 
                      stringsAsFactors = FALSE)

## clean up
rm(list=ls(pattern = "^tmp\\."))

## loop through res files and estimate FDC for each run
for(ii in 1:length(chr.par.files)) {
  ## get calibrated par values
  tmp.pars <- scan(file =chr.par.files[ii], 
                   what = "character", sep = "\n", quiet = TRUE, skip = 1)
  tmp.par <- do.call(rbind, 
                     strsplit(gsub(" {1,}", ",", gsub("(^( ){1,})|(( ){1,}$)", "", tmp.pars)),
                              split = ","))[ , c(-3,-4)]
  ## create par data.frame
  df.pars <- rbind(df.pars, 
                   data.frame(
                     par = tmp.par[, 1], 
                     run = gsub("(^.*/)|(\\.par$)","", chr.par.files[ii]), 
                     val = tmp.par[, 2], stringsAsFactors = FALSE))
  ## clean up
  rm(list=ls(pattern = "^tmp\\."))
}

## add more descriptors
df.tmp <- cbind(df.pars, lc = "all", proc = "none", src = "uncert", stringsAsFactors = FALSE)

## lc - land cover, values are "for" - forest (100), "pas" - pasture (200),
## "dev" - developed (300) and "all" - all lc (000) 
df.tmp$lc[grep("[a-z]{1,}1[0-9]{2}", unique(df.pars$par))] <- "for"
df.tmp$lc[grep("[a-z]{1,}2[0-9]{2}", unique(df.pars$par))] <- "pas"
df.tmp$lc[grep("[a-z]{1,}3[0-9]{2}", unique(df.pars$par))] <- "dev"
df.tmp$lc[grep("[a-z]{1,}0[0-9]{2}", unique(df.pars$par))] <- "all"
unique((df.tmp$lc))

## proc - hydrologic process, values are "wtb" - water balance, 
## "sur" - surface flow and "sub" - sub-surface flow
df.tmp$proc[gsub("[0-9]{1,}","", df.pars$par) == "lzsn"   |
            gsub("[0-9]{1,}","", df.pars$par) == "infilt" |
            gsub("[0-9]{1,}","", df.pars$par) == "lzetp"] <- "wtb"
df.tmp$proc[gsub("[0-9]{1,}","", df.pars$par) == "uzsn" |
            gsub("[0-9]{1,}","", df.pars$par) == "cepsc"] <- "sur"
df.tmp$proc[gsub("[0-9]{1,}","", df.pars$par) == "deepfr" |
            gsub("[0-9]{1,}","", df.pars$par) == "basetp" |
            gsub("[0-9]{1,}","", df.pars$par) == "agwetp" |
            gsub("[0-9]{1,}","", df.pars$par) == "intfw"  |
            gsub("[0-9]{1,}","", df.pars$par) == "agwrc"  |
            gsub("[0-9]{1,}","", df.pars$par) == "irc"]   <- "sub"
## add src factor
df.tmp$src[df.tmp$run == "cal"] <- "cal"
## re-order columns to make nice
df.tmp2 <- df.tmp[, c("src", "par", "lc", "proc", "run", "val")]
## create factors
df.tmp2$src <- factor(df.tmp2$src, levels = c("cal", "uncert"))
df.tmp2$par <- factor(df.tmp2$par)
df.tmp2$proc <- factor(df.tmp2$proc, levels = c("sur", "sub", "wtb" ))
df.tmp2$lc <- factor(df.tmp2$lc, levels = c("for", "pas", "dev", "all"))
## final data.frame
df.pars <- df.tmp2
## clean up
rm(list=ls(pattern = "df\\.tmp.*"))

## save the pars to file
save(df.pars, file = paste0(chr.uncert.dir, "/all-par-vals.RData"))
