## script run monte carlo anaysis for calib.pst

## follows the preocedure recommended by John Dherty (2016-04-04)
## 1. Calibrate
## 2. Get the posterior covariance matrix using PREDUNC7
## 3. Sample that
## 4. Put each sample into a PEST control file using PARREP
## 5. Run ADDREG1 to add regularisation which tells the parameters to stay as close to initial values as possible.
## 6. Set PHIMLIM to a suitable value - a little above the best measurement objecive function that you got for calibration.
## 7. Run PEST with /i switch using JCO obtained on basis of calibrated parameter field (first iteration is thus free)
## 8. Repeat steps 4 -7.


## path to rec files
chr.unc.dir <- "\\\\deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/upd-uncert"

chr.curwd.dir <- getwd() ## get current directory to reset bact to at end of script

## file names
chr.pst.cal <- "calib.pst" ## PEST control with calibrated parameter values
chr.rec.cal <- "calib.rec" ## PEST record for calibrated parameter values
gsub("/","\\\\",chr.unc.dir)


setwd(chr.unc.dir)

shell(paste0("cd ",gsub("/","\\\\",chr.unc.dir)))

shell("m:\ ; dir")

## reset back to original working directory
setwd(chr.curwd.dir)
