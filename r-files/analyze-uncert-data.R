## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert/uncert-rerun"

## get names of runs selected
load(paste0(chr.uncert.rerun.dir, "/uncert-run-kept-list.RData"))

## loop through res files and estimate FDC for each run

## read residuals file
chr.res <- scan(file = paste0(chr.dir,"/org-calib/Final_Deliverables_EPA_July2012/PEST_end/control.res"),
                what = "character", sep = "\n", quiet = TRUE)
## replace * with x in field names
chr.res[1] <- gsub("\\*","x",chr.res[1])

## replace spaces among columns with comma
chr.res <- gsub("( ){1,}",",",chr.res)

## convert chracter vector to data.frame
df.res <- data.frame(do.call(rbind,strsplit(chr.res,split = ",")), 
                     stringsAsFactors = FALSE)

## remove first column becuase it is empty
df.res <- df.res[ ,-1]

## first row is names for columns
names(df.res) <- df.res[1, ]

## discard first row
df.res <- df.res[-1, ]
