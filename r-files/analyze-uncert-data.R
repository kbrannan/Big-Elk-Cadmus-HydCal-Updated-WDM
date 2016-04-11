## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert/uncert-rerun"

## get names of runs selected
load(paste0(chr.uncert.rerun.dir, "/uncert-run-kept-list.RData"))

chr.kept.runs.res <- gsub("\\.rec$", ".res", chr.kept.runs)

## loop through res files and estimate FDC for each run
for(ii in 1:length(chr.kept.runs.res)) {
  ## read residuals file
  tmp.chr.res <- scan(file = chr.kept.runs.res[ii], what = "character", 
                      sep = "\n", quiet = TRUE)
  ## replace * with x in field names
  tmp.chr.res[1] <- gsub("\\*","x", tmp.chr.res[1])
  
  ## replace spaces among columns with comma
  tmp.chr.res <- gsub("( ){1,}",",",tmp.chr.res)
  
  ## convert chracter vector to data.frame
  tmp.df.res <- data.frame(do.call(rbind,strsplit(tmp.chr.res,split = ",")), 
                       stringsAsFactors = FALSE)
  
  ## remove first column becuase it is empty
  tmp.df.res <- tmp.df.res[ ,-1]
  
  ## first row is names for columns
  names(tmp.df.res) <- tmp.df.res[1, ]
  
  ## discard first row
  tmp.df.res <- tmp.df.res[-1, ]

  
}

