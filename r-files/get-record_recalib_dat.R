## path
chr.uncert.dir <- "c:/temp/uncert-upd"

## get uncert sub-dirs
chr.uncert.dirs <- list.dirs(path = chr.uncert.dir, recursive = FALSE, 
                             full.names = FALSE)
chr.uncert.dirs <- grep("uncert[0-9]{8}", chr.uncert.dirs, value = TRUE)

## create data.frame for summary
df.phis <- data.frame(par.set="none", phi1 = NA, phi2 = NA, 
                      stringsAsFactors = FALSE)


## seq used to orginize data.frame
num.seq <- cbind(start=seq(from = 1, to = length(tmp.rec03), by = 3),
                 end=seq(from = 3, to = length(tmp.rec03), by = 3))

for(ii in 1:length(chr.uncert.dirs)) {

  ## read phi summary file  
  tmp.rec <- scan(file = paste0(chr.uncert.dir, "/", 
                                chr.uncert.dirs[ii], "/record_recalib.dat"),
                  skip = 2, sep = "\n", what = "character", quiet = TRUE)
## remove extra characters
  tmp.rec00 <- tmp.rec[-1 * grep("(UNCERT)|(model calls)", tmp.rec)]
  tmp.rec01 <- gsub("^( ){1,}Sum of squared weighted residuals \\(ie phi\\)( ){1,}\\=", "", tmp.rec00)
  tmp.rec02 <- tmp.rec01[-1 * grep("\\' \\'", tmp.rec01)]
  tmp.rec03 <- gsub("(^( ){1,})|(( ){1,}$)","",tmp.rec02)

  
  
  tmp.phis <- rbind(tmp.rec03[num.seq[1,1]:num.seq[1,2]])
  for(jj in 2:length(num.seq[,1])) {
    tmp.phis <- rbind(tmp.phis,rbind(tmp.rec03[num.seq[jj,1]:num.seq[jj,2]]))
  }
  tmp.phis <- data.frame(par.set=tmp.phis[, 1], 
                         phi1 = as.numeric(tmp.phis[, 2]), 
                         phi2 = as.numeric(tmp.phis[, 3]), 
                         stringsAsFactors = FALSE)
  df.phis <- rbind(df.phis, tmp.phis)
  rm(list=ls(pattern="^tmp\\."))
}

df.phis <- df.phis[-1, ]

min(df.phis[,2])
min(df.phis[,3])

max(df.phis[,2])
max(df.phis[,3])
