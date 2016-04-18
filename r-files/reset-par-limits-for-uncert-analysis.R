## main path
chr.pest.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## get the calibrated pst file
tmp.pest <- scan(file = paste0(chr.pest.dir, 
                                  "/pest-hspf-files/upd-calib/calib.pst"),
                    sep = "\n", what = "character")
## get parameter data block
num.b <- grep("\\* parameter data", tmp.pest) + 1
num.e <- min(grep("\\*", tmp.pest[(num.b):length(tmp.pest)])) + num.b - 2
chr.par.blk <- tmp.pest[num.b:num.e]

## get par, lower-limt and upper-limit values
df.par.org <- data.frame(do.call(rbind,
                      strsplit(
                        gsub("( ){1,}",",",chr.par.blk), 
                        split = ","))[, c(1, 4, 5, 6)],
                      stringsAsFactors = FALSE)
names(df.par.org) <- c("name", "par", "ll", "ul")
df.par.org[, 2:4] <- sapply(df.par.org[, 2:4], as.numeric)


## calc new upper and lower limts
num.frac <- 0.05 # +/- fraction of par for new upper and lower limit
df.par.new <- data.frame(df.par.org[, 1:2], ll = NA, ul = NA,
                         stringsAsFactors = FALSE)
calc.ll <- function(x, y, frac) max(x * (1 - frac), y)
calc.ul <- function(x, y, frac) min(x * (1 + frac), y)
for(ii in 1:length(df.par.org$ll)) {
  df.par.new$ll[ii] <- calc.ll(df.par.org$par[ii], df.par.org$ll[ii], num.frac)
  df.par.new$ul[ii] <- calc.ul(df.par.org$par[ii], df.par.org$ul[ii], num.frac)
}

chr.new.par.ll.ul <- paste0(sprintf(fmt = "%.6E", df.par.new$par, ""), "  ",
  sprintf(fmt = "%.6E", df.par.new$ll, ""), "  ",
       sprintf(fmt = "%.6E", df.par.new$ul, ""))
tmp.blk.new <- data.frame(do.call(rbind,
                                 strsplit(
                                   gsub("( ){1,}",",",chr.par.blk), 
                                   split = ",")),
                         stringsAsFactors = FALSE)
tmp.blk.new[ ,c(4,5,6)] <- df.par.new[, c("par", "ll", "ul")]

chr.new.blk <- c()

for(kk in 1:length(tmp.blk.new[, 1])) {
  chr.new.blk <- c(chr.new.blk,
                   sprintf(fmt = "%-13s%-8s%-7s   %.6E   %.6E   %.6E   %-13s %.4f %.4f %4d", 
                           tmp.blk.new[kk, 1], tmp.blk.new[kk, 2], tmp.blk.new[kk, 3],
                           tmp.blk.new[kk, 4], tmp.blk.new[kk, 5], tmp.blk.new[kk, 6],
                           tmp.blk.new[kk, 7], 
                           as.numeric(tmp.blk.new[kk, 8]), as.numeric(tmp.blk.new[kk, 9]),
                           as.numeric(tmp.blk.new[kk, 10]))
  )
}


for(jj in 1:length(tmp.blk.new[, 1])) {
  chr.new.blk <- c(chr.new.blk, paste0(c(tmp.blk.new[jj, 1:3],
           sprintf(fmt = "%.6E", tmp.blk.new[jj, 4:6]),
           tmp.blk.new[jj,7:10]), collapse = "   "))
}
paste0(c(tmp.blk.new[ii, 1:3],
         sprintf(fmt = "%.6E", tmp.blk.new[, 4:6]),
         tmp.blk.new[,7:10]), collapse = "   ")

tmp.pest.new <- tmp.pest
tmp.pest.new[num.b:num.e] <- chr.new.blk
tmp.pest[num.b:num.e]


## write new pest file
cat(tmp.pest.new, file = paste0(chr.pest.dir, 
                   "/pest-hspf-files/upd-calib/calib-new-lims.pst"),
     sep = "\n")
