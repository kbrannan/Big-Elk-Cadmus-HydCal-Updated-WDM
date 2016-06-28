## set path
chr.dir <- "m:/models/bacteria/hspf/Big-Elk-Cadmus-HydCal-Updated-WDM"

## observed data for control file processed by ""
chr.file.obs <- "obs-group-data-flow-removed.RData"

## load the observed data processed by "gather-raw-data-files.R"
load(file=paste0(chr.dir,"/ObsData/",chr.file.obs))

## get obs names (obs groups), assumes all/only obs variable names in workspace start with "m"
chr.obs.grp <- ls(pattern = "^m.*")

## number of observation groups
lng.num.obs.grp <- length(chr.obs.grp)

## get numbner of observations, 
lng.num.obs <- sum(sapply(chr.obs.grp, 
                      function(chr.name = NULL) 
                        length(eval(as.name(chr.name)))))
## get the number of digits to use in format of obs names
lng.num.obs.dgt <- max(
  nchar(
    max(
      sapply(chr.obs.grp, function(chr.name = NULL) 
        length(eval(as.name(chr.name)))))), 1)

## get max number of charcters for obs name
lng.max.nchar <- max(nchar(
  sprintf(
    paste0(chr.obs.grp, paste0("_%0", lng.num.obs.dgt, "i")), 0)))

## create string of obs for pest control file, make weight 1/value for obs
## mlog_1                 1.658966        6.027852E-02  mlog
chr.obs.blk <- ""
chr.col.spc <- "     "


for(ii in 1:length(chr.obs.grp)) {
  tmp.grp <- chr.obs.grp[ii]
  tmp.data <- eval(as.name(tmp.grp))
  tmp.blk <- ""
  for(jj in 1:length(tmp.data)) {
    tmp.nme <- sprintf(paste0("%-",lng.max.nchar,"s"),sprintf(paste0(tmp.grp, paste0("_%0", lng.num.obs.dgt, "i")), jj))
    tmp.val <- sprintf("%8.4E", tmp.data[jj])
    tmp.wtg <- sprintf("%8.4E", abs(1/tmp.data[jj])) ## initial weight set to inverse of value
    tmp.blk <- c(tmp.blk,
                 paste0(tmp.nme, chr.col.spc, tmp.val, chr.col.spc, tmp.wtg,
                        chr.col.spc, tmp.grp))
    rm(tmp.nme, tmp.val, tmp.wtg)
  }
  tmp.blk <- tmp.blk[-1]
  chr.obs.blk <- c(chr.obs.blk, tmp.blk)
  rm(tmp.grp, tmp.data, tmp.blk)
}
## get rid of first row becuase it is empty
chr.obs.blk <- chr.obs.blk[-1]

# get pest control file
chr.dir.pst <- "m:/models/bacteria/hspf/bigelkhydrocal201601/pest-files"
chr.control <- scan(paste0(chr.dir.pst,"/control.pst"), sep = "\n", 
                    what = "character", quiet = TRUE)
tmp.blk.hd <- grep("\\*", chr.control)
chr.obs.grp.names <- 
  chr.control[(tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                               chr.control[tmp.blk.hd])] + 1):
                (tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                                 chr.control[tmp.blk.hd]) + 1] - 1)]
## copy control file for updating
chr.control.new <- chr.control

## replace the number of obs and number of obs groups
tmp.ln.4 <- strsplit(gsub("( ){1,}", ",",gsub("^( ){1,}","", chr.control.new[4])), split = ",")[[1]]
tmp.ln.4[2] <- as.character(lng.num.obs)
tmp.ln.4[5] <- as.character(lng.num.obs.grp)
chr.control.new[4] <- paste0(
  sprintf(paste0("%", max(nchar(tmp.ln.4)) + 4, "s"), tmp.ln.4), collapse = "")

## insert new block of observations into the control
lng.obs.st <- grep("\\* observation data" ,chr.control.new)
lng.obs.ed <- lng.obs.st + min(grep("\\* " , 
                       chr.control.new[(lng.obs.st + 1):length(chr.control.new)]))
chr.control.new <- c(chr.control.new[1:lng.obs.st], 
                     chr.obs.blk, 
                     chr.control.new[lng.obs.ed:length(chr.control)])

## update obs group block

## write updated control file
write.table(chr.control.new, file = paste0(chr.dir,"/new.pst"), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
