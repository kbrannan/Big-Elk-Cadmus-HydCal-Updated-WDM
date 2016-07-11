## set paths
chr.dir <- "m:/models/bacteria/hspf/Big-Elk-Cadmus-HydCal-Updated-WDM"
chr.dir.pest.hspf <- paste0(chr.dir, "/pest-hspf-files")

## pest control template file
chr.file.pest.tpl <- "control-tpl.pst"

## new pest control file name
chr.file.pest.new <- "control-new.pst"

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
chr.obs.blk <- ""
chr.col.spc <- "     "


for(ii in 1:length(chr.obs.grp)) {
  tmp.grp <- chr.obs.grp[ii]
  tmp.data <- eval(as.name(tmp.grp))
  tmp.blk <- ""
  ##tmp.wt <- 1 / length(tmp.data) ## weight the group by the number of observations in the group
  ##tmp.wt <- lng.num.obs / length(tmp.data)
  tmp.wt <- 1 ## back to weight 1 / obs value
  for(jj in 1:length(tmp.data)) {
    tmp.nme <- sprintf(paste0("%-",lng.max.nchar,"s"),sprintf(paste0(tmp.grp, paste0("_%0", lng.num.obs.dgt, "i")), jj))
    tmp.val <- sprintf("%8.4E", tmp.data[jj])
    tmp.wtg <- sprintf("%8.4E", tmp.wt * abs(1/tmp.data[jj])) ## initial weight set to inverse of value
    tmp.blk <- c(tmp.blk,
                 paste0(tmp.nme, chr.col.spc, tmp.val, chr.col.spc, tmp.wtg,
                        chr.col.spc, tmp.grp))
    rm(tmp.nme, tmp.val, tmp.wtg)
  }
  tmp.blk <- tmp.blk[-1]
  chr.obs.blk <- c(chr.obs.blk, tmp.blk)
  rm(tmp.grp, tmp.data, tmp.wt, tmp.blk)
}
## get rid of first row becuase it is empty
chr.obs.blk <- chr.obs.blk[-1]

# get pest control file
chr.control <- scan(paste0(chr.dir.pest.hspf, "/", chr.file.pest.tpl), sep = "\n", 
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
tmp.ln.4 <- strsplit(gsub("( ){1,}", ",",gsub("^( ){1,}","", chr.control.new[4])),
                     split = ",")[[1]]
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
                     chr.control.new[lng.obs.ed:length(chr.control.new)])

## update obs group block
## insert new block of observations into the control
lng.obs.grp.st <- grep("\\* observation groups" ,chr.control.new)
lng.obs.grp.ed <- lng.obs.grp.st + min(grep("\\* " , 
                                    chr.control.new[(lng.obs.grp.st + 1):length(chr.control.new)]))
chr.control.new[lng.obs.grp.st:lng.obs.grp.ed]

chr.control.new <- c(chr.control.new[1:lng.obs.grp.st], 
                     chr.obs.grp, 
                     chr.control.new[lng.obs.grp.ed:length(chr.control.new)])
## write updated control file
write.table(chr.control.new, file = paste0(chr.dir.pest.hspf,"/", chr.file.pest.new), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
