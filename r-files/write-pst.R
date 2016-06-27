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
                      function(chr.name = NULL) length(eval(as.name(chr.name)))))

## create string of obs for pest control file, make weight 1/value for obs
## mlog_1                 1.658966        6.027852E-02  mlog
paste0(sprintf(paste0(chr.obs.grp[1], "_%04i"), 1), "     ",
       sprintf("%8.4E", eval(as.name(chr.obs.grp[1]))))


paste0(sprintf(paste0(chr.obs.grp[5], "_%04i"), 1:length(eval(as.name(chr.obs.grp[5])))), "     ",
       sprintf("%8.4E", eval(as.name(chr.obs.grp[5]))))





# get pest control file
chr.dir.pst <- "m:/models/bacteria/hspf/bigelkhydrocal201601/pest-files"
str.control <- scan(paste0(chr.dir.pst,"/control.pst"), sep = "\n", 
                    what = "character", quiet = TRUE)
tmp.blk.hd <- grep("\\*", str.control)
str.obs.grp.names <- 
  str.control[(tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                               str.control[tmp.blk.hd])] + 1):
                (tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                                 str.control[tmp.blk.hd]) + 1] - 1)]






## combine the dfs to form the block of obs 
lng.og <- grep("\\* observation groups", str.control) + 1
lng.og.e <- lng.og + grep("\\*", str.control[lng.og:length(str.control)])[1] - 2
chr.df.names <- paste0("df.",str.control[lng.og:lng.og.e])
## combine blocks
df.obs.block <- do.call(rbind, mget(chr.df.names))

grep("mbaseind_", str.control, value = TRUE)

grep("mbaseind_", df.obs.block$line, value = TRUE)

## insert new block of observations into the control
lng.obs.st <- grep("\\* observation data" ,str.control)
lng.obs.ed <- lng.obs.st + min(grep("\\* " , 
                       str.control[(lng.obs.st + 1):length(str.control)]))
str.control.new <- c(str.control[1:lng.obs.st], 
                          paste0(df.obs.block[ ,1]),
                          str.control[lng.obs.ed:length(str.control)])

## update the number of observation in control
## get number of obs
lng.n.obs <- length(df.obs.block$line)

## set number of observations
chr.ln <- str.control.new[4]

# substitute the new number of observations
chr.ln.new <- gsub("[0-9]{4,}",as.character(lng.n.obs), chr.ln)

## put new line in control file
str.control.new[4] <- chr.ln.new

## write updated control file
write.table(str.control.new, file = paste0(chr.dir,"/new.pst"), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
