# gather the raw observed data in one place for use in the construction of the
# PEST control file. Some of the raw data will be processed in other R-projects
# but I will keep a copy of the raw data used in this repo to increase 
# transparency
# created Kevin Brannan 2015-12-08

## set path
chr.dir <- "m:/models/bacteria/hspf/HydroCal201506/R_Projs/construct_control"


# simulation period determined from updated meteo data. The files are in a 
# zipfile. The HSPF UCI file for the extended simulation period is extracted 
# from the zipfile and the begin and end dates of the simulation are taken from
# this UCI file
# zipfile path and name
tmp.dir <- "M:/Models/Bacteria/HSPF/TetraTech20150211 WDM file extension/"
tmp.zip <- "HSPF&WDMfiles.zip"

# get list of files in the zipfile
tmp.fns <- unzip(zipfile = paste0(tmp.dir,tmp.zip), list = TRUE)

# read the uci file as a character vector
str.uci <- scan(unz(paste0(tmp.dir,tmp.zip),
                    grep("*.extend.*\\.uci$", tmp.fns[ , 1], value=TRUE)
                    )
                , sep = "\n", what = character()
                )

# get the line in the UCI that has the begin and end dates of the simulation
tmp.str <- grep("START", str.uci, value=TRUE)

# get begin date
dt.sim.bg <- as.POSIXct(
  gsub("/","-",
       gsub("((\\s{1,})|([aA-zZ]))", "",
            gsub("( ){,1}00:00.*END.*$","",tmp.str))))

# get end date
dt.sim.ed <- as.POSIXct(
  gsub("/","-",
       gsub("((\\s{1,})|(24:00))","",
            gsub(".*END","",tmp.str))))

# clean up
rm(list=ls(pattern = "^tmp\\..*"))


# flow data
# CANNOT get the ssh authentification to work
# use scripts from the Select_Storm_HydCal repo to get observed flow and estimate the
# flow for Big Elk Creek
# library(devtools)
# library(RCurl)
# 
# x <- scp(host = "www.github.com", 
#          path = "Select_Storm_HydCal/devel/get-obs-flow-data.R", 
#          user = "kbrannan",
#          key = "~/.ssh/id_rsa_Git_Bash"
#            )
# 
# x <- getURL("https://raw.github.com/kbrannan/Select_Storm_HydCal/master/devel/get-obs-flow-data.R")
# y <- read.csv(text = x)
# use the script from Select_Storm_HydCal on local server to get flow data

# get obs flow from Yaquina River gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/get-obs-flow-data.R")

# estimate flow for Big Elk Creek from Yaquina River Gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/estimate-flow.R")

# remove Yaquina data
rm(df.flow.obs)

# get flow data within the simulation period and simplify data.frame
df.flow <- df.flow.est[ df.flow.est$date >= dt.sim.bg &
                          df.flow.est$date <= dt.sim.ed, 
                        c("date", "mean_daily_flow_cfs")]
names(df.flow) <- c("date", "flow_cfs")

# remove Big Elk Creek original data.frame
rm(df.flow.est)

# get PEST control file

# zipfile path and name
tmp.dir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/Hydro Calibration/Files from Cadmus"
tmp.zip <- "Final_Deliverables_EPA_July2012.zip"

# get list of files in the zipfile
tmp.fns <- unzip(zipfile = paste0(tmp.dir,"/", tmp.zip), list = TRUE)

# see how many occurances there are of the control file in the zipfile
grep("control.pst", tmp.fns$Name, value = TRUE)

# only one, I'll use it
tmp.file <- "Final_Deliverables_EPA_July2012/PEST_end/control.pst"

# read the PEST control file as a character vector
str.control <- scan(unz(paste0(tmp.dir,"/", tmp.zip),
                        tmp.file
                        )
, sep = "\n", what = character()
)

# get observational groups names

# get rows where the block names are
tmp.blk.hd <- grep("\\*", str.control)
str.obs.grp.names <- 
  str.control[(tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                               str.control[tmp.blk.hd])] + 1):
              (tmp.blk.hd[grep("[Oo]bs.*[Gg]roups", 
                              str.control[tmp.blk.hd]) + 1] - 1)]

# clean up
rm(list = ls(pattern = "^tmp\\..*"))

# create vector strings of output for each observation group

# mlog - log10 of daily flow in cfs. addedd 0.0001 cfs in case there are 0 flows

# get flow data and apply log10 transform
tmp.data <- log10(df.flow$flow_cfs + 0.0001)

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[1]

# get first occrence of observation in group to see format
grep(paste0(tmp.grp,".*"), str.control, value = TRUE)[2]

# format of line
# "mlog_1                 1.797268        5.564000E-02  mlog"

# create sequence for number of obs
tmp.num <- 1:length(tmp.data)

# write lines of obs data for mlog to a data.frame
df.mlog <- data.frame(line = 
                         paste0(tmp.grp, "_", 
                                sprintf(fmt = paste0("%", 
                                                     paste0("0",
                                                            nchar(length(tmp.data))),"i"), 
                                        tmp.num[tmp.num]),
                                "               ",
                                sprintf(fmt = "%1.5E", tmp.data[tmp.num]),
                                "     1.000000E+00  ", tmp.grp),
                      stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))

# mflow - daily flow in cfs

# get flow data
tmp.data <- df.flow$flow_cfs

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[2]

# get first occrence of observation in group to see format
grep(paste0(tmp.grp,".*"), str.control, value = TRUE)[2]

# format of line
# "mflow_1                62.70000         0.00000      mflow"

# create sequence for number of obs
tmp.num <- 1:length(tmp.data)

# write lines of obs data for mlog to a data.frame
df.mflow <- data.frame(line = 
                        paste0(tmp.grp, "_", 
                               sprintf(fmt = paste0("%", 
                                                    paste0("0",
                                                           nchar(length(tmp.data))),"i"), 
                                       tmp.num[tmp.num]),
                               "              ",
                               sprintf(fmt = "%1.5E", tmp.data[tmp.num]),
                               "     1.000000E+00  ", tmp.grp),
                      stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))

# mbaseind - baseflow index
# output from the hysep analysis in "select-storms-hysep.R" which is in the 
# Select_Storm_HydCal repo is used to calculate the baseflow index

# get hysep results
load(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/hysep88_8.RData")

# get flow and base flow data
tmp.data <- df.hysep88.8[ , c("Dates", "BaseQ", "Flow")]
names(tmp.data) <- c("date", "baseflow", "flow")

tmp.data$date <- as.POSIXct(format(tmp.data$date, "%Y-%m-%d"))

# make sure data is witin simulation begin and end dates
tmp.data <- tmp.data[tmp.data$date >= dt.sim.bg &
                     tmp.data$date <= dt.sim.ed, ]

min(tmp.data$date) == dt.sim.bg
max(tmp.data$date) == dt.sim.ed

# remove origincal hysep results
rm(df.hysep88.8)

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[3]

# calculate baseflow index
tmp.bfi <- round(sum(tmp.data$baseflow) / sum(tmp.data$flow), 4)


# reset tmp.data to the baseflow index data to be consitent with earlier names
# in commands to write lines
tmp.data <- tmp.bfi

# get first occrence of observation in group to see format
grep(paste0(tmp.grp,".*"), str.control, value = TRUE)[2]

# format of line
# "mbaseind_1            0.4400000         1.00000      mbaseind"

# create sequence for number of obs
tmp.num <- 1:length(tmp.data)

# write lines of obs data for mlog to a data.frame
df.mbaseind <- data.frame(line = 
                         paste0(tmp.grp, "_", 
                                sprintf(fmt = paste0("%", 
                                                     paste0("0",
                                                            nchar(length(tmp.data))),"i"), 
                                        tmp.num[tmp.num]),
                                "              ",
                                sprintf(fmt = "%1.5E", tmp.data[tmp.num]),
                                "     1.000000E+00  ", tmp.grp),
                       stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))


# mpeak and mvol_stm - flow peaks and volumes for the storms. this was done in 
# the "Select_Storm_HydCal" repo. I will read in the file "strm_peaks_vols.dat"
# that is the output from the storm selection

# get storm data
tmp.data <- scan(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/strm_peaks_vols.dat",
                 what = "character",
                 sep = "\n")

# get rid of a leading space
tmp.data <- gsub("^ {1}","",tmp.data)

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[4]

# get the mpeak lines and put in a data.frame
df.mpeak <- data.frame(line = grep(paste0(".*", tmp.grp, "$"), tmp.data,
                                   value = TRUE),
                       stringsAsFactors = FALSE
)

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[8]

# get the mvol_stm lines and put in a data.frame
df.mvol_stm <- data.frame(line = grep(paste0(".*", tmp.grp, "$"), tmp.data,
                                      value = TRUE),
                       stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))

# convert flow from cfs to ac-ft/day, 1 day = 86400 sec and 
# 1 ac-ft = 43559.9 cu ft
df.flow <- cbind(df.flow, 
                 flow_acft = df.flow$flow_cfs * 86400 * (1 / 43559.9))

# for flow volumes I will use factors to get sums. Using doBy package to dothis
library(doBy)

# mvol_ann - annual flow volume in ac-ft

# get flow data and add factors for year and month
df.flow.exp <- cbind(df.flow, year=strftime(df.flow$date, format = "%Y"),
                  month = strftime(df.flow$date, format = "%b"))

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[5]

# get first occrence of observation in group to see format
grep(paste0(tmp.grp,".*"), str.control, value = TRUE)[2]

# format of line
# "mvol_ann_1            1.9271139E+10    1.000000E-02  mvol_ann"

# get sum for group
tmp.data <- summaryBy(flow_acft ~ year, data = df.flow.exp, FUN = sum)

# create sequence for number of obs
tmp.num <- 1:length(tmp.data[ , 2])

# write lines of obs data for mvol_ann to a data.frame
df.mvol_ann <- data.frame(line = 
                         paste0(tmp.grp, "_", 
                                sprintf(fmt = paste0("%", 
                                                     paste0("0",
                                                            nchar(length(tmp.data[, 2]))),"i"), 
                                        tmp.num[tmp.num]),
                                "             ",
                                sprintf(fmt = "%1.5E", tmp.data[, 2]),
                                "     1.000000E+00  ", tmp.grp),
                       stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))

# mvol_smr - summer (Jun - Aug) flow volume in ac-ft by year

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[6]

# get first occrence of observation in group to see format
grep(paste0(tmp.grp,".*"), str.control, value = TRUE)[2]

# format of line
# "mvol_smr_1            4.2016752E+08    1.000000E-02  mvol_smr"

# get sums for months
tmp.month.year <- summaryBy(flow_acft ~ month + year, data = df.flow.exp,
                            FUN = sum)

# get summary for season
tmp.data <- summaryBy(flow_acft.sum ~ year,
                      data = tmp.month.year[as.character(tmp.month.year$month) 
                                            %in% c("Jun", "Jul", "Aug"), ],
                      FUN = sum)

# create sequence for number of obs
tmp.num <- 1:length(tmp.data[ , 2])

# write lines of obs data for mvol_smr to a data.frame
df.mvol_smr <- data.frame(line = 
                            paste0(tmp.grp, "_", 
                                   sprintf(fmt = paste0("%", 
                                                        paste0("0",
                                                               nchar(length(tmp.data[, 2]))),"i"), 
                                           tmp.num[tmp.num]),
                                   "             ",
                                   sprintf(fmt = "%1.5E", tmp.data[, 2]),
                                   "     1.000000E+00  ", tmp.grp),
                          stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))


# mvol_wtr - winter (Dec - Feb) flow volume in ac-ft by year

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[7]

# get first occrence of observation in group to see format
grep(paste0(tmp.grp,".*"), str.control, value = TRUE)[2]

# format of line
# "mvol_wtr_1            9.6252621E+09    1.000000E-02  mvol_wtr"

# get sums for months
tmp.month.year <- summaryBy(flow_acft ~ month + year, data = df.flow.exp,
                            FUN = sum)

# get summary for season
tmp.data <- summaryBy(flow_acft.sum ~ year,
                      data = tmp.month.year[as.character(tmp.month.year$month) 
                                            %in% c("Dec", "Jan", "Feb"), ],
                      FUN = sum)

# create sequence for number of obs
tmp.num <- 1:length(tmp.data[ , 2])

# write lines of obs data for mvol_wtr to a data.frame
df.mvol_wtr <- data.frame(line = 
                            paste0(tmp.grp, "_", 
                                   sprintf(fmt = paste0("%", 
                                                        paste0("0",
                                                               nchar(length(tmp.data[, 2]))),"i"), 
                                           tmp.num[tmp.num]),
                                   "             ",
                                   sprintf(fmt = "%1.5E", tmp.data[, 2]),
                                   "     1.000000E+00  ", tmp.grp),
                          stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))

# mtime - % exceedance for flow, using 1%, 5%, 10%, 25%, 50%, 75%, 95%, 99%
# this is different than what Cadmus using in tsproc which is the fraction
# of time the flow is above some value. I am not going to use tsproc when
# doinmg the calculations. I will use R script

# get the prefix for the observation in the group, which is the group name
tmp.grp <- str.obs.grp.names[9]

# percents used
tmp.per <- c(0.0001, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)

tmp.data <- quantile(x = df.flow$flow_cfs, probs = tmp.per)

# create sequence for number of obs
tmp.num <- 1:length(tmp.data)

# write lines of obs data for mtime to a data.frame
df.mtime <- data.frame(line = 
                            paste0(tmp.grp, "_", 
                                   sprintf(fmt = paste0("%", 
                                                        paste0("0",
                                                               nchar(length(tmp.data))),"i"), 
                                           tmp.num[tmp.num]),
                                   "                 ",
                                   sprintf(fmt = "%1.5E", tmp.data),
                                   "     1.000000E+00  ", tmp.grp),
                          stringsAsFactors = FALSE
)

# clean up
rm(list=ls(pattern = "^tmp\\..*"))

# save data.frames for observations in PEST control
save(list = c("df.mlog", "df.mflow", "df.mpeak", "df.mbaseind", "df.mvol_ann",
              "df.mvol_smr", "df.mvol_wtr", "df.mvol_stm", "df.mtime"), 
     file = paste0(chr.dir, "/obs-blocks.RData"))

# save HSPF-UCI and PEST-control files
save(list = c("str.uci", "str.control"), 
     file = paste0(chr.dir, "/uci-control.RData"))

