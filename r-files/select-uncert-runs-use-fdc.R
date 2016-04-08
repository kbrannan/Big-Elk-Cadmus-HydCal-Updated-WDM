## load packages
#library(ggplot2, quietly = TRUE)
#library(reshape, quietly = TRUE)
#library(gtable, quietly = TRUE)

## load functions
source("M:/Models/Bacteria/LDC/Calculations/Rscripts/LDC Functions.R")

## estimate fdc
## stations at outlet is LASAR 34453
tmp.one.station <- 34453
tmp.ss.est.fn <- paste0("st",tmp.one.station,".xml")
df.fdc.ss.est <- fdc.ss.estimate(ss.fn=tmp.ss.est.fn, ss.path=get.path("StreamStatsBacteria"))

## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert/uncert-rerun"

## get sub-dirs for the uncert runs
chr.sub.dirs <- grep("^uncert", list.dirs(path = chr.uncert.rerun.dir, 
                          full.names = FALSE, recursive = FALSE),
                     value = TRUE)
## create empty vector for names of runs to keep
chr.kept.runs <- c()

## loop through sub-dirs
for(ii in 1:length(chr.sub.dirs)) {
## get the file names for the residual (res) files for the current sub-dir
  chr.res.files <- list.files(
    path = paste0(chr.uncert.rerun.dir, "/", chr.sub.dirs[ii]),
    pattern = "*.\\.res$")
## process files for the current sub-dir
  for(jj in 1:length(chr.res.files)) {
## open connection to the current res file
    con.res <- file(paste0(chr.uncert.rerun.dir, "/", chr.sub.dirs[ii], 
                           "/", chr.res.files[jj]))
## get modeled flow values
    tmp.mflow <- as.numeric(
      substr(tmp.res.file[min(grep("mflow", tmp.res.file)):
                            max(grep("mflow", tmp.res.file))], 46, 62))
## close the file connection
    close(con.res)
## calc quantile (flows) for the %-exceed of USGS equation
    tmp.quant <- quantile(tmp.mflow, 1 - df.fdc.ss.est$FDPercent, names = FALSE)
## chec if current fdc flows are within the USGS error bars 
    if(sum(df.fdc.ss.est$lower <= tmp.quant & 
           tmp.quant <= df.fdc.ss.est$upper) == 
       length(tmp.quant)) {
## if yes keep file name append to list of files
      chr.kept.runs <- c(chr.kept.runs, 
                         paste0(chr.uncert.rerun.dir, "/", 
                                chr.sub.dirs[ii], "/", chr.res.files[jj]))
      
    }
    rm(list=ls(pattern="^tmp//.*")) ## clean up
  }
}





## create three difference data.frames for plots
df.mtime.obs.org <- data.frame(
  probs = as.numeric(df.res.org[df.res.org$Group == "mtime", "Measured" ]),
  value = num.fdc.flows)
df.mtime.mod.org <- data.frame(
  probs = as.numeric(df.res.org[df.res.org$Group == "mtime", "Modelled" ]),
  value = num.fdc.flows)
df.mtime.eq <- fdc.ss.st34453

df.mtime.obs.org$probs <- 100 * df.mtime.obs.org$probs
df.mtime.mod.org$probs <- 100 * df.mtime.mod.org$probs
df.mtime.eq$FDPercent <- 100 * df.mtime.eq$FDPercent

names(df.mtime.obs.org) <- c("x", "y")
names(df.mtime.mod.org) <- c("x", "y")
names(df.mtime.eq)  <- c("x", "y", "ymin", "ymax")

df.mtime.obs.org <- df.mtime.obs.org[order(df.mtime.obs.org$x), ]
df.mtime.mod.org <- df.mtime.mod.org[order(df.mtime.mod.org$x), ]

df.mtime.all.org <- melt(list(obs = df.mtime.obs.org, 
                          mod = df.mtime.mod.org, 
                          eq = df.mtime.eq),
                     id.vars = "x")

df.mtime.all.org$L1 <- factor(df.mtime.all.org$L1, levels = c("obs", "mod", "eq"))

## plot mtime and related data
p.mtime00.org <- ggplot(data = df.mtime.all.org,
                    title = "flow duration curves") + 
  scale_y_log10() + 
  scale_colour_manual(name = "", breaks = c("obs", "mod", "eq"), 
                      labels = c("Obs", "Model", "USGS Eq"), 
                      values = c("blue", "green", "black")) +
  scale_shape_manual(name = "", breaks = c("obs", "mod", "eq"), 
                     labels = c("Obs", "Model", "USGS Eq"),
                     values = c(21, 17, 15)) +
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)")

## add obs and mod flow data along with reg eq
p.mtime00.org <- p.mtime00.org + 
  geom_line(data = df.mtime.all.org[as.character(df.mtime.all.org$variable) == "y", ],
            aes(x = x, y = value, colour = L1))

p.mtime00.org <- p.mtime00.org + 
  geom_point(data = df.mtime.all.org[as.character(df.mtime.all.org$variable) == "y", ],
             aes(x = x, y = value, colour = L1, shape = L1), size = 4)

## add error bars from reg eq
p.mtime00.org <- p.mtime00.org + 
  geom_errorbar(data = df.mtime.all.org[df.mtime.all.org$L1 == "eq", ], 
                aes(x = df.mtime.all.org[df.mtime.all.org$L1 == "eq" & 
                                       as.character(df.mtime.all.org$variable) == "ymin", 'x'],
                    ymin = df.mtime.all.org[df.mtime.all.org$L1 == "eq" & 
                                          as.character(df.mtime.all.org$variable) == "ymin", 'value'],
                    ymax = df.mtime.all.org[df.mtime.all.org$L1 == "eq" & 
                                          as.character(df.mtime.all.org$variable) == "ymax", "value"]
                ))
## plot fdc
png(file = paste0(chr.bacteria.twg.17.dir, "/tables-charts-figures/fdc-org.png"), 
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(p.mtime00.org)
dev.off()


