## load packages
library(ggplot2, quietly = TRUE)
library(reshape2, quietly = TRUE)

## load USGS fdc function
source("//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/LDC/Calculations/Rscripts/LDC Functions.R")

## paths 
chr.dir.main <- "M:/models/bacteria/hspf/Big-Elk-Cadmus-HydCal-Updated-WDM"
chr.dir.pest.hspf <- paste0(chr.dir.main, "/pest-hspf-files")
chr.dir.r.files <- paste0(chr.dir.main, "/r-files")
## files
chr.file.res <- "control-new.res"
chr.file.proc.model <- "proc-mod-flow-for-pest.R"

## estimate fdc
## stations at outlet is LASAR 34453
tmp.one.station <- 34453
tmp.ss.est.fn <- paste0("st",tmp.one.station,".xml")
tmp.fdc.ss.est <- fdc.ss.estimate(ss.fn=tmp.ss.est.fn, ss.path=get.path("StreamStatsBacteria"))
tmp.fdc.ss.name <- paste0("fdc.ss.st",tmp.one.station)
eval(parse(text=paste0(tmp.fdc.ss.name," <- tmp.fdc.ss.est")))
rm(list=ls(pattern="^tmp\\.*")) ## clean up


## read res file
chr.raw.res <- scan(file = paste0(chr.dir.pest.hspf, "/", chr.file.res),
                    what = "character", sep = "\n")
junk <-  data.frame(
  do.call(rbind,strsplit(gsub("( ){1,}", ",", chr.raw.res), split = ",")),
  stringsAsFactors = FALSE)[, -1]

names(junk) <- junk[1, ]
junk <- junk[-1, ]
head(junk)
junk$Group <- as.factor(junk$Group)

junk$Measured <- as.numeric(junk$Measured)
junk$Modelled <- as.numeric(junk$Modelled)



## get probabilities for obs and mod mtime
chr.proc <- scan(file = paste0(chr.dir.r.files, "/", chr.file.proc.model),
                 what = "character", sep = "\n")
eval(parse(text = grep("tmp.per <\\-", chr.proc, value = TRUE)))
tmp.per <- 1 - tmp.per


## get the fdc values for the storm peaks
ecdf.obs <- ecdf(junk$Measured[junk$Group == "mflow"])
ecdf.mod <- ecdf(junk$Modelled[junk$Group == "mflow"])
df.storm.obs.upd <- data.frame(
  probs = 1 - ecdf.obs(junk$Measured[junk$Group == "mpeak"]),
  value = junk$Measured[junk$Group == "mpeak"])
df.storm.mod.upd <- data.frame(
  probs = 1 - ecdf.mod(junk$Modelled[junk$Group == "mpeak"]),
  value = junk$Modelled[junk$Group == "mpeak"])



## create three difference data.frames for plots
df.mtime.obs.upd <- data.frame(
  probs = tmp.per,
  value = junk[junk$Group == "mtime", "Measured"])
df.mtime.mod.upd <- data.frame(
  probs = tmp.per,
  value = junk[junk$Group == "mtime", "Modelled"])
df.mtime.eq <- fdc.ss.st34453

df.mtime.obs.upd$probs <- 100 * df.mtime.obs.upd$probs
df.mtime.mod.upd$probs <- 100 * df.mtime.mod.upd$probs
df.mtime.eq$FDPercent <- 100 * df.mtime.eq$FDPercent

names(df.mtime.obs.upd) <- c("x", "y")
names(df.mtime.mod.upd) <- c("x", "y")
names(df.storm.obs.upd) <- c("x", "y")
names(df.storm.mod.upd) <- c("x", "y")
names(df.mtime.eq)  <- c("x", "y", "ymin", "ymax")

df.mtime.obs.upd <- df.mtime.obs.upd[order(df.mtime.obs.upd$x), ]
df.mtime.mod.upd <- df.mtime.mod.upd[order(df.mtime.mod.upd$x), ]
df.storm.obs.upd <- df.storm.obs.upd[order(df.storm.obs.upd$x), ]
df.storm.mod.upd <- df.storm.mod.upd[order(df.storm.mod.upd$x), ]

df.mtime.all.upd <- melt(list(obs = df.mtime.obs.upd, 
                          mod = df.mtime.mod.upd, 
                          eq = df.mtime.eq),
                     id.vars = "x")

df.mtime.all.upd$L1 <- factor(df.mtime.all.upd$L1, levels = c("obs", "mod", "eq"))

## plot mtime and related data
p.mtime00.upd <- ggplot(data = df.mtime.all.upd,
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
p.mtime00.upd <- p.mtime00.upd + 
  geom_line(data = df.mtime.all.upd[as.character(df.mtime.all.upd$variable) == "y", ],
            aes(x = x, y = value, colour = L1))

p.mtime00.upd <- p.mtime00.upd + 
  geom_point(data = df.mtime.all.upd[as.character(df.mtime.all.upd$variable) == "y", ],
             aes(x = x, y = value, colour = L1, shape = L1), size = 4)

## add error bars from reg eq
p.mtime00.upd <- p.mtime00.upd + 
  geom_errorbar(data = df.mtime.all.upd[df.mtime.all.upd$L1 == "eq", ], 
                aes(x = df.mtime.all.upd[df.mtime.all.upd$L1 == "eq" & 
                                       as.character(df.mtime.all.upd$variable) == "ymin", 'x'],
                    ymin = df.mtime.all.upd[df.mtime.all.upd$L1 == "eq" & 
                                          as.character(df.mtime.all.upd$variable) == "ymin", 'value'],
                    ymax = df.mtime.all.upd[df.mtime.all.upd$L1 == "eq" & 
                                          as.character(df.mtime.all.upd$variable) == "ymax", "value"]
                ))
## plot fdc
png(file = paste0(chr.bacteria.twg.17.dir, "/tables-charts-figures/fdc-upd.png"), 
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(p.mtime00.upd)
dev.off()


