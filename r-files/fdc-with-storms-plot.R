## load packages
library(ggplot2, quietly = TRUE)

## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert"

## get names of runs selected
load(paste0(chr.uncert.rerun.dir, "/uncert-fdc-est.RData"))

##drop the uncert runs
df.fdc <- df.fdc[df.fdc$src != "uncert", ]

## plot fdc of USGS eq
## plot mtime and related data
p.mtime00.upd <- ggplot(data = df.fdc,
                        title = "flow duration curves") + 
  scale_y_log10(limits = c(1,10^ceiling(log10(max(df.fdc$y)))))  + 
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)") +
  scale_colour_manual(name = "", breaks = c("eq", "obs", "calib"), 
                      labels = c("USGS Eq", "Obs", "Calibration"), 
                      values = c("black", "blue", "red")) +
  scale_shape_manual(name = "", breaks = c("eq", "obs", "calib"), 
                     labels = c("USGS Eq", "Obs", "Calibration"),
                     values = c(0, 1, 2)) +
  scale_size_manual(name = "", breaks = c("eq", "obs", "calib"), 
                     labels = c("USGS Eq", "Obs", "Calibration"),
                     values = c(5, 5, 5)) +
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)")


## add obs and mod flow data along with reg eq

p.mtime00.upd <- p.mtime00.upd + 
  geom_point(data = df.fdc[df.fdc$par == "est", ],
             aes(x = x * 100, y = y, colour = src,
                 shape = src, size = src))
p.mtime00.upd <- p.mtime00.upd + 
  geom_line(data = df.fdc[df.fdc$par == "est" &
                             df.fdc$src != "uncert", ],
            aes(x = x * 100, y = y, colour = src))


## add error bars from reg eq
p.mtime00.upd <- p.mtime00.upd + 
  geom_errorbar(data = df.fdc[df.fdc$src == "eq", ],
                 aes(x = df.fdc[df.fdc$src == "eq" &
                                 df.fdc$par == "upper", "x"] * 100,
                    ymin = df.fdc[df.fdc$src == "eq" &
                                   df.fdc$par == "lower", "y"],
                    ymax = df.fdc[df.fdc$src == "eq" &
                                   df.fdc$par == "upper", "y"]))

# add storms
p.mtime00.upd <- p.mtime00.upd + 
  geom_point(data = df.fdc[df.fdc$par == "peak" &
                            df.fdc$src == "obs", ],
            aes(x = x * 100, y = y, shape = src, colour = src))
p.mtime00.upd <- p.mtime00.upd + 
  geom_point(data = df.fdc[df.fdc$par == "peak" &
                             df.fdc$src == "calib", ],
             aes(x = x * 100, y = y, shape = src, colour = src))

## plot fdc
png(file = paste0(chr.uncert.rerun.dir, "fdc-all.png"), 
    width = round(1.61803398875 * 480), height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(p.mtime00.upd)
dev.off()
