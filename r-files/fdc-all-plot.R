## load packages
library(ggplot2, quietly = TRUE)


## main path for uncert re-reun
##chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/uncert-upd"
chr.uncert.rerun.dir <- "c:/temp/upd-uncert"

## get fdcs
load(paste0(chr.uncert.rerun.dir, "/uncert-fdc-est.RData"))


10^ceiling(log10(max(df.fdc$y)))


## plot fdc of USGS eq
## plot mtime and related data
p.mtime00.upd <- ggplot(data = df.fdc,
                        title = "flow duration curves") + 
  scale_y_log10(limits = c(1,10^ceiling(log10(max(df.fdc$y)))))  + 
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)") +
  scale_colour_manual(name = "", breaks = c("eq", "obs", "calib", "uncert"), 
                      labels = c("USGS Eq", "Obs", "Calibration", "Uncertainty Runs"), 
                      values = c("black", "blue", "red", "gray")) +
  scale_shape_manual(name = "", breaks = c("eq", "obs", "calib", "uncert"), 
                     labels = c("USGS Eq", "Obs", "Calibration", "Uncertainty Runs"),
                     values = c(0, 1, 2, 20)) +
  scale_size_manual(name = "", breaks = c("eq", "obs", "calib", "uncert"), 
                     labels = c("USGS Eq", "Obs", "Calibration", "Uncertainty Runs"),
                     values = c(5, 5, 5, 3)) +
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)")


## add obs and mod flow data along with reg eq

p.mtime00.upd <- p.mtime00.upd + 
  geom_point(data = df.fdc[df.fdc$par == "est", ],
             aes(x = 100 * x, y = y, colour = src,
                 shape = src, size = src))
p.mtime00.upd <- p.mtime00.upd + 
  geom_line(data = df.fdc[df.fdc$par == "est" &
                             df.fdc$src != "uncert", ],
            aes(x = 100 * x, y = y, colour = src))


## add error bars from reg eq
p.mtime00.upd <- p.mtime00.upd + 
  geom_errorbar(data = df.fdc[df.fdc$src == "eq", ],
                 aes(x = 100 * df.fdc[df.fdc$src == "eq" &
                                 df.fdc$par == "upper", "x"],
                    ymin = df.fdc[df.fdc$src == "eq" &
                                   df.fdc$par == "lower", "y"],
                    ymax = df.fdc[df.fdc$src == "eq" &
                                   df.fdc$par == "upper", "y"]))
## plot fdc
png(file = paste0(chr.uncert.rerun.dir, "/fdc-all.png"), 
    width = 776, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(p.mtime00.upd)
dev.off()
