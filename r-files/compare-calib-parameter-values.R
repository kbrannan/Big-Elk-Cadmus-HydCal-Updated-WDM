## path to rec files
chr.pest.rec.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## get the calibrated parameter values for the original calibration
tmp.org.par <- scan(file = paste0(chr.pest.rec.dir, 
                                  "/org-calib/Final_Deliverables_EPA_July2012/PEST_end/calib.par"),
                    sep = "\n", what = "character")[-1]

tmp.df.org.par <- gsub('\"', "",do.call(rbind,strsplit(tmp.org.par, " {1,}"))[, c(-1, -4, -5)])

df.par <- data.frame(name = tmp.df.org.par[, 1], 
                     val = as.numeric(tmp.df.org.par[, 2]),
                     src = "org")
rm(list=ls(pattern="^tmp\\..*"))  ## clean up

## get the calibrated parameter values for the updated calibration
tmp.upd.par <- scan(file = paste0(chr.pest.rec.dir, 
                                  "/pest-hspf-files/upd-calib/controlwa.par"),
                    sep = "\n", what = "character")[-1]

tmp.df.upd.par <- gsub('\"', "",do.call(rbind,strsplit(tmp.upd.par, " {1,}"))[, c(-1, -4, -5)])

df.par <- rbind(df.par, 
                data.frame(name = tmp.df.upd.par[, 1], 
                           val = as.numeric(tmp.df.upd.par[, 2]),
                           src = "upd"))
rm(list=ls(pattern="^tmp\\..*"))  ## clean up

## compare org and upd values
df.par <- rbind(df.par,
                data.frame(
                  name = df.par[df.par$src == "upd", "name"],
                  val = 100 * (df.par[df.par$src == "upd", "val"] - 
                                 df.par[df.par$src == "org", "val"]) /
                    df.par[df.par$src == "org", "val"],
                  src = "comp"))

df.par[df.par$src == "comp",]

library(ggplot2)

p.pars <- ggplot(data = df.par[df.par$src == "comp", ]) + geom_bar(aes(x=name, y = val ),
                                                                   stat = "identity")
plot(p.pars)
