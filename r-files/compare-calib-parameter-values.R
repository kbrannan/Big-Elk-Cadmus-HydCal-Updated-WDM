## path to rec files
chr.pest.rec.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## get the calibrated parameter values for the original calibration
tmp.org.par <- scan(file = paste0(chr.pest.rec.dir, 
                                  "/org-calib/Final_Deliverables_EPA_July2012/PEST_end/calib.par"),
                    sep = "\n", what = "character")[-1]

tmp.df.org.par <- gsub('\"', "",do.call(rbind,strsplit(tmp.org.par, " {1,}"))[, c(-1, -4, -5)])

chr.levels <- tmp.df.org.par[, 1]

df.par <- data.frame(name = tmp.df.org.par[, 1], 
                     val = as.numeric(tmp.df.org.par[, 2]),
                     src = "org", stringsAsFactors = FALSE)
rm(list=ls(pattern="^tmp\\..*"))  ## clean up



## get the calibrated parameter values for the updated calibration
tmp.upd.par <- scan(file = paste0(chr.pest.rec.dir, 
                                  "/pest-hspf-files/control-new.par"),
                    sep = "\n", what = "character")[-1]

tmp.df.upd.par <- gsub('\"', "",do.call(rbind,strsplit(tmp.upd.par, " {1,}"))[, c(-1, -4, -5)])

df.par <- rbind(df.par, 
                data.frame(name = tmp.df.upd.par[, 1], 
                           val = as.numeric(tmp.df.upd.par[, 2]),
                           src = "upd", stringsAsFactors = FALSE))
rm(list=ls(pattern="^tmp\\..*"))  ## clean up

## dropping retsc from df.par. Not used in orginial calibration
df.par <- df.par[-1 * grep("retsc", as.character(df.par$name)), ]


## need to convert agwrctr000 to agwrc for updated calibration
tmp.row <- grep("agwrctr000", as.character(df.par$name))
df.par$val[tmp.row] <- df.par$val[tmp.row] / (1 + df.par$val[tmp.row])
df.par$name[tmp.row] <- "agwrc000"
rm(tmp.row)

## compare org and upd values
df.par <- rbind(df.par,
                data.frame(
                  name = df.par[df.par$src == "upd", "name"],
                  val = 100 * (df.par[df.par$src == "upd", "val"] - 
                                 df.par[df.par$src == "org", "val"]) /
                    df.par[df.par$src == "org", "val"],
                  src = "comp"))

df.par[df.par$src == "comp",]

junk <- df.par
junk$name <- factor(junk$name, levels = unique(df.par$name))
junk$src <- factor(junk$src, levels = c("org", "upd", "comp"))
tmp.factor <- factor(unique(df.par$name), levels = unique(df.par$name))
tmp.row <- grep("agwrctr000", tmp.levels)
tmp.levels[-1 * tmp.row]

tmp.max.min <- max(abs(junk$val[as.character(junk$src) == "comp"]))
tmp.max <- log10(tmp.max.min)
y.max <- 10^(floor(tmp.max) + ceiling((tmp.max - floor(tmp.max)) * 10)/10)

library(ggplot2)

p.pars <- ggplot(data = junk[junk$src == "comp", ]) + 
  geom_bar(aes(x=name, y = val ), stat = "identity") +
  xlab("Paramater Name") + ylab("Percent change from Orginial to Updated") + 
  ylim(-1 * y.max, y.max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(p.pars)
