## path to rec files
chr.pest.rec.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## check push

## get the obj values for the original calibration
tmp.org.rec <- scan(file = paste0(chr.pest.rec.dir, 
                      "/org-calib/Final_Deliverables_EPA_July2012/PEST_end/control.rec"),
                    sep = "\n", what = "character")
tmp.top <- 
  grep("See file .*\\.seo for composite observation sensitivities\\.", 
       tmp.org.rec) + 1
tmp.bot <- grep("Correlation Coefficient ----->", tmp.org.rec) - 1
tmp.org.rec.phi.blk <- tmp.org.rec[tmp.top:tmp.bot]
tmp.org.rec.phi.blk <- tmp.org.rec.phi.blk[-1 * grep("Objective function ----->", tmp.org.rec.phi.blk)]
df.phi <- data.frame(
  name = c("total", gsub('\".*$',"",
                         gsub('(^.*\ ")', "", tmp.org.rec.phi.blk[-1]))),
  val = as.numeric(gsub("^.*=", "", tmp.org.rec.phi.blk)),
  src = "org")
rm(list=ls(pattern="^tmp\\..*")) ## clean up

## get the obj values for the updated calibration
tmp.upd.rec <- scan(file = paste0(chr.pest.rec.dir, 
                                  "/pest-hspf-files/control.rec"),
                    sep = "\n", what = "character")
tmp.top <- 
  grep("See file .*\\.seo for composite observation sensitivities\\.", 
       tmp.upd.rec) + 1
tmp.bot <- grep("Correlation Coefficient ----->", tmp.upd.rec) - 1
tmp.upd.rec.phi.blk <- tmp.upd.rec[tmp.top:tmp.bot]
tmp.upd.rec.phi.blk <- tmp.upd.rec.phi.blk[-1 * grep("Objective function ----->", tmp.upd.rec.phi.blk)]
df.phi <- rbind(df.phi,
                data.frame(
                  name = c("total", gsub('\".*$',"",
                                         gsub('(^.*\ ")', "", 
                                              tmp.upd.rec.phi.blk[-1]))),
                  val = as.numeric(gsub("^.*=", "", tmp.upd.rec.phi.blk)),
                  src = "upd"))
rm(list=ls(pattern="^tmp\\..*")) ## clean up

## compare org and upd values
df.phi <- rbind(df.phi,
                data.frame(
                  name = df.phi[df.phi$src == "upd", "name"],
                  val = 100 * (df.phi[df.phi$src == "upd", "val"] - 
                                 df.phi[df.phi$src == "org", "val"]) /
                    df.phi[df.phi$src == "org", "val"],
                  src = "comp"))
