chr.pest.rec.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

chr.org.rec <- scan(file = paste0(chr.pest.rec.dir, 
                      "/org-calib/Final_Deliverables_EPA_July2012/PEST_end/control.rec"),
                    sep = "\n", what = "character")
tmp.top <- 
  grep("See file .*\\.seo for composite observation sensitivities\\.", 
       chr.org.rec) + 1

tmp.bot <- grep("Correlation Coefficient ----->", chr.org.rec) - 1

phi.tot.org <- as.numeric(
  gsub("Sum of squared weighted residuals \\(ie phi\\)                \\=","",
       grep("Sum of squared weighted residuals \\(ie phi\\)                \\=",
            chr.org.rec[tmp.top:tmp.bot], value = TRUE)))

tmp.top <- tmp.top + 
  grep("Sum of squared weighted residuals \\(ie phi\\)                \\=", 
       chr.org.rec[tmp.top:tmp.bot])



