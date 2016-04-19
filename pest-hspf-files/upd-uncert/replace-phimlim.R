chr.uncert.dir <- "c:/temp/upd-uncert"


chr.addreg <- list.files(path = paste0(chr.uncert.dir, "/addreg-files"), 
                                       pattern = "^add.*\\.pst")

for(ii in 1:length(chr.addreg)) {
  addrg <- file(paste0(chr.uncert.dir, "/addreg-files/", chr.addreg[ii]))
  chr.tmp <- readLines(addrg)
  close(addrg)
  chr.tmp[13840] <- "7.0000000E+03  1.0500000E-10  0.1000000"
  pst <- file(paste0(chr.uncert.dir, "/uncert-pst/", "uncert", ii, ".pst"), "w")
  writeLines(chr.tmp, con = pst)
  close(pst)
  print(paste0(" uncert", ii, ".pst compltetd"))
  rm(chr.tmp)
}
