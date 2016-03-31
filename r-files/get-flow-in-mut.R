# get obs flow from Yaquina River gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/get-obs-flow-data.R")

# estimate flow for Big Elk Creek from Yaquina River Gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/estimate-flow.R")

## add hour min to flow data.frame
df.mut <- paste0(
  paste0("      ",
         format(df.flow$date,format="%Y"), " ",
         format(df.flow$date,format="%m"), " ",
         format(df.flow$date,format="%d"), " ",
         "24 ", "00   ",
         formatC(df.flow$flow_cfs,digits=5,width=11,format="E",flag="0")
         ))
                 

header <- c("**** Daily Estimated Flow for Big Elk Creek",
            "**** used in calibration for updated met wdm",
            "      Year Mo Da Hr Mi   Flow_cfs")
fname <- "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/obs_flow.mut"
write(header,fname)
write.table(df.mut,
            fname,append=TRUE,row.name=FALSE,col.names=FALSE,sep="",quote=FALSE)
