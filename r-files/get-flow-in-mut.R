# get obs flow from Yaquina River gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/get-obs-flow-data.R")

# estimate flow for Big Elk Creek from Yaquina River Gage
source(file = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/estimate-flow.R")

header <- c("**** Daily Estimated Flow for Big Elk Creek",
            "**** used in calibration for updated met wdm",
            "      Year Mo Da Hr Mi   FC")
fname <- "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf/files/obs_flow.mut"
write(header,fname)
write.table(df.flow[ , c("date", "flow_cfs")],
            fname,append=TRUE,row.name=FALSE,col.names=FALSE,sep="",quote=FALSE)
