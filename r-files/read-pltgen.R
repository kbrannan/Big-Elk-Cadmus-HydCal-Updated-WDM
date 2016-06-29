
read.pltgen <- function(chr.file) {
  ## funtion that reads PLTGEN output file from HSPF simulations
  ## created 2015-12-24 by Kevin Brannan
  ## chr.file is the path and name of the pltgen file to read
  

  ## read in the PLTGEN file
  chr.pltgen <- scan(file = chr.file, sep = "\n", 
                     what = "character", quiet = TRUE)
  
  ## get first line of data. the "-1.0000000E+30" is a flag for no data and 
  ## should only occur on the first day for a daily tiime step aggregation of
  ## hourly data. take min just in case
  lng.str <- min(grep("-1.0000000E\\+30$", chr.pltgen)) + 1
  
  
  ## get data only from orginal PLTGEN file
  str.data <- chr.pltgen[lng.str:length(chr.pltgen)]

  ## get data in data frame
  ## fisrt get data as characters
  df.data.chr <- data.frame(do.call(rbind,
                                    strsplit(x = gsub("( ){1,}24( ){1,}0", " ",
                                                      gsub("^( ){1,}Mo( ){1,}", "",
                                                           str.data))
                                             , split = "( ){1,}")),
                            stringsAsFactors = FALSE)

  ## rename variables in the data frame to names in PLTGEN file
  names(df.data.chr) <- c("year", "month", "day", "rovol")
  
  ## create data frame with date and numeric values
  tmp.flows <- as.numeric(df.data.chr[, "rovol"])
  tmp.date <- as.POSIXct(apply(df.data.chr[, 1:3], 
                               1, function(x) paste0(x, collapse = "-")))
  
  ## create data.frame and return
  df.data <- data.frame(date = tmp.date, rovol = tmp.flows)
  return(df.data)

}


