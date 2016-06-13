chr.sen.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-calib"
chr.sen.file <- "calib.sen"

chr.title <- "Parmeter Sensitivity for Updated Calibration"

## get parameter sensitvit file
chr.sen.raw <- scan(file = paste0(chr.sen.dir, "/", chr.sen.file), 
                    what = "chatacter", sep = "\n" )

## get par upper and lower bounds
chr.pst.raw <- scan(file = paste0(chr.sen.dir, "/", gsub("sen$","pst",chr.sen.file)), 
                    what = "chatacter", sep = "\n" )
df.par.rng <- do.call(rbind,
        strsplit(gsub(" {1,}",",", chr.pst.raw[
  (grep("\\*", chr.pst.raw)[grep("\\* parameter data", grep("\\*", chr.pst.raw, value = TRUE))] + 1):
    (grep("\\*", chr.pst.raw)[grep("\\* parameter data", grep("\\*", chr.pst.raw, value = TRUE)) + 1] - 1)]),
  split = ","))[, c(1,5,6)]
df.par.rng <- data.frame(df.par.rng, stringsAsFactors = FALSE)
names(df.par.rng) <- c("par", "lbnd", "ubnd")
df.par.rng$lbnd <- as.numeric(df.par.rng$lbnd)
df.par.rng$ubnd <- as.numeric(df.par.rng$ubnd)
str(df.par.rng)
df.par.rng <- cbind(df.par.rng, rel = log(df.par.rng$ubnd) - log(df.par.rng$lbnd))


## get rows of composite sensitivity table heads
num.comp.sen.rows <- grep("Composite sensitivities", chr.sen.raw)

## output data.frame
df.sens <- data.frame()

for(ii in 1:length(num.comp.sen.rows)) {
  ## get names of compsite tables (same as obs groups)
  tmp.group.cur <- gsub("(^.* \")|(\" .*-.*$)","",
                        chr.sen.raw[num.comp.sen.rows][ii])

  ## get current table data
  if(ii < length(num.comp.sen.rows)) tmp.cur <- chr.sen.raw[num.comp.sen.rows[ii]:(num.comp.sen.rows[ii + 1] - 1)]
  if(ii == length(num.comp.sen.rows)) tmp.cur <- chr.sen.raw[num.comp.sen.rows[ii]:length(chr.sen.raw)]
  if(length(grep("No observations", tmp.cur)) == 0) {
    tmp.cur <- tmp.cur[(grep("Parameter", tmp.cur) + 1):length(tmp.cur)]
    tmp.df.cur <- as.data.frame(
      do.call(
        rbind,strsplit(
          gsub(" {1,}",",", gsub("(^ {1,})|( {1,}$)", "",tmp.cur)), split = ",")),
      stringsAsFactors = FALSE)
    names(tmp.df.cur) <- c("par", "group", "val", "sens")
    tmp.df.cur[, 3] <- as.numeric(tmp.df.cur[, 3])
    tmp.df.cur[, 4] <- as.numeric(tmp.df.cur[, 4])
    ## calculate relative sensitivity as the "The relative composite sensitivity 
    ## of a log-transformed parameter is determined by multiplying the composite
    ## sensitivity of that parameter by the absolute log of the value of 
    ## that parameter." page 5-17 PEST Manual (absolute page 146 in pdf file)
    ## tmp.df.cur <- data.frame(tmp.df.cur, rel.sens = abs(log(tmp.df.cur[,3])) * tmp.df.cur[, 4])
    tmp.df.cur <- data.frame(tmp.df.cur, rel.sens = tmp.df.cur[, 4]/ df.par.rng$rel) 
    ## create rank column  
    tmp.df.cur <- data.frame(tmp.df.cur, obs.group.rank = -1)
    ## rank relative sensitivity for obs group
    tmp.df.cur[order(tmp.df.cur[, 5], decreasing = TRUE), 6] <- 
      1:length(tmp.df.cur[, 1])
    ## add name of obs group  
    tmp.df.cur <- data.frame(obs.group = tmp.group.cur,
                             tmp.df.cur, stringsAsFactors = FALSE)
    ## append sensitivity data.frame in long form
    ##if(length(df.sens) == 0) df.sens <- tmp.df.cur
    ##if(length(df.sens) > 0) df.sens <- rbind(df.sens, tmp.df.cur)
    df.sens <- rbind(df.sens, tmp.df.cur)
    
  }

  ## clean up
  rm(list = ls(pattern = "^tmp.*"))
}

## change name of group for all
df.sens[grep(" Composite sensitivities for all observations/prior info ----->", 
             df.sens$obs.group), 1] <- "all"

as.character(unique(df.sens$par))[order(df.sens$obs.group.rank[grep("all", df.sens$obs.group)])]

## make obs group name a factor
df.sens$obs.group <- factor(df.sens$obs.group, levels =  unique(df.sens$obs.group))

## make par a factor
df.sens$par <- factor(
  df.sens$par, 
  levels =  as.character(
    unique(df.sens$par))[order(
      df.sens$obs.group.rank[grep("all", df.sens$obs.group)],
      decreasing = TRUE)])

junk <- reshape(df.sens[ , c("obs.group", "par", "obs.group.rank")],
                idvar = "par", timevar = "obs.group", direction = "wide")



library(ggplot2)
p.ranks <- ggplot(data = df.sens) + 
  geom_tile(aes(y = par, x = obs.group, fill = obs.group.rank), color = "grey") +
  geom_text(aes(y = par, x = obs.group, label = obs.group.rank)) +
  ggtitle(chr.title) +
  xlab("Observation Group") + ylab("Paramater") + 
  scale_fill_gradient("Sensitvity", low = "#edf8b1", high = "#2c7fb8", 
                      limits = c(1,24),
                      breaks = c(1,5,10,15,20,24), 
                      labels = c("1 most",5,10,15,20,"24 least")) +
  guides(fill = guide_legend(reverse = FALSE))


pdf(file = "c:/temp/sens-plot.pdf", height = 8.5, width = 11)  
plot(p.ranks)
dev.off()



