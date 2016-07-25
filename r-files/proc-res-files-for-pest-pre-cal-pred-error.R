## purpose of this script is to process the residual files from the Monte Carlo 
## simulations for use in pest prediction error analaysis for the 
## pre-calibration of HSPF model for BigElk Creek. For the predictive error 
## analysis, only obs flow flow on days when bacteria samples were collected 
## are used.
## 
## load packages
##library(stringr)
library(ggplot2)
library(scales)
library(doBy)
library(smwrStats)

## paths
## primary path
chr.dir.prime <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"
chr.dir.pred.pre.cal <- paste0(chr.dir.prime,
                               "/pest-hspf-files/pred-unc-pre-cal")
## file name for saved data
chr.filename.save <- "pred-err-pre-cal-mc"

## residual file prefix
chr.prefix.res <- "pre"

## get observed flow
load(file = paste0(chr.dir.pred.pre.cal, "/obs-flow.RData"))

## long format of data
df.all.data.pred.err.pre.cal <- data.frame(
  run = "obs", 
  pred = paste0("prediction_",sprintf("%03i",1:length(df.flow.est.pred$flow))),
  flow=df.flow.est.pred$flow)


## number of runs
n.run <- 1000

for(ii in 1:n.run) {
  chr.file.cur <- paste0(chr.dir.pred.pre.cal, "/", paste0(chr.prefix.res, ii), ".res")
  df.cur.res <- 
    gsub(" ","",
         do.call(
           rbind,
           strsplit(
             gsub("( ){2,}",",", 
                  scan(file = chr.file.cur, what = "charcater", sep = "\n",
                       quiet = TRUE)),
             split = ","))[-1,c(1,4)])
  
  df.tmp <- data.frame(
    run = paste0(chr.prefix.res, ii), 
    pred = paste0("prediction_",sprintf("%03i",1:length(df.flow.est.pred$flow))),
    flow=abs(df.flow.est.pred$flow + as.numeric(df.cur.res[, 2])))
  
  df.all.data.pred.err.pre.cal <- rbind(df.all.data.pred.err.pre.cal, df.tmp)
  rm(chr.file.cur, df.cur.res, df.tmp)
}

## save copy of results
save(df.all.data.pred.err.pre.cal, 
     file = paste0(chr.dir.pred.pre.cal, "/", chr.filename.save, ".RData"))


junk <- data.frame(df.all.data.pred.err.pre.cal, res = NA)

str(junk)

junk.2 <- transformBy(~ run, data = junk[junk$run != "obs", ], res = (flow - junk$flow[junk$run == "obs"]))


tail(jun)
## clean up
rm(df.flow.est.pred)

## look at sd for residuals
summaryBy(res ~ pred, data = junk.2, FUN = c(mean,sd,skew))




## boxplots
p.box <- 
  ggplot() + 
  geom_boxplot(
    data = df.all.data.pred.err.pre.cal[df.all.data.pred.err.pre.cal$run != "obs", ], 
    aes(pred, flow))

p.box <- p.box + geom_point(data = df.all.data.pred.err.pre.cal[df.all.data.pred.err.pre.cal$run == "obs", ], 
                            aes(pred, flow), color = "red", fill = "blue", size = 4, pch = 21)

p.box <- p.box + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))

p.box <- p.box + ylab("Flow (cfs)") + xlab("Prediction") +
  scale_x_discrete(labels= sprintf("%04i", 1:44)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))


plot(p.box)

## histograms
f.binwidth <- round((max(df.all.data.pred.err.pre.cal$flow) -
                 min(df.all.data.pred.err.pre.cal$flow)[1]) / 200)
p.hist <- 
  ggplot(
    data = df.all.data.pred.err.pre.cal[df.all.data.pred.err.pre.cal$pred == "prediction_004" &
                                          df.all.data.pred.err.pre.cal$run != "obs", ]) +
  geom_histogram(aes(flow), binwidth = f.binwidth)

plot(p.hist)

## densities
chr.pred <- "prediction_027"
p.dens <- 
  ggplot(
    data = df.all.data.pred.err.pre.cal[df.all.data.pred.err.pre.cal$pred == chr.pred &
                                          df.all.data.pred.err.pre.cal$run != "obs", ]) +
  geom_density(aes(x = flow, y = ..scaled..), color = "red") +
  geom_vline(xintercept = df.all.data.pred.err.pre.cal[df.all.data.pred.err.pre.cal$pred == chr.pred &
                                                         df.all.data.pred.err.pre.cal$run == "obs", "flow"],
             color = "blue") + scale_x_log10()

plot(p.dens)

