## load packages
library(ggplot2)

## main path for uncert
chr.uncert.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert"

## main path for uncert-rerun
chr.uncert.rerun.dir <- paste0(chr.uncert.dir, "/uncert-rerun")

## save the pars to file
load(file = paste0(chr.uncert.dir, "/all-par-vals.RData"))

df.pars$val <- as.numeric(df.pars$val)

## add type factor
unique(gsub("[0-9]{1,}","",df.pars$run))


# plots
p.agwrc <- ggplot(data = df.pars[df.pars$par == "agwrc000" , ]) + 
  xlab("") + ylab("Parameter Value")

p.agwrc <- p.agwrc + geom_point(data = df.pars[df.pars$par == "agwrc000" &
                                                 df.pars$run == "cal", ],
                                aes(x = par, y = val), shape = 15, size = 5,
                                colour = "red")

p.agwrc <- p.agwrc + geom_boxplot(data = df.pars[df.pars$par == "agwrc000" &
                                                 df.pars$run != "cal", ],
                                  aes(x = par, y = val))

plot(p.agwrc)


## sub-surface
p.sub <- ggplot(data = df.pars[df.pars$proc == "sub" , ]) + 
  xlab("") + ylab("Parameter Value")


p.sub <- p.sub + geom_boxplot(data = df.pars[df.pars$proc == "sub" &
                                                 df.pars$run != "cal", ],
                                  aes(x = par, y = val))
p.sub <- p.sub + geom_point(data = df.pars[df.pars$proc == "sub" &
                                             df.pars$run == "cal", ],
                            aes(x = par, y = val), shape = 15, size = 5,
                            colour = "red")

plot(p.sub)

# water balance
p.wtb <- ggplot(data = df.pars[df.pars$proc == "wtb" , ]) + 
  xlab("") + ylab("Parameter Value")


p.wtb <- p.wtb + geom_boxplot(data = df.pars[df.pars$proc == "wtb" &
                                               df.pars$run != "cal", ],
                              aes(x = par, y = val))
p.wtb <- p.wtb + geom_point(data = df.pars[df.pars$proc == "wtb" &
                                             df.pars$run == "cal", ],
                            aes(x = par, y = val), shape = 15, size = 5,
                            colour = "red")

plot(p.wtb)

# Surface
p.sur <- ggplot(data = df.pars[df.pars$proc == "sur" , ]) + 
  xlab("") + ylab("Parameter Value")


p.sur <- p.sur + geom_boxplot(data = df.pars[df.pars$proc == "sur" &
                                               df.pars$run != "cal", ],
                              aes(x = par, y = val))
p.sur <- p.sur + geom_point(data = df.pars[df.pars$proc == "sur" &
                                             df.pars$run == "cal", ],
                            aes(x = par, y = val), shape = 15, size = 5,
                            colour = "red")

plot(p.sur)
