

library(ggplot2)
library(dplyr)
library(knitr)
library(xtable)
# library(tidyr)
# library(broom)
# library(grid)
# library(relaimpo)
# library(leaps)
# library(data.table)
# library(stringr)
# source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
t<-readRDS('SOA-NRTP.RDS')

t<- filter(t,monkey=='Bee',cellnum<100)


t %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         # sdf=spikedensity(rasters,sd=20),
         # verg.velocity=parabolicdiff(verg.angle,14),
         transient.type='none',
         transient.type=replace(transient.type, verg.velocity > 15,'convergence'),
         transient.type=replace(transient.type, verg.velocity < -15, 'divergence')) %>%

  do(r=summary(lm(sdf~verg.angle+verg.velocity:transient.type,data=.))$coefficients,
     m=summary(lm(sdf~verg.angle+verg.velocity:transient.type,data=.)),
     c=summary(lm(sdf~verg.angle+verg.velocity,data=.))) %>%
  mutate(bias=r[1],verg.angle=r[2],
         conv.trans=r[3],dive.trans=r[4],
         verg.velocity=r[5],
         r2=m$r.squared,
         r2control=c$r.squared,
         improvement=r2-r2control) %>%
  select(-r,-m,-c)->
  z