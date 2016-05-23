

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

z<- filter(t,monkey=='Bee',cellnum %in% c(6,15,205))

dynamicleadverg<-function(p,lags=seq(0,150,by=5)) {
  
  rsq<-NULL
  bias<- NULL
  verg.angle<-NULL
  conv.trans<-NULL
  dive.trans<- NULL
  verg.velocity<- NULL
  for (i in 1:length(lags)) {
    if (lags[i] > 0){
      p$sdflag<-dplyr::lag(p$sdf,lags[i])
    }
    else{
      p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
    }
    m<- lm(sdflag~verg.angle+verg.velocity:transient.type,data=p)
    r<- summary(m)$coefficients
    rsq[i]<- summary(m)$r.squared
    bias[i]<- r[1]
    verg.angle[i]<- r[2]
    conv.trans[i]=r[3]
    dive.trans[i]=r[4]
    verg.velocity[i]=r[5]
    
  }
  x<- data.frame(lag=lags,bias=bias,verg.angle=verg.angle,conv.trans=conv.trans,dive.trans=dive.trans,
                 verg.velocity=verg.velocity,r2=rsq)
  # return(x)
  return(filter(x,r2==max(r2)))
}


t %>% 
  group_by(neuron) %>%
  mutate(time=row_number(),
         transient.type='none',
         transient.type=replace(transient.type, verg.velocity > 15,'convergence'),
         transient.type=replace(transient.type, verg.velocity < -15, 'divergence')) %>%
  do(dynamicleadverg(p=.)) ->
p



