t<- readRDS('crashproof.RDS')
library(dplyr)
library(relaimpo)
library(tidyr)
t %>% 
  group_by(neuron) %>%
  do(m=lm(maxfr~max.verg.angle+max.verg.velocity,data=.)) -> 
  mm

r<- data.frame()
r2 <- NULL

for (i in 1:nrow(mm)){
  bb<- calc.relimp(mm$m[[i]])
  b<- bb$lmg
  r<-rbind(r,b)
  r2<- c(r2,bb$R2)
  
}

r$neuron<-mm$neuron
r$R2<- r2

names(r)<- c(names(b),'neuron','R2')

r %>%
  separate(neuron, c('monkey','cellnum'),remove=FALSE) %>%
  mutate(celltype=as.factor(as.numeric(cellnum)>100))->
  r
levels(r$celltype)<- c("NRTP","SOA")
