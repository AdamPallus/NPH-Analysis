
sd<-100
gsize<- sd*10

g<-dnorm(-gsize:gsize,mean=0,sd=sd)
sdf<-convolve(t$rasters,g,type="open")
sdf<-sdf[gsize:(length(sdf)-(gsize+1))]*1000

spiketimes<-which(t$rasters==1)
isi<-spiketimes[2:length(spiketimes)]-spiketimes[1:(length(spiketimes)-1)]
fr<-(1/isi)*1000

firing<-data.frame(spiketimes,fr=c(0,fr))

library(dplyr)
t <-mutate(t,time=row_number())
library(ggplot2)
qplot(spiketimes,fr,data=firing)+coord_cartesian(xlim=c(1,30000))+geom_line()
