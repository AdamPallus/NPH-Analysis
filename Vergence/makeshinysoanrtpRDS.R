
t<- readRDS('enhancemarked-NRTP.RDS')
tsoa<- readRDS('enhancemarked.RDS')

znrtp<- filter(t,neuron %in% c('Bee-15','Bee-33','Bee-09'))



zsoa<- filter(tsoa,neuron %in% c('Bee-211','Bee-209','Bee-112'))

z<- rbind(znrtp,zsoa)
z<-ungroup(z)
z<- mutate(z, showrasters=replace(rasters,rasters<1,NA))

ztest<- select(z,time,sdf,lev,rev,lep,rep,lepV,repV,showrasters,neuron)

saveRDS(ztest,'shinySOANRTP.RDS')

ztest %>%
  group_by(neuron) %>%
  mutate(smoothsdf=spikedensity(showrasters,sd=25)) ->
  ztest