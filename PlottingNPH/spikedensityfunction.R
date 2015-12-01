spikedensity<-function (rasters,sd=100) {
  
  gsize<- sd*10
  
  g<-dnorm(-gsize:gsize,mean=0,sd=sd)
  sdf<-convolve(t$rasters,g,type="open")
  sdf<-sdf[gsize:(length(sdf)-(gsize+1))]*1000
  sdf
}