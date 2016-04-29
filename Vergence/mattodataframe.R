mat.to.data.frame<- function (filepath){
  library(R.matlab)
  library(dplyr)
  # filepath<- 'C:\\Users\\setup\\Desktop\\NRTP Vergence\\Bee_Mark_Vergence 2016.04.06\\Bee_Mark_Vergence_2016.04.06_1214_Vergence-sorted.mat'
  
  m<- readMat(filepath)
  spiketimes<- as.vector(m$spiketimes)
  rep<- m$H.Eye['values',,]
  lep<- m$H.Eye2['values',,]
  repV<-m$V.Eye['values',,]
  lepV<-m$V.Eye2['values',,]
  
  t<- data.frame(rep=rep$values,lep=lep$values,repV=repV$values,lepV=lepV$values)
  t<- mutate(t,time=row_number())
  t$rasters<- 0
  t$rasters[t$time %in% floor(spiketimes/50)]<- 1
  return(t)
}