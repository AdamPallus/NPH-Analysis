VAF<- function(mod,x,param='verg.velocity'){
  
  x<- mutate(x,predV=predict(mod,newdata=x))
  1-var(x$predV-x[param],na.rm=T)/var(x[param],na.rm=T)
}
