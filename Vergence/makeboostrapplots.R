
library(dplyr)
library(ggplot2)
library(tidyr)

b<- readRDS('Bootstrap1999results.RDS')

b %>%
  mutate(term=replace(term,term=='verg.velocity:enhance.typeconvergence','Convergence'),
         term=replace(term,term=='verg.velocity:enhance.typedivergence','Divergence'),
         term=replace(term,term=='verg.velocity:enhance.typenone','Slow.Velocity'),
         term=replace(term,term=='(Intercept)','Bias'),
         overlap.zero=low*high<0,
         mm=replace(m,overlap.zero,NA))->
  b


overlapterm<- function(b, A='Convergence',B='Slow.Velocity'){
  # print(b)
  lowA=b$low[b$term==A]
  highA=b$high[b$term==A]
  lowB=b$low[b$term==B]
  highB=b$high[b$term==B]
  
  r=((lowA<highB & highA > highB) | (lowA<lowB & highA > lowB))
  # print(r)
  return(r)
  
}

conv.is.slow=overlapterm(b,'Convergence','Slow.Velocity')
conv.is.div=overlapterm(b,'Convergence','Divergence')

btest<-data.frame(neuron=unique(b$neuron),conv.is.slow,conv.is.div)

bx<- left_join(b,btest,by='neuron')

##########
#in the next 2 lines, use m for all data or mm for data with zero-crossers replaced with NA
bx<- dplyr::select(bx,term,neuron,mm,conv.is.slow,conv.is.div)
bx<- spread(bx,term,mm)
#########

ggplot(bx)+
  geom_point(aes(Convergence,Slow.Velocity,shape=verg.angle<0,color=conv.is.slow),size=3)+
  geom_abline(slope=1)

ggplot(bx)+
  geom_point(aes(Convergence,Slow.Velocity,color=conv.is.slow),size=3)+
  geom_abline(slope=1)

ggplot(bx)+
  geom_point(aes(Convergence,Divergence,color=conv.is.div),size=3)+
  geom_abline(slope=1)



