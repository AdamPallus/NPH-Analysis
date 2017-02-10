t<- readRDS('EnhanceMarkedSOA.RDS')
t %>%
group_by(neuron) %>%
mutate(sdf20= lag(sdf,20)) ->
t
t %>% group_by(neuron) %>%
do(joinsaccades(.,buffer=20,threshold=20))->
t
x<- filter(t,neuron=='Bee-215')

mod<- lm('sdf20~verg.angle+verg.velocity:enhance.type',data=filter(x,saccade.type != 'conj'))
summary(mod)
# x<- filter(x,saccade.type != 'conj')
x<- mutate(x,enhance.type2=replace(enhance.type,enhance.type=='convergence','none'))
x<- rename(x, enhance.type.old=enhance.type,enhance.type=enhance.type2)
x<- ungroup(x)
# x<- mutate(x,slowpredict=predict(mod,newdata=x))


# ggplot(filter(x,time<10000))+
#   geom_line(aes(time,sdf))+
#   geom_line(aes(time,slowpredict),color='orange')

s<- tidy(mod)$estimate
x<- mutate(x, pred= (sdf20-s[1]-verg.angle*s[2])/s[5])
x<- mutate(x, predDIV= (sdf20-s[1]-verg.angle*s[2])/s[4])
x$pred[x$enhance.type=='divergence']=x$predDIV[x$enhance.type=='divergence']

window_size=2000
step_size=1000

manipulate(ggplot(filter(x,time>=window,time<window+window_size))+
             # geom_line(aes(time,sdf))+
             # geom_line(aes(time,slowpredict),color='orange')+
             geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
             geom_line(aes(time,pred),color='orange')+
             geom_line(aes(time,verg.velocity),color='darkblue')+
             geom_line(aes(time,verg.velocity-pred),color='red')
             ,
           window=slider(window_size,max(x$time-window_size),step=step_size))

ggplot(filter(xx,time>=22500,time<23000))+
  geom_line(aes(time,sdf))+
  geom_line(aes(time,slowpredict),color='orange')+
  geom_line(aes(time,verg.angle*10),color='darkgreen')+
  geom_line(aes(time,verg.velocity),color='maroon')

#--------------------------------------------

# x<- ungroup(filter(t,neuron=='Bee-113'))
x<- ungroup(filter(t,neuron=='Ozette-120'))

#smooth?
x %>%
  mutate(verg.velocity=parabolicdiff(lep-rep,20),
         sdf=spikedensity(rasters,sd=20),
         sdf20=lag(sdf,20)) ->
  x



# mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,saccade.type != 'conj',enhance.type=='none',sdf20>30))
mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none'))
mod2<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none',sdf20>30))
summary(mod)

x<- mutate(x,predV=predict(mod,newdata=x),
           predV2=predict(mod2,newdata=x),
           showrasters=replace(rasters,rasters<1,NA))

window_size=2000
step_size=1000

manipulate(ggplot(filter(x,time>=window,time<window+window_size))+
             geom_point(aes(time,showrasters+30),shape='|')+
             geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
             # geom_line(aes(time,slowpredict),color='orange')+
             geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
             # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
             geom_line(aes(time,predV),color='orange')+
             geom_line(aes(time,predV2),color='magenta')+
             geom_line(aes(time,verg.velocity),color='darkblue')+
             geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
             # ylim(c(-10,20))
           ,
           window=slider(window_size,max(x$time-window_size),step=step_size))

ggplot(filter(x,time>=9800, time<10500))+
  # geom_line(aes(time,sdf))+
  # geom_line(aes(time,slowpredict),color='orange')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange')+
  geom_line(aes(time,verg.velocity),color='darkblue')

ggplot(filter(x,time>=9800, time<10500))+
  # geom_line(aes(time,sdf))+
  # geom_line(aes(time,slowpredict),color='orange')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange')+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  ylim(c(-10,20))



#====SACCADES=================

xx <- joinsaccadesuniform(x,buffer=200,threshold=20,saccade.length=150)
xx %>%
  group_by(sacnum) %>%
  mutate(conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         conj.h.amp=last(conj.h)-first(conj.h),
         conj.v.amp=last(conj.v)-first(conj.v),
         conj.angle=atan2(conj.v,conj.h)*180/pi,
         peak.verg.velocity=maxabs(verg.velocity),
         r.amp=sqrt(conj.h^2+conj.v^2),
         verg.amp=last(verg.angle)-first(verg.angle),
         saccade.type='conj',
         saccade.type=replace(saccade.type,verg.amp< -2 & r.amp>2,'diverging'),
         saccade.type=replace(saccade.type,verg.amp> 2& r.amp>2,'converging'),
         saccade.type=replace(saccade.type,is.na(sacnum) | r.amp<2,'no.saccade')) ->
  xx

# p<- filter(xx,verg.amp>3)
p<- filter(xx,r.amp>5)
goodsacs<- unique(p$sacnum)     
nsac=length(goodsacs)


manipulate(ggplot(filter(xx,sacnum==goodsacs[sac]))+
             geom_point(aes(counter,showrasters+30),shape='|')+
             geom_line(aes(counter,verg.angle*10+0),color='darkgreen')+
             geom_line(aes(counter,predV),color='orange')+
             geom_line(aes(counter,predV2),color='magenta')+
             geom_line(aes(counter,verg.velocity),color='darkblue')+
             geom_line(aes(counter,verg.velocity-predV),color='red',linetype=2)+
             geom_area(aes(counter,sdf),alpha=1/10,fill='pink',color='black')
           ,
           sac=slider(2,nsac,step=1))

ggplot(filter(xx,sacnum.y %in% goodsacs),aes(group=sacnum.y))+
  geom_line(aes(counter.y,predV),color='magenta')+
  geom_line(aes(counter.y,verg.velocity),color='darkblue')

demopredictVV<-function(xx,min.amp=4,max.amp=8){
  p<- filter(xx,verg.amp>min.amp,verg.amp<max.amp)
  goodsacs<- unique(p$sacnum.y) 
  
  xx %>%
    filter(sacnum.y %in% goodsacs) %>%
    group_by(counter.y) %>%
    mutate(n=n(),
           goodsacs=list(goodsacs)) %>%
    summarize_each(funs(mean))->
    ss
}

ggplot(ss)+
  geom_line(aes(counter.y,predV),color='orange',size=2)+
  geom_line(aes(counter.y,verg.velocity),color='darkblue',size=2)+
  geom_line(aes(counter.y,verg.angle*10),color='darkgreen',size=2)+
  geom_line(aes(counter.y,verg.velocity,group=sacnum.y),color='darkblue',alpha=1/20,data=filter(xx,sacnum.y %in% goodsacs))+
  geom_area(aes(counter.y,sdf),fill='pink',alpha=1/5)

manipulate(ggplot(demopredictVV(xx,min.amp=minamp,max.amp=maxamp))+
             geom_line(aes(counter.y,predV),color='orange',size=2)+
             geom_line(aes(counter.y,verg.velocity),color='darkblue',size=2)+
             geom_line(aes(counter.y,verg.angle*10),color='darkgreen',size=2)+
             geom_area(aes(counter.y,sdf),fill='pink',alpha=1/5)+
             geom_label(aes(200,250,label=n))
           ,
           
           minamp=slider(-10,10,initial=5),
           maxamp=slider(-10,10,initial=10)
)

chooseSacs<- function(xx,min.amp,max.amp){
  p<- filter(xx,verg.amp>min.amp,verg.amp<max.amp)
  goodsacs<- unique(p$sacnum.y)
}

demopredictVV<-function(xx,goodsacs){
  xx %>%
    filter(sacnum.y %in% goodsacs) %>%
    group_by(counter.y) %>%
    mutate(n=n(),
           goodsacs=list(goodsacs)) %>%
    summarize_each(funs(mean))->
    ss
}

manipulate(ggplot(demopredictVV(xx,goodsacs=chooseSacs(xx,min.amp=minamp,max.amp=maxamp)))+
             geom_line(aes(counter.y,predV),color='orange',size=2)+
             geom_line(aes(counter.y,verg.velocity),color='darkblue',size=2)+
             geom_line(aes(counter.y,verg.angle*10),color='darkgreen',size=2)+
             geom_area(aes(counter.y,sdf),fill='pink',alpha=1/5)+
             geom_label(aes(200,250,label=n))+
             geom_line(aes(counter.y,verg.velocity,group=sacnum.y),
                       color='darkblue',alpha=1/20,
                       data=filter(xx,sacnum.y %in% chooseSacs(xx,min.amp=minamp,max.amp=maxamp)))
           ,
           
           minamp=slider(-10,10,initial=5),
           maxamp=slider(-10,10,initial=10)
)


parabolicdiff <- function(pos,n=7){
  q <- sum(2*((1:n)^2))
  convoutput<- rcpp_convolve(pos,c(-n:-1, 1:n))
  convoutput<- convoutput[(n*2):(length(pos)-((n*2)+1))]
  vels<- c(array(convoutput[1],dim=n*2),convoutput,array(convoutput[length(convoutput)],dim=n*2))
  vels <- vels/q*1000
}           

spikedensity<-function (rasters,sd=100) {
  gsize<- sd*10
  g<-dnorm(-gsize:gsize,mean=0,sd=sd)
  sdf<-rcpp_convolve(rasters,g)
  sdf<-sdf[gsize:(length(sdf)-(gsize+1))]*1000
  sdf
}
