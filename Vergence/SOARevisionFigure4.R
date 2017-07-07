#THis figure will show the two models used to predict firing rate.
#First, I'll show the simple model massively overestimates the firing rate during saccades
#Then I'll show that a more complex model improves the fit (will I?) 
#And demonstrate that the terms are significant, using bootstrapping 
#Next demonstrate that the terms are significantly different from each other
#And finally, show that sensitivity is reduced during saccades

modelVV<- function(t,chosenCell='Bee-211',saccadebuffer=200,saccadethreshold=30,
                    model.form='sdf20~+verg.angle+verg.velocity', #simple model
                    lagsdf=31,returnmodel=FALSE,
                   includeSaccades=FALSE){
  
  x<- ungroup(filter(t,neuron==chosenCell))
  if (!('time' %in% names(x))){
    x<- mutate(x,time=row_number())
  }
  parabolic_n<- 10
  sdfsigma=10
  x %>%
    mutate(verg.velocity=parabolicdiff(lep-rep,parabolic_n),
           rev=parabolicdiff(rep,parabolic_n),
           lev=parabolicdiff(lep,parabolic_n),
           revV=parabolicdiff(repV,parabolic_n),
           levV=parabolicdiff(lepV,parabolic_n),
           sdf=spikedensity(rasters,sd=sdfsigma),
           sdf20=dplyr::lag(sdf,lagsdf),
           conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) ->
    x
  
  x<- joinsaccades(x,buffer=saccadebuffer,threshold=saccadethreshold)
  x %>%
    group_by(sacnum) %>%
    mutate(verg.amp=last(verg.angle)-first(verg.angle),
           isconj=verg.amp<1,
           # saccadic=counter>saccadebuffer & counter<n()-saccadebuffer+20,
           saccadic=counter>saccadebuffer & counter<n()-saccadebuffer,
           enhance.type=as.character(saccadic),
           saccadic=replace(saccadic,!saccadic,NA),
           conj.h.vel=(lev+rev)/2,
           conj.v.vel=(levV+revV)/2,
           instant.dir=atan2(conj.v.vel,conj.h.vel)*180/pi
           # target.verg=thp-thp2,
           # verg.error=verg.angle-target.verg
    )->
    x
  x<- mutate(x,verg.bins=cut(verg.amp,c(-50,-1,1,50)))
  levels(x$verg.bins)<- c('Diverging','Conjugate','Converging')
  x %>%
    mutate(enhance.type=replace(enhance.type,enhance.type=='FALSE','slow'),
           enhance.type=replace(enhance.type,is.na(enhance.type),'slow'),
           # enhance.type=replace(enhance.type,enhance.type=='slow'&abs(verg.velocity)<2.5,'fix'),
           enhance.type=replace(enhance.type,enhance.type=='slow'&verg.velocity>0,'slowC'),
           enhance.type=replace(enhance.type,enhance.type=='slow'&verg.velocity<0,'slowD'),
           enhance.type=replace(enhance.type,enhance.type=='TRUE' & verg.velocity>0,'conver'),
           enhance.type=replace(enhance.type,enhance.type=='TRUE' & verg.velocity<0,'diver'),
           enhance.type=as.factor(enhance.type))->
    x
  x %>% group_by(time) %>%
    summarize_each(funs(first)) %>%
    ungroup() %>%
    filter(verg.bins != 'Conjugate',
           abs(verg.velocity<400))->
    xm
  # x<- mutate(x,saccadic=!is.na(sacnum))
  if (includeSaccades){
    mod<- lm(model.form,data=xm) 
  }else{
    mod<- lm(model.form,data=filter(xm,is.na(saccadic),verg.velocity>0))
  }
  # mod<- lm(model.form,data=filter(x,!saccadic))
  # mod<- lm(model.form,data=filter(x,verg.velocity>0,!saccadic))
  
  # mod<-lm(model.form,data=xm)
  # mod<- lm(model.form,data=filter(x,!isconj))
  if (returnmodel) return(mod)
  print(tidy(mod))
  # mod2<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none',sdf20>30))
  message(paste('R-squared =', summary(mod)$r.squared))
  # x<- dplyr::select(x,-sacnum,-counter)
  x<- mutate(ungroup(x),predSDF=predict(mod,newdata=x),
             showrasters=replace(rasters,rasters<1,NA))
  
}

mod<-modelVV(t,chosenCell='Ozette-122',lagsdf=20,
                           model.form='sdf20~+verg.angle+verg.velocity',
                           returnmodel = TRUE)
bufferlength=200

modelVV(t,chosenCell='Ozette-122',lagsdf=35,
                model.form='sdf20~+verg.angle+verg.velocity',
                saccadebuffer = bufferlength) %>%
  group_by(sacnum) %>%
  mutate(saccade.dur=n()-2*bufferlength, 
         saccade.end=saccade.dur+bufferlength,
         peak.conj.velocity=maxabs(conj.velocity),
         peak.R.H= maxabs(rev),
         peak.R.V= maxabs(revV),
         peak.L.H= maxabs(lev),
         peak.L.V= maxabs(levV),
         R.H.Amp=rep[saccade.end]-rep[bufferlength],
         L.H.Amp=lep[saccade.end]-lep[bufferlength],
         R.V.Amp=repV[saccade.end]-repV[bufferlength],
         L.V.Amp=lepV[saccade.end]-lepV[bufferlength],
         conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
         conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
         r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
         r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
         vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
         maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
         saccadic.verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
         total.verg.amp=sum(verg.velocity)/1000,
         mean.verg.amp=mean(verg.angle[saccade.end:n()]-mean(verg.angle[1:bufferlength])),
         peak.verg.velocity= maxabs(verg.velocity),
         min.verg.trans = min(verg.velocity),
         max.verg.trans = max(verg.velocity),
         off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
         min.verg.angle=min(verg.angle),
         max.verg.angle=max(verg.angle),
         max.verg.velocity=max(verg.velocity),
         min.verg.velocity=min(verg.velocity),
         initial.verg.angle=verg.angle[bufferlength],
         peakFR=max(sdf),
         predict.peakFR=max(predSDF))->
  z

zpsimp<- summarize_each(z,funs(first))
zpsimp<-filter(zpsimp,r.amp>3,abs(total.verg.amp)<30)
qplot(peakFR,predict.peakFR,data=zpsimp)+
  geom_abline()+
  stat_smooth(method='lm')

qplot(total.verg.amp,peakFR,data=zpsimp)+
  geom_point(aes(total.verg.amp,predict.peakFR),color='orange')+
  xlim(0,NA)

A<-qplot(peak.verg.velocity,peakFR,data=zpsimp)+
  geom_point(aes(peak.verg.velocity,predict.peakFR),color='orange')+
  xlim(0,NA)+
  xlab('Peak Vergence Velocity (deg/s)')+
  ylab('Peak Firing Rate (spks/s)')
A
# ggsave('Fig4A-SimplePrediction.PDF',height=5,width=8,plot=A+theme_bw())

qplot(saccadic.verg.amp,peakFR,data=zpsimp)+
  geom_point(aes(saccadic.verg.amp,predict.peakFR),color='orange')

#ComplexModel----
modelVV(t,chosenCell='Bee-211',lagsdf=20,
        model.form='sdf20~+verg.angle+verg.velocity:enhance.type',
        saccadebuffer = bufferlength,
        includeSaccades=TRUE) %>%
  group_by(sacnum) %>%
  mutate(cverg=abs(verg.velocity)>3,
         cverg=replace(counter,cverg,NA),
         saccade.dur=n()-2*bufferlength, 
         saccade.end=saccade.dur+bufferlength,
         peak.conj.velocity=maxabs(conj.velocity),
         peak.R.H= maxabs(rev),
         peak.R.V= maxabs(revV),
         peak.L.H= maxabs(lev),
         peak.L.V= maxabs(levV),
         R.H.Amp=rep[saccade.end]-rep[bufferlength],
         L.H.Amp=lep[saccade.end]-lep[bufferlength],
         R.V.Amp=repV[saccade.end]-repV[bufferlength],
         L.V.Amp=lepV[saccade.end]-lepV[bufferlength],
         conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
         conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
         r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
         r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
         vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
         maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
         saccadic.verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
         total.verg.amp=sum(verg.velocity)/1000,
         # mean.verg.amp=mean(verg.angle[saccade.end:n()]-mean(verg.angle[1:bufferlength])),
         peak.verg.velocity= maxabs(verg.velocity),
         min.verg.trans = min(verg.velocity),
         max.verg.trans = max(verg.velocity),
         off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
         min.verg.angle=min(verg.angle),
         max.verg.angle=max(verg.angle),
         max.verg.velocity=max(verg.velocity),
         min.verg.velocity=min(verg.velocity),
         initial.verg.angle=verg.angle[bufferlength],
         peakFR=max(sdf),
         predict.peakFR=max(predSDF),
         verg.lead=bufferlength-max(cverg,na.rm=T),
         verg.lead=replace(verg.lead,is.na(verg.lead),0))->
  z

z<- mutate(z,verg.bins=cut(verg.amp,c(-50,-1,1,50)))
levels(z$verg.bins)<- c('Diverging','Conjugate','Converging')

zp<- summarize_each(z,funs(first))
zp<-filter(zp,r.amp>3,abs(total.verg.amp)<30)
qplot(verg.lead,min.verg.trans,data=zp)
qplot(peakFR,predict.peakFR,data=zp)+
  geom_abline()+
  stat_smooth(method='lm')

qplot(total.verg.amp,peakFR,data=zp)+
  geom_point(aes(total.verg.amp,predict.peakFR),color='orange')

B<- qplot(peak.verg.velocity,peakFR,data=zp)+
  geom_point(aes(peak.verg.velocity,predict.peakFR),color='hotpink')+
  xlim(0,NA)+
  xlab('Peak Vergence Velocity (deg/s)')+
  ylab('Peak Firing Rate (spks/s)')
B
# ggsave('Fig4B-ComplexPrediction.PDF',height=5,width=8,plot=B+theme_bw())


qplot(saccadic.verg.amp,peakFR,data=zp)+
  geom_point(aes(saccadic.verg.amp,predict.peakFR),color='orange')
#----
goodsacs=unique(z$sacnum[z$r.amp>2&z$verg.amp>2])
manipulate({
  d2<- filter(z,sacnum==goodsacs[chosenSac])  
  verg.start=max(d2$counter[d2$counter<200&abs(d2$verg.velocity)<3])
  verg.end=min(d2$counter[d2$counter>200+first(d2$saccade.dur)&abs(d2$verg.velocity)<3])
  ggplot(d2)+
    geom_point(aes(counter,showrasters+30),shape='|',size=3)+
    geom_line(aes(counter,verg.velocity),color='darkblue',size=2)+
    # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
    geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
    geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkblue')+
    # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
    geom_area(aes(counter,predSDF),fill='orange',alpha=.2)+
    geom_area(aes(counter,sdf),alpha=0.2)+
    geom_hline(yintercept = c(-3,3))+
    # ylim(c(NA,150))+
    xlab('Time (ms)')+
    ylab('Velocity (deg/s)')+
    # annotate('text',x=100,y=30,label=paste('real.amp=',round(real.amp,2)))+
    # annotate('text',x=100,y=25,label=paste('predict.amp=',round(predict.amp,2)))+
    # annotate('text',x=100,y=20,label=paste('sacnum=',d2$sacnum[1]))+
    geom_line(aes(counter,verg.angle*10),color='darkgreen')#+
    # geom_line(aes(counter,target.verg*10))
},
chosenSac=slider(1,length(goodsacs),initial=1))

#BatchRecalculate----
t %>%
  group_by(neuron) %>%
  do(tidy(modelVV(t,chosenCell=first(.$neuron),lagsdf=20,
                  model.form='sdf20~+verg.angle+verg.velocity:enhance.type',
                  saccadebuffer = bufferlength,
                  includeSaccades=TRUE,returnmodel = TRUE))) %>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','b'))->
  complex

complex %>%
  mutate(term=replace(term,term=='verg.velocity:enhance.typeconver','conver'),
         term=replace(term,term=='verg.velocity:enhance.typediver','diver'),
         term=replace(term,term=='verg.velocity:enhance.typeslowC','slowC'),
         term=replace(term,term=='verg.velocity:enhance.typeslowD','slowD'))->
  complex

cx<- spread(complex,term,estimate)

#compare slow convergence to enhanced convergence
qplot(conver,slowC,data=cx)+geom_abline()+geom_vline(xintercept = 0)

qplot(conver,slowC,data=cx)+geom_abline()+geom_vline(xintercept = 0)+
  geom_point(aes(conver,slowC),color='red',size=2,data=filter(cx,neuron=='Bee-211'))+
  coord_fixed()

slowvsfast<-ggplot(cx)+
  geom_point(aes(conver,slowC))+
  # geom_point(aes(conver,slowD),color='hotpink')+
  geom_abline()+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  xlab('Sensitivity to Saccadic Convergence (spks/s)/(deg/s)')+
  ylab('Sensitivity to Non-Saccadic Convergence (spks/s)/(deg/s)')



##uncomment if you need the model for bootstrapping or whatever
# t %>%
#   group_by(neuron) %>%
#   do(m=modelVV(t,chosenCell=first(.$neuron),lagsdf=20,
#                model.form='sdf20~+verg.angle+verg.velocity',
#                saccadebuffer = bufferlength,
#                includeSaccades=FALSE,returnmodel = TRUE))->
#   simple

t %>%
  group_by(neuron) %>%
  do(tidy(modelVV(t,chosenCell=first(.$neuron),lagsdf=20,
               model.form='sdf20~+verg.angle+verg.velocity',
               saccadebuffer = bufferlength,
               includeSaccades=FALSE,returnmodel = TRUE))) %>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','b'))->
  simpletidy

t %>%
  group_by(neuron) %>%
  do(tidy(modelVV(t,chosenCell=first(.$neuron),lagsdf=20,
                  model.form='sdf20~+verg.angle+verg.velocity',
                  saccadebuffer = bufferlength,
                  includeSaccades=TRUE,returnmodel = TRUE))) %>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','b'))->
  simpleplot

st<- spread(simpletidy,term,estimate)
st<- separate(st,neuron,c('monkey','cellnum'),remove=FALSE)

ggplot(st)+
  geom_point(aes(verg.angle,verg.velocity),size=3)+
  xlab('Sensitivity to Vergence Angle (spks/s/deg)')+
  ylab('Sensitivity to Vergence Velocity (spks/s)/(deg/s)')

ggplot(st)+
  geom_point(aes(verg.angle,verg.velocity,color=monkey),size=3)+
  xlab('Sensitivity to Vergence Angle (spks/s/deg)')+
  ylab('Sensitivity to Vergence Velocity (spks/s)/(deg/s)')

n %>%
  group_by(neuron) %>%
  do(m=modelVV(.,chosenCell=first(.$neuron),lagsdf=20,
                  model.form='sdf20~+verg.angle+verg.velocity',
                  saccadebuffer = bufferlength,
                  includeSaccades=FALSE,returnmodel = TRUE)) %>%
  mutate(vvimp=calc.relimp(m,rela=F)$lmg[1],
         vaimp=calc.relimp(m,rela=F)$lmg[2],
         m=NULL) ->
  simplerelaimp

ggplot(simplerelaimp)+geom_point(aes(vaimp,vvimp),size=3)


#FOR PAPER:----
ggplot(filter(complex,term %in% c('conver','slowC')))+
  geom_boxplot(aes(term,estimate))+
  theme_minimal()+
  ylab('Sensitivity to Vergence Velocity (spks/s)/(deg/s)')+
  xlab('')

ggsave('4D.PDF',height=8,width=10)

ggplot(filter(simpleplot,term=='verg.velocity'))+
  geom_boxplot(aes(1,estimate))+
  xlim(0,2)+
  theme_minimal()+
  ylab('Sensitivity to Vergence Velocity (spks/s)/(deg/s)')+
  xlab('All Movements (excluding conjugate saccades)')
  

ggsave('4B.PDF',height=8,width=10)

#might need to use rstudio to export this one - plot it then use export option
A<-qplot(peak.verg.velocity,peakFR,data=zpsimp)+
  geom_point(aes(peak.verg.velocity,predict.peakFR),color='orange')+
  xlim(0,NA)+
  xlab('Peak Vergence Velocity (deg/s)')+
  ylab('Peak Firing Rate (spks/s)')
ggsave('4A.PDF',plot=A+theme_bw(),height=8,width=12)

ggplot(cx)+
  geom_point(aes(conver,slowC))+
  # geom_point(aes(conver,slowD),color='hotpink')+
  geom_abline()+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  xlab('Sensitivity to Saccadic Convergence (spks/s)/(deg/s)')+
  ylab('Sensitivity to Non-Saccadic Convergence (spks/s)/(deg/s)')+
  theme_bw()
ggsave('4C.PDF',height=16,width=8)

#BOOTSTRAPPING GOES HERE----
#see BootstrapSaccadesScript.R
bootstrapSaccades<- function(n){
  # z<- readRDS('bootstrapSaccades.RDS')
  get('z') #get from global environment
  # if (n %% 100 == 0){
  #   message(n)
  # }
  z %>%
    group_by(verg.bins) %>%
    filter(verg.bins != 'Conjugate') %>%
    sample_frac(1,replace=TRUE) %>%
    ungroup() %>%
    do(tidy(lm(sdf20~verg.angle+verg.velocity:enhance.type,data=.))) %>%
    mutate(repN=n)->
    zx
}
n<- matrix(1:1999)
# x<- as.data.frame(rbindlist(apply(n,1,bootstrapSaccades)))
xx<-readRDS('Bootstrap1999quantiles.RDS')


#GroupBootstra----
#First, go to BootstrapSaccadesScript.R and run that on all the cells. This takes hours 
#then come back here with xx after you rbindlist it
# xx<-readRDS('Bootstrap1999quantiles.RDS')
# xx$term<-as.factor(xx$term)
# levels(xx$term)<-c('b','verg.angle','fastC','fastD','slowC','slowD')

xx<- readRDS('Bootstrap1999-final.RDS')

# qplot(data=filter(xx,term %in% c('fastC','slowC')),estimate,binwidth=0.005,fill=neuron)+
#   facet_wrap(~term,ncol=1)

#find confidence intervals
xx %>%
  group_by(neuron,term) %>%
  summarize(ciLOW=quantile(estimate,probs=0.025),
         ciHIGH=quantile(estimate,probs=0.975),
         m=mean(estimate)) %>%
  mutate(zerocross=(ciLOW*ciHIGH)<0)->
  confints

confints %>%
  select(neuron,term,m) %>%
  spread(term,m)->
  ciplot

confints %>%
  select(neuron,term,ciLOW) %>%
  spread(term,ciLOW)->
  ci.low
names(ci.low)<-paste(names(ci.low),'low',sep='.')

confints %>%
  select(neuron,term,ciHIGH) %>%
  spread(term,ciHIGH)->
  ci.high
names(ci.high)<-paste(names(ci.high),'high',sep='.')

ciplot<-cbind(ciplot,ci.low,ci.high)


ggplot(ciplot,aes(fastC,slowC))+
  geom_errorbar(aes(ymin=slowC.low,ymax=slowC.high),size=0.5,width=0)+
  geom_errorbarh(aes(xmin=fastC.low,xmax=fastC.high),height=0,size=0.5)+
  geom_point(size=1,color='hotpink')+
  geom_abline()+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  xlab('Sensitivity to Saccadic Vergence Velocity (spks/s)/(deg/s)')+
  ylab('Sensitivity to Non-Saccadic Vergence Velocity (spks/s)/(deg/s)')
  

# xx1<- readRDS('Bootstrap1999-6-15-pt1.RDS')
# xx2<- readRDS('Bootstrap1999-6-15.RDS')
# xx<-rbind(xx1,xx2)
# saveRDS(xx,'Bootstrap1999quantiles.RDS')

#BootstrapDemoPlot----
x<- filter(xx,neuron=='Bee-211')

ciplot %>%
  filter(neuron=='Bee-211') %>%
  select(neuron,fastC.low,fastC.high,slowC.low,slowC.high) ->
  bootplot

qplot(data=filter(x,term %in% c('fastC','slowC')),
      estimate,binwidth=0.005,fill=term)

levels(x$term)<-c('b','verg.angle','Saccadic Convergence','fastD','Non-Saccadic Convergence','slowD')

#get ciplot from next section
ggplot(filter(x,term %in% c('Saccadic Convergence','Non-Saccadic Convergence')))+
  geom_histogram(aes(estimate,fill=term),
                 binwidth=0.005,
                 color='white',
                 size=0.5)+
  theme_minimal()+
  xlab('Sensitivity to Vergence Velocity (spks/s)/(deg/s)')+
  ylab('Number of Bootstrap Iterations')+
  geom_segment(aes(x=slowC.low,xend=slowC.high),data=bootplot,
               y=-10,yend=-10,size=3,color='#00BFC4')+
  geom_segment(aes(x=fastC.low,xend=fastC.high),data=bootplot,
               y=-10,yend=-10,size=3,color='#F8766D')+
  theme(legend.position=c(0.5,0.5),
        legend.title=element_blank())
                 
