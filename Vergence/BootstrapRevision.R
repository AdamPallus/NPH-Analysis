
z<- filter(t,neuron=='Bee-106')
z<- mutate(z,time=row_number())
z<- joinsaccadesuniform(z,buffer = 20,saccade.length = 150)

z %>%
  mutate( sdf=spikedensity(rasters,10),
          sdf20=lag(sdf,20),
          verg.angle=lep-rep,
          lev=parabolicdiff(lep,15),
          rev=parabolicdiff(rep,15),
          verg.velocity=lev-rev,
          verg.type='Converging',
          verg.type=replace(verg.type,verg.velocity<0,'Diverging'),
          saccadic=!is.na(sacnum)
          ) %>%
  group_by(sacnum) %>%
  mutate(verg.amp=last(verg.angle)-first(verg.angle),
         peak.verg.velocity=maxabs(verg.velocity),
         peakFR=max(sdf20,na.rm=T))->
  z

summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity,data=z))


summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity,data=filter(z,abs(verg.amp)>0.5)))

summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity,data=filter(z,abs(verg.amp)>0.5),verg.velocity>0))

#This one: 
summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity:verg.type,data=filter(z,abs(verg.amp)>0.5)))

# summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity:verg.type,data=z))

# summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity:verg.type,data=filter(z,is.na(sacnum))))

# summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity:is.na(sacnum),data=filter(z,verg.velocity>0)))

# summary(mod<- lm(sdf20 ~ verg.angle+verg.velocity:is.na(sacnum),data=filter(z,verg.velocity>0,abs(verg.amp)>0.5)))

bootfunc<- function(n,z){
  z %>%
    group_by(neuron) %>%
    filter(abs(verg.amp)>0.5) %>%
    sample_frac(1,replace=TRUE) %>%
    do(tidy(lm(sdf20 ~ verg.angle+verg.velocity:verg.type,data=.))) %>%
    mutate(n=n)->
    zx
  zx
}


t %>%
  # filter(neuron %in% c('Bee-101','Bee-102')) %>%
  group_by(neuron) %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer = 20,saccade.length = 150)) %>%
  mutate( sdf=spikedensity(rasters,10),
          sdf20=lag(sdf,20),
          verg.angle=lep-rep,
          lev=parabolicdiff(lep,15),
          rev=parabolicdiff(rep,15),
          verg.velocity=lev-rev,
          verg.type='Converging',
          verg.type=replace(verg.type,verg.velocity<0,'Diverging'),
          saccadic=!is.na(sacnum)
  ) %>%
  group_by(neuron,sacnum) %>%
  mutate(verg.amp=last(verg.angle)-first(verg.angle),
         peak.verg.velocity=maxabs(verg.velocity),
         peakFR=max(sdf20,na.rm=T))->
  z
       
##########THIS LINE TAKES HOURS TO RUN ############# it failed to finish after 17 hours  -- rejected
{
xx<- as.data.frame(rbindlist(apply(matrix(1:1999),1,bootfunc,z=z)))
saveRDS(xx,'BootstrapSimpleRevision.RDS')
}

#------TSBOOT
soa.ts.fun <- function(tsb,x) {
  fit <- lm(data=tsb,formula='sdf20~verg.angle+verg.velocity')
  return(as.numeric(coef(fit)[3])) }

#----NORMAL BOOT
soa.normal.fun <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
} 

# tb<-tsboot(t, inc.fun, R=999,sim="geom", l=1000)
# plot(tb)
# tbs<-tidy(tb)
# tbs<-mutate(tbs,low95=statistic-2*std.error,high95=statistic+2*std.error)

#---- 
z %>%
  filter(neuron=='Bee-101') %>%
  group_by(neuron) %>%
  do(tidy(tsboot(.,soa.fun,R=19,sim="geom",l=500))) ->
  xx

z %>%
  filter(neuron=='Bee-106') %>%
  group_by(neuron) %>%
  do(tb=tsboot(.,soa.fun,R=999,sim="geom",l=500)) ->
  mm


z %>%
  # filter(neuron=='Bee-111') %>%
  group_by(neuron) %>%
  do(tidy(boot.ci(tsboot(.,soa.ts.fun,R=9,sim="geom",l=500),type='basic')$basic[4:5])) ->
  mm

mm %>%
  group_by(neuron) %>%
  mutate(key=row_number(),
         key=replace(key,key==1,'low'),
         key=replace(key,key==2,'high')) %>%
  spread(key,'x') %>%
  mutate(zero.cross=low*high<0)->
  ms





xx %>% #give terms readable names - don't need this for simple model...
  mutate(term=replace(term,term=='verg.velocity:verg.typeConverging','Conv'),
         term=replace(term,term=='verg.velocity:verg.typeDiverging','Dive'))->
  xx

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

  