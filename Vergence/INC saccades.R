t %>%
  group_by(neuron)%>%
  mutate(time=row_number()) %>%
  filter(dsnum>0) %>% #saccades
  group_by(neuron,dsnum) %>%
  summarize(sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            sd.verg.velocity=sd(verg.velocity),
            spread=max(conj.velocity)-min(conj.velocity),
            qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
            dur=n(),
            R.H.Amp=last(rep)-first(rep),
            R.V.Amp=last(repV)-first(repV),
            L.H.Amp=last(lep)-first(lep),
            L.V.Amp=last(lepV)-first(lepV),
            conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
            conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
            peak.conj.velocity=maxabs(conj.velocity),
            nspk=sum(rasters),
            r.amp=sqrt(conj.H.Amp^2+conj.V.Amp^2),

            asleep=sd.conj.velocity>7.5 || dur>2000) %>%
  ungroup() %>%
  mutate(sacID=row_number())->
  zsp

t<- left_join(t,select(zsp,neuron,dsnum,sacID),by=c('neuron','dsnum'))


qplot(dur,sd.verg.velocity,data=zsp)

goodsacs=filter(zsp,R.V.Amp*L.V.Amp<0,r.amp>4,dur<200)$sacID

manipulate({
  d<- filter(t,sacID==goodsacs[chosenSac])
  ggplot(d)+
    geom_line(aes(time,conj.velocity))+
    geom_line(aes(time,rev),color='red')+
    geom_line(aes(time,lev),color='blue')+
    geom_line(aes(time,revV),color='red',linetype=2)+
    geom_line(aes(time,levV),color='blue',linetype=2)+
    geom_point(aes(time,rasters+100),data=filter(d,rasters>0),shape='|',size=4)
  # ggplot(d)+
    # geom_point(aes(rep,repV,alpha=sdf/300),color='red')+
    # geom_point(aes(lep,lepV,alpha=sdf/300),color='blue')+
    # geom_point(aes((lep+rep)/2,(lepV+repV)/2,alpha=sdf/300))+
    # xlim(c(-20,20))+
    # ylim(c(-20,20))
    
  },
  chosenSac=slider(1,length(goodsacs),step=1)
)



burst <- filter(zsp,dur<150)

burst %>%
  group_by(neuron) %>%
  summarize(cor.up=cor(conj.V.Amp[conj.V.Amp>0],nspk[conj.V.Amp>0]),
            cor.down=cor(conj.V.Amp[conj.V.Amp<0],nspk[conj.V.Amp<0]),
            cor.right=cor(conj.H.Amp[conj.H.Amp>0],nspk[conj.H.Amp>0]),
            cor.left=cor(conj.H.Amp[conj.H.Amp<0],nspk[conj.H.Amp<0]))%>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  bs
                         
burst %>%
  group_by(neuron) %>%
  do(tidy(lm(nspk~conj.V.Amp+conj.H.Amp,data=.))) %>%
  mutate(term=replace(term,term=='(Intercept)','b')) %>%
  select(term,estimate) %>%
  spread(term,estimate) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  blm

ggplot(blm)+
  geom_histogram(aes(conj.H.Amp,fill=monkey),position='dodge')
               
ggplot(blm)+
  geom_density(aes(conj.V.Amp,color=monkey))
