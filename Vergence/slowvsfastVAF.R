VAF<- function(mod,x,param='verg.velocity'){
  
  x<- mutate(x,predV=predict(mod,newdata=x))
  result=1-var(x$predV-x[param],na.rm=T)/var(x[param],na.rm=T)
  as.numeric(result)
}

compareVAF<- function(x){
  x<- mutate(x,sdf20=lag(sdf,20))
  x<- mutate(x,saccadic=markSaccades(conj.velocity,buffer=15,threshold=20)>0)
  modfast=lm(sdf20~verg.angle+verg.velocity,data=filter(x,saccadic))
  modslow=lm(sdf20~verg.angle+verg.velocity,data=filter(x,!saccadic))
  
  result<- data.frame(fast=VAF(modfast,filter(x,saccadic),'sdf20'),
                         slow=VAF(modslow,filter(x,!saccadic),'sdf20'))
  result
             
}



compareVAFsingle<- function(x){
  x<- mutate(x,sdf20=lag(sdf,20))
  x<- mutate(x,saccadic=markSaccades(conj.velocity,buffer=15,threshold=20)>0)
  # modfast=lm(sdf20~verg.angle+verg.velocity,data=filter(x,saccadic))
  # modslow=lm(sdf20~verg.angle+verg.velocity,data=filter(x,!saccadic))
  mod=lm(sdf20~verg.angle+verg.velocity,data=x)
  
  result<- data.frame(fast=VAF(mod,filter(x,saccadic),'sdf20'),
                      slow=VAF(mod,filter(x,!saccadic),'sdf20'),
                      all=VAF(mod,x,'sdf20'))
  result
  
}

compareVAFsinglenoconj<- function(x){
  x<- mutate(x,sdf20=lag(sdf,20))
  
  x %>%
    mutate(saccadic=markSaccades(conj.velocity,buffer=15,threshold=20)) %>%
    group_by(saccadic) %>%
    mutate(verg.amp=last(verg.angle)-first(verg.angle)) %>%
    ungroup() %>%
    mutate(saccadic=saccadic>0)->
    x
  mod=lm(sdf20~verg.angle+verg.velocity,data=filter(x,abs(verg.amp)>1 | !saccadic))
  
  result<- data.frame(fast=VAF(mod,filter(x,saccadic),'sdf20'),
                      slow=VAF(mod,filter(x,!saccadic),'sdf20'))
  result
  
}

compareVAFcomplex<- function(x){
  x<- mutate(x,sdf20=lag(sdf,20))
  x<- mutate(x,saccadic=markSaccades(conj.velocity,buffer=15,threshold=20)>0)
  # modfast=lm(sdf20~verg.angle+verg.velocity,data=filter(x,saccadic))
  # modslow=lm(sdf20~verg.angle+verg.velocity,data=filter(x,!saccadic))
  mod=lm(sdf20~verg.angle+verg.velocity:saccadic,data=x)
  
  result<- data.frame(complex=VAF(mod,x,'sdf20'))
  result
  
}

compareVAFvelocity<- function(x){
  x<- mutate(x,sdf20=lag(sdf,20))
  x<- mutate(x,saccadic=markSaccades(conj.velocity,buffer=4,threshold=30)>0)
  # modfast=lm(sdf20~verg.angle+verg.velocity,data=filter(x,saccadic))
  # modslow=lm(sdf20~verg.angle+verg.velocity,data=filter(x,!saccadic))
  mod=lm(verg.velocity~sdf20+verg.angle,data=filter(x,!saccadic))
  
  result<- data.frame(fast=VAF(mod,filter(x,saccadic),'verg.velocity'),
                      slow=VAF(mod,filter(x,!saccadic),'verg.velocity'),
                      all=VAF(mod,x,'verg.velocity'))
  result
  
}


t<- readRDS('SOA-NRTP.RDS')
t<- filter(t,cellnum>100,cellnum != 'Bee-109')
t %>%
  group_by(neuron) %>%
  do(compareVAF(.))->
  vafcomp

t %>%
  group_by(neuron) %>%
  do(compareVAFsingle(.))->
  vafcompsingle

vafcompsingle %>%
  ungroup() %>%
  summarize(fastmean=mean(fast),
            fastsd=sd(fast),
            slowmean=mean(slow),
            slowsd=sd(slow),
            allmean=mean(all),
            allsd=sd(all))->
  vafcompsinglesummary


x %>%
  mutate(saccadic=markSaccades(conj.velocity,buffer=15,threshold=20)) %>%
  group_by(saccadic) %>%
  mutate(verg.amp=last(verg.angle)-first(verg.angle)) %>%
  ungroup() %>%
  mutate(saccadic=saccadic>0)->
  x

t %>%
  group_by(neuron) %>%
  do(compareVAFsingle(.))->
  vafcompsinglenoconj

vafcompsinglenoconj %>%
  ungroup() %>%
  summarize(fastmean=mean(fast),fastsd=sd(fast),slowmean=mean(slow),slowsd=sd(slow))->
  vafcompsinglenoconjsummary

#complexmodel---
t %>%
  group_by(neuron) %>%
  do(compareVAFcomplex(.))->
  vafcomplex

vafcomplex %>%
  ungroup() %>%
  summarize(mean.complex=mean(complex),
            sd.complex=sd(complex))->
  vafcomplexsummary
