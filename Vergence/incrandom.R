
tv<- filter(ts,conj.v.amp>5,abs(peak.V.vel)>200,neuron=='Kopachuck-914')
tv<- mutate(tv,showrasters=replace(rasters,rasters<1,NA))

goodsacs<- unique(tv$sacnum)     
nsac=length(goodsacs)


manipulate(ggplot(filter(tv,sacnum==goodsacs[currentsac]))+
             geom_line(aes(counter,sdf20,group=sacnum))+
             geom_line(aes(counter,(levV+revV)/20),color='magenta')+
             geom_line(aes(counter,(lepV+repV)/2),color='orange')+
             geom_point(aes(counter,showrasters+50),shape='|')+
             facet_wrap(~neuron,scales='free')
           ,
           currentsac=slider(1,nsac,step=1))

currentsac=1

ggplot(filter(tv,sacnum==currentsac))+
  geom_line(aes(counter,sdf20,group=sacnum))+
  geom_line(aes(counter,levV+revV)/2),color='magenta')+
  facet_wrap(~neuron)
#-------------BOOT.CI
bootci <- function(t,n=100,alpha=0.05,formula='lagsdf~rev+lev'){

  ta %>%
    bootstrap(n) %>%
    do(tidy(lm(formula,.)))%>%
    group_by(term) %>%
    summarize(
      low=quantile(estimate, alpha / 2),
      high=quantile(estimate, 1 - alpha / 2)) ->
    ci
  
  return(ci)
}

formula=  'sdf20~rep+lep+rev+lev'
n=199
alpha=0.05

results<-boot(data=ta,)


#--------

bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
results <- boot::boot(data=ta, statistic=bs,
                R=1000, formula=sdf20~repV+lepV)


#------TSBOOT
inc.fun <- function(tsb) {
       fit <- lm(data=tsb,formula='sdf20~repV+lepV+rep+lep')
       return(coef(fit)) }

tb<-tsboot(t, inc.fun, R=999,sim="geom", l=1000)
plot(tb)
tbs<-tidy(tb)
tbs<-mutate(tbs,low95=statistic-2*std.error,high95=statistic+2*std.error)

#---- 
t %>%
  group_by(neuron) %>%
  do(tidy(tsboot(.,inc.fun,R=999,sim="geom",l=1000))) %>%
  mutate(low95=statistic-2*std.error,high95=statistic+2*std.error)->
  tsba

tsba %>%
  mutate(zerocross=low95*high95<0)->
  tsba

tsba %>%
  dplyr::select(neuron,term,statistic) %>%
  spread(term,statistic)->
  tplot


tsba %>%
  dplyr::select(neuron,term,low95) %>%
  spread(term,low95) %>%
  rename(rep.low=rep,repV.low=repV,lep.low=lep,lepV.low=lepV)->
  tlow



tsba %>%
  dplyr::select(neuron,term,high95) %>%
  spread(term,high95) %>%
  rename(rep.high=rep,repV.high=repV,lep.high=lep,lepV.high=lepV)->
  thigh

tplot<- left_join(tplot,tlow,by='neuron')
tplot<- left_join(tplot,thigh,by='neuron')

ggplot(tplot,aes(rep,repV))+
  geom_point()+
  geom_errorbar(aes(ymin=repV.low,ymax=repV.high))+
  geom_errorbarh(aes(xmin=rep.low,xmax=rep.high))+
  geom_abline()


