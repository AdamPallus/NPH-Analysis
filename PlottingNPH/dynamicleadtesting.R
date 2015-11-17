#script to figure out the "dynamic lead time" of each neuron

dynamiclead<-function(p,lags=seq(10,200,by=10)) {

rsq<-NULL
for (i in 1:length(lags)) {
  if (lags[i] > 0){
  p$sdflag<-dplyr::lag(p$sdf,lags[i])
  }
  else{
    p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
  }
    
  rsq[i]<- summary(lm(sdflag~rep+lep+repV+lepV,data=p))$r.squared
}
#return(rsq)
return(lags[rsq==max(rsq)])
}

tt %>%
  filter(maxamp>10,!disjEither)->
  ttt
ttt%>%
  group_by(neuron) %>%
  do(leadtime=dynamiclead(.)) %>%
  separate(neuron,c("Monkey","cellnum"),remove =FALSE) %>%
  mutate(leadtime=as.numeric(leadtime), cellnum=as.numeric(cellnum))->
  dltest
qplot(cellnum,leadtime,data=dltest,binwidth=10)+facet_grid(Monkey~.)


t %>%
  group_by(neuron) %>%
  mutate(sdflag=lag(sdf,as.numeric(dltest$leadtime[dltest$leadtime==neuron[1]]))) ->
  tx