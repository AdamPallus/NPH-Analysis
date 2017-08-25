
bootstrapci<-function(xx){
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
  ciplot
}

xx<- readRDS('SOA simple Bootstrap analysis 1999reps.RDS')
simplebootstrap<- bootstrapci(xx)

table1<- select(simplebootstrap,1:4)
names(table1)<- c('neuron','b','VG','VGdotS','VGdotN')
table2p<- gather(table2,term,value,-neuron)
qplot(term,abs(value),data=table2p,geom='boxplot')+facet_wrap(~term,scales='free')

xx<- readRDS('SOA saccadic Bootstrap 1999reps.RDS')
complexbootstrap<- bootstrapci(xx)

table2<- select(complexbootstrap,1:5)
names(table2)<- c('neuron','b','VG','VGdotN','VGdotS')
table2p<- gather(table2,term,value,-neuron)
qplot(term,abs(value),data=table2p,geom='boxplot')+facet_wrap(~term,scales='free')

qplot(term,abs(value),data=filter(table2p, term %in% c('VGdotN','VGdotS')),
                                  geom='boxplot',group=term)
