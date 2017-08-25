bootstrapSaccadesOK<- function(n){
  #n is just a label that is added. This function is run multiple times
  #using different random samples each time 
  get('z') #get from global environment
  #randomly pick from the list of saccades of each type. Replace=true means that
  #we can get the same number multiple times
  samp <- sample(unique(z$sacnum[z$verg.bins=='Converging']), 
                 length(unique(z$sacnum[z$verg.bins=='Converging'])), replace = TRUE)
  
  samp <- c(samp,sample(unique(z$sacnum[z$verg.bins=='Diverging']), 
                        length(unique(z$sacnum[z$verg.bins=='Diverging'])), replace = TRUE))
  #convert to data.table for faster processing
  z <- as.data.table(z)
  setkey(z, "sacnum") #like group_by
  # create the new data set
  z <- z[J(samp), allow.cartesian = TRUE] #replicate data set based on above sample 
  
  z %>% #make the model
    # filter(verg.velocity>0) %>%
    do(tidy(lm(sdf20~verg.angle+verg.velocity:realsaccade,data=.))) %>%
    # do(tidy(lm(sdf20~lep+rep+lev+rev,data=.))) %>%
    mutate(repN=n)-> #add the number of the bootstrap iteration. 
    z
}
source('preparetoBOOT.R')
source('preparetoBOOTmarksaccs.R')
#step 1 is to run the first two chunks from SOArevision.Rmd
#that just loads the necessary libraries, helper functions and the main data file: ('SOA-NRTP.RDS')
nreps=1999 #number of bootstrap iterations
n<- matrix(1:nreps) #set this up in the proper form to work with apply below
# t<- readRDS('SOA.RDS')

neurons=unique(t$neuron)

xx<- NULL
for (i in 1:length(neurons)){
  # for(i in 1:2){
  message(paste('Processing: ',neurons[i]))
  z <- filter(t,neuron==neurons[i])
  z<-preparetoBOOTmarksaccs(z) #marks and measures saccades
  if (neurons[i]=='Ozette-123'){
    coilnoise<-c(65000:71000,
                 80500:85000,
                 95500:98500,
                 101000:105500,
                 151500:156000,
                 187000:191500,
                 226000:230500,
                 242000:nrow(z))
    z<-filter(z,!time%in%coilnoise)
  }
  x<- as.data.frame(rbindlist(apply(n,1,bootstrapSaccadesOK))) #calls the function n times
  x<- mutate(x,neuron=neurons[i]) 
  xx[[i]]<- x #add to list for combining in next phase
}

xx<- rbindlist(xx) #combine efficiently (from data.table)
if (nreps==1999){
  saveRDS(xx,'SOA saccadic Bootstrap 1999reps.RDS')
}

#makeplots----
xx<-readRDS('SOA saccadic Bootstrap 1999reps.RDS') #8-22-2017

#remove Bee-109
xx<- filter(xx,neuron!='Bee-109')


xx %>%
  mutate(term=replace(term,term=='(Intercept)','b'),
         term=replace(term,term=='verg.velocity:realsaccadeFALSE','slow'),
         term=replace(term,term=='verg.velocity:realsaccadeTRUE','fast'))->
  xx

xx %>%
  group_by(neuron,term) %>%
  summarize(ciLOW=quantile(estimate,probs=0.025),
            ciHIGH=quantile(estimate,probs=0.975),
            m=median(estimate)) %>%
  mutate(zerocross=(ciLOW*ciHIGH)<0)->
  confints

confints %>%
  select(neuron,term,m) %>%
  spread(term,m)->
  ciplot
# names(ciplot)<- c('neuron','b','verg.angle','fastC','fastD','slowC','slowD')

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

ciplot %>%
  group_by(neuron) %>%
  mutate(overlap=max(slow.low,fast.low)<=min(slow.high,fast.high))->
  ciplot

write.csv(ciplot,'BootstrapSaccadesComplexModel.csv')
# ggplot(ciplot,aes(slow,fast))+
#   geom_errorbar(aes(ymin=fast.low,ymax=fast.high),size=0.25,width=0)+
#   geom_errorbarh(aes(xmin=slow.low,xmax=slow.high),height=0,size=0.25,alpha=0.5)+
#   geom_point(size=1,color='hotpink')+
#   geom_vline(xintercept = 0)+
#   coord_fixed()+
#   # geom_text(aes(label=neuron))+
#   theme_minimal()+
#   geom_abline()

ggplot(ciplot,aes(fast,slow))+
  geom_errorbarh(aes(xmin=fast.low,xmax=fast.high),size=0.25,alpha=0.5)+
  geom_errorbar(aes(ymin=slow.low,ymax=slow.high),size=0.25,alpha=0.5)+
  geom_point(size=1,color='red')+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()


ggplot(filter(ciplot,overlap),aes(fast,slow))+
  geom_errorbarh(aes(xmin=fast.low,xmax=fast.high),size=0.25,alpha=0.5)+
  geom_errorbar(aes(ymin=slow.low,ymax=slow.high),size=0.25,alpha=0.5)+
  geom_point(size=1,color='red')+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()

ggplot(filter(ciplot,overlap),aes(fast,slow))+
  # geom_linerange(aes(xmin=fast.low,xmax=fast.high))+
  # geom_linerange(aes(ymin=slow.low,ymax=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.high,yend=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.low,yend=slow.low))+
  geom_segment(aes(y=slow.low,yend=slow.high,x=fast.high,xend=fast.high))+
  geom_segment(aes(y=slow.low,yend=slow.high,x=fast.low,xend=fast.low))+
  geom_point(size=1,color='red')+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()

ggplot(ciplot,aes(fast,slow))+
  # geom_linerange(aes(xmin=fast.low,xmax=fast.high))+
  # geom_linerange(aes(ymin=slow.low,ymax=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.high,yend=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.low,yend=slow.low))+
  geom_segment(aes(y=slow.low,yend=slow.high,x=fast.high,xend=fast.high))+
  geom_segment(aes(y=slow.low,yend=slow.high,x=fast.low,xend=fast.low))+
  geom_point(size=1,color='red')+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()

ggplot(filter(ciplot,!overlap),aes(fast,slow))+
  # geom_linerange(aes(xmin=fast.low,xmax=fast.high))+
  # geom_linerange(aes(ymin=slow.low,ymax=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.high,yend=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.low,yend=slow.low))+
  geom_segment(aes(y=slow.low,yend=slow.high,x=fast.high,xend=fast.high))+
  geom_segment(aes(y=slow.low,yend=slow.high,x=fast.low,xend=fast.low))+
  geom_point(size=1,color='red')+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()


ggplot(ciplot,aes(fast,slow))+
  # geom_linerange(aes(xmin=fast.low,xmax=fast.high))+
  # geom_linerange(aes(ymin=slow.low,ymax=slow.high))+
  geom_rect(aes(xmin=fast.low,xmax=fast.high,ymin=slow.low,ymax=slow.high),alpha=0.2,
            data=filter(ciplot,!overlap))+
  geom_rect(aes(xmin=fast.low,xmax=fast.high,ymin=slow.low,ymax=slow.high),alpha=0.2,
            fill='orange',
            data=filter(ciplot,overlap))+
  # geom_point(size=1,color='red',data=filter(ciplot,!overlap))+
  # geom_point(size=1,color='black',data=filter(ciplot,overlap))+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()

ggplot(filter(ciplot,overlap),aes(fast,slow))+
  # geom_linerange(aes(xmin=fast.low,xmax=fast.high))+
  # geom_linerange(aes(ymin=slow.low,ymax=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.low,yend=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.high,yend=slow.low))+

  # geom_point(size=1,color='red')+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()

ggplot(ciplot,aes((fast.low+fast.high)/2,(slow.low+slow.high)/2))+
  # geom_linerange(aes(xmin=fast.low,xmax=fast.high))+
  # geom_linerange(aes(ymin=slow.low,ymax=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.low,yend=slow.high),
               data=filter(ciplot,!overlap),alpha=0.3)+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.high,yend=slow.low),
               data=filter(ciplot,!overlap),alpha=0.3)+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.low,yend=slow.high),
               color='red',data=filter(ciplot,overlap),alpha=0.3)+
  geom_segment(aes(x=fast.low,xend=fast.high,y=slow.high,yend=slow.low),
               color='red',data=filter(ciplot,overlap),alpha=0.3)+
  geom_point(size=1,color='black',data=filter(ciplot,!overlap))+
  geom_point(size=1,color='red',data=filter(ciplot,overlap))+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()

ggplot(ciplot,aes(fast,slow))+
  # geom_linerange(aes(xmin=fast.low,xmax=fast.high))+
  # geom_linerange(aes(ymin=slow.low,ymax=slow.high))+
  geom_segment(aes(x=fast.low,xend=fast.high,yend=slow),
               data=filter(ciplot,!overlap),alpha=0.3)+
  geom_segment(aes(xend=fast,y=slow.high,yend=slow.low),
               data=filter(ciplot,!overlap),alpha=0.3)+
  geom_segment(aes(x=fast.low,xend=fast.high,yend=slow),
               data=filter(ciplot,overlap),alpha=0.3,color='red')+
  geom_segment(aes(xend=fast,y=slow.high,yend=slow.low),
               data=filter(ciplot,overlap),alpha=0.3,color='red')+
  geom_point(size=1,color='black',data=filter(ciplot,!overlap))+
  geom_point(size=1,color='red',data=filter(ciplot,overlap))+
  geom_vline(xintercept = 0)+
  # coord_fixed()+
  # geom_text(aes(label=neuron))+
  theme_minimal()+
  geom_abline()+
  xlab('Sensitivity to Saccadic Vergence Velocity\n(spk/s)/(deg/s)')+
  ylab('Sensitivity to Non-Saccadic Vergence Velocity\n(spk/s)/(deg/s)')



ggplot(filter(xx,term %in% c('slow','fast')))+
  geom_density(aes(estimate,color=term))+
  facet_wrap(~neuron,scales='free')

#Bootstrap Histogram----
x<- filter(xx,neuron=='Ozette-123')

ciplot %>%
  filter(neuron=='Ozette-123') %>%
  select(neuron,fast.low,fast.high,slow.low,slow.high) ->
  bootplot

qplot(data=filter(x,term %in% c('fast','slow')),
      estimate,binwidth=0.005,fill=term)
x$term<- as.factor(x$term)
levels(x$term)<-c('b','Saccadic Vergence','Non-Saccadic Vergence','verg.angle')

#get ciplot from next section
ggplot(filter(x,term %in% c('Saccadic Vergence','Non-Saccadic Vergence')))+
  geom_histogram(aes(estimate,fill=term),
                 binwidth=0.025,
                 color='white',
                 size=0.5)+
  theme_minimal()+
  xlab('Sensitivity to Vergence Velocity (spk/s)/(deg/s)')+
  ylab('Number of Bootstrap Iterations')+
  geom_segment(aes(x=slow.low,xend=slow.high),data=bootplot,
               y=-10,yend=-10,size=3,color='#00BFC4')+
  geom_segment(aes(x=fast.low,xend=fast.high),data=bootplot,
               y=-10,yend=-10,size=3,color='#F8766D')+
  theme(legend.position=c(0.5,0.5),
        legend.title=element_blank())

#SensitivityBoxplot----
qplot(term,estimate,data=filter(xx,term %in% c('slow','fast')),geom='boxplot')

ciplot %>%
  ungroup() %>%
  select(fast,slow) %>%
  gather(term,estimate)->
  boxplotdata

qplot(term,abs(estimate),data=boxplotdata,geom='boxplot')+
  theme_minimal()

#calculateOverlap----

#found this from stackOverflow -- translate to dplyr and omit sorting since we already did that
# ciplot$overlap<- apply(select(ciplot,slow.low,slow.high,fast.low,fast.high),1,
#                        function(x){y<-c(sort(x[1:2]),sort(x[3:4]));
#                        max(y[c(1,3)])<=min(y[c(2,4)])})

ciplot %>%
  group_by(neuron) %>%
  mutate(overlap=max(slow.low,fast.low)<=min(slow.high,fast.high))->
  ciplot




