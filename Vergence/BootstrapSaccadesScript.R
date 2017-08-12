#Full Model----
bootstrapSaccades<- function(n){
  # z<- readRDS('bootstrapSaccades.RDS')
  get('z') #get from global environment
  # if (n %% 100 == 0){
    message(n)
  # }
  z %>%
    # group_by(verg.bins) %>%
    # filter(verg.bins != 'Conjugate') %>%
    sample_frac(1,replace=TRUE) %>%
    ungroup() %>%
    do(tidy(lm(sdf20~verg.angle+verg.velocity:enhance.type,data=.))) %>%
    mutate(repN=n)->
    zx
}


t<-readRDS('SOA.RDS')

nreps=1999
nbreps<- matrix(1:nreps)


# p<- filter(t,monkey %in% c('Bee','Ozette'),cellnum>100)
# p<- filter(t,neuron=='Bee-211')
neurons=unique(t$neuron)
# p<-NULL
xx<- NULL
for (i in 1:length(neurons)){
# for(i in 1:2){
  message(paste('Processing: ',neurons[i]))
  z <- filter(t,neuron==neurons[i])
  z<-preparetoBOOT2(z)
  z %>%
    group_by(verg.bins) %>%
    filter(verg.bins != 'Conjugate',
           abs(verg.velocity)<350) ->
    z
  x<- as.data.frame(rbindlist(apply(nbreps,1,bootstrapSaccades)))
  x<- mutate(x,neuron=neurons[i])
  xx[[i]]<- x
}

xx<- rbindlist(xx)
saveRDS(xx,'Bootstrap1999-6-15-OzetteWeirdos.RDS')


# 
# xx %>%
#   mutate(term=replace(term,term=='verg.velocity:enhance.typeconverging','ConvergenceE'),
#          term=replace(term,term=='verg.velocity:enhance.typediverging','DivergenceE'),
#          term=replace(term,term=='verg.velocity:enhance.typenone','Slow'))->
#   xx
# xx<- separate(xx,neuron,c('monkey','cellnum'),remove=FALSE)
# 
# saveRDS(xx,paste('Bootstrap',nreps,'.RDS',sep=''))
# 
# qplot(estimate,data=filter(xx,term != '(Intercept)',term != 'verg.angle'),bins=10)+
#   facet_grid(term~neuron,scales='free')
# 
# xxp<- filter(xx,term != '(Intercept)',
#                             term != 'verg.angle',
#                             as.numeric(cellnum)>100,
#                             monkey %in% c('Bee','Ozette'))
# 
# qplot(estimate,data=filter(xxp,monkey=='Bee'),bins=50)+
#   facet_grid(term~neuron,scales='free')
# 
# 
# qplot(estimate,data=filter(xxp,monkey=='Ozette'),bins=50)+
#   facet_grid(term~neuron,scales='free')
# 
# 
# xxp %>%
#   # group_by(verg.bins,term) %>%
#   group_by(neuron,term) %>%
#   summarize(e=mean(estimate),
#             stdev=sd(estimate),
#             ci=2*stdev/sqrt(nreps))->
#   xs
# 
# xxa<- filter(xx,term == 'verg.angle',
#              as.numeric(cellnum)>100,
#              monkey %in% c('Bee','Ozette'))
# qplot(estimate,data=filter(xxa,monkey=='Ozette'),bins=50)+
#   facet_wrap(~neuron,ncol=1,scales='free')
# 
# xxv<- filter(xx,term == 'Slow',
#              as.numeric(cellnum)>100,
#              monkey %in% c('Bee','Ozette'))
# 
# qplot(estimate,data=filter(xxv,monkey=='Ozette'),bins=50)+
#   facet_wrap(~neuron,ncol=1,scales='free')

#simpler model----

bootstrapSaccadesSIMPLE<- function(n){
  #This function does not work as intended
  #It does not sample by saccade, it treats each point independently 
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
    do(tidy(lm(sdf20~verg.angle+verg.velocity,data=.))) %>%
    mutate(repN=n)->
    zx
}

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
    do(tidy(lm(sdf20~verg.angle+verg.velocity,data=.))) %>%
    mutate(repN=n)-> #add the number of the bootstrap iteration. 
    z
}
source('preparetoBOOT.R')
#step 1 is to run the first two chunks from SOArevision.Rmd
#that just loads the necessary libraries, helper functions and the main data file: ('SOA-NRTP.RDS')
nreps=1999 #number of bootstrap iterations
n<- matrix(1:nreps) #set this up in the proper form to work with apply below
t<- readRDS('SOA.RDS')

neurons=unique(t$neuron)

xx<- NULL
for (i in 1:length(neurons)){
  # for(i in 1:2){
  message(paste('Processing: ',neurons[i]))
  z <- filter(t,neuron==neurons[i])
  z<-preparetoBOOT(z) #marks and measures saccades
  x<- as.data.frame(rbindlist(apply(n,1,bootstrapSaccadesOK))) #calls the function n times
  x<- mutate(x,neuron=neurons[i]) 
  xx[[i]]<- x #add to list for combining in next phase
}

xx<- rbindlist(xx) #combine efficiently (from data.table)

xx %>% #give terms readable names - don't need this for simple model...
  mutate(term=replace(term,term=='verg.velocity:enhance.typeconverging','ConvergenceE'),
         term=replace(term,term=='verg.velocity:enhance.typediverging','DivergenceE'),
         term=replace(term,term=='verg.velocity:enhance.typenone','Slow'))->
  xx
xx<- separate(xx,neuron,c('monkey','cellnum'),remove=FALSE) #get monkey for plotting


# qplot(estimate,data=filter(xx,term=='verg.velocity'),binwidth=0.001)+
#   facet_wrap(~neuron,scales='free',ncol=1)+
#   annotate('segment',x=0,y=0,xend=0,yend=20)+
#   xlab('Sensitivity to vergence velocity during\nsaccade-vergence (spks/s/deg/s')
# 
# qplot(estimate,data=filter(xx,term=='verg.angle'),binwidth=0.001)+
#   facet_wrap(~neuron,scales='free',ncol=1)+
#   annotate('segment',x=0,y=0,xend=0,yend=20)+
#   xlab('Sensitivity to vergence position during\nsaccade-vergence (spks/s/deg')

xx %>% 
  select(neuron,term,estimate) %>%
  group_by(neuron,term) %>%
  summarize(e=mean(estimate),
            stdev=sd(estimate),
            ci=2*stdev/sqrt(nreps))->
  xs

xs %>%
  filter(term=='verg.velocity') %>%
  select(-term) %>%
  rename(verg.velocity.estimate=e) %>%
  arrange(abs(verg.velocity.estimate)) %>%
  mutate(zero.cross=abs(verg.velocity.estimate)<ci)%>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  xp

xp %>% group_by(monkey) %>%
  summarize(m=mean(verg.velocity.estimate),
            ma=mean(abs(verg.velocity.estimate)),
            masd=sd(abs(verg.velocity.estimate)))->
  av

xs %>%
  filter(term=='(Intercept)') %>%
  select(-term) %>%
  rename(intercept.estimate=e) %>%
  arrange(abs(intercept.estimate)) %>%
  mutate(zero.cross=abs(intercept.estimate)<ci)%>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  xpi

xpi %>% group_by(monkey) %>%
  summarize(m=mean(intercept.estimate),
            ma=mean(abs(intercept.estimate)),
            masd=sd(abs(intercept.estimate)))->
  ai

xs %>%
  filter(term=='verg.angle') %>%
  select(-term) %>%
  rename(verg.angle.estimate=e) %>%
  arrange(abs(verg.angle.estimate)) %>%
  mutate(zero.cross=abs(verg.angle.estimate)<ci) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  xpa

xpa %>% group_by(monkey) %>%
  summarize(m=mean(verg.angle.estimate),
            ma=mean(abs(verg.angle.estimate)),
            masd=sd(abs(verg.angle.estimate)))->
  as
  
saveRDS(x,'Bootstrap1999SaccadesRaw.RDS')
write.csv(xp,'Bootstrap1999SaccadesSimple.csv')



