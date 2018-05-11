
#'Plan: We wish to use non-parametric bootstrapping to determine if there is a significant relationship
#'between the firing rate of the neuron and the horizontal and vertical positions of the eyes
#'during fixation. We will need to test each eye and the cyclopean eye separately, in addition to testing
#'horizontal and vertical separately. 
#'
#'The initial plan is to use the boot package since we are working with summary data, unlike the previous
#'bootstrapping I've done where I wrote custom scripts to randomly select entire saccades.
#'
#'
#'Output: 
#'We want to know for each cell what the 95% confidence interval is for the horizontal and vertical 
#'sensitivity of the neuron. This will be for left eye, right eye and cyclopean, both horizontal and 
#'vertical.
#'
#'Neuron | Left_CI | Right_CI | Cyclopean_CI |
#'-------- H  | V  |  H | V   |  H | V       |
#'
#'
#'second plan:
#'intercept | horizontal slope | vertical slope | replication number| modeltype 
#'
#'modeltype is left right or cylopean. At the end we'll have a data frame with 5997 rows
#'
#'-----------------------------------------------------------------------------------------
#'


getObservedcoef<-function(z){
  modR <- lm(meanFR ~ mean.R.H+mean.R.V,data=z)
  modL <- lm(meanFR ~ mean.L.H+mean.L.V,data=z)
  modRL<- lm(meanFR ~ mean.H+mean.V,data=z)
  rn<- c('right','left','cyclopean')
  dt<-as_tibble(rbind(as.numeric(coef(modR)),
                      as.numeric(coef(modL)),
                      as.numeric(coef(modRL))))
  names(dt)<-c('intercept_o','horizontal_o','vertical_o')
  dt$type=rn
  dt 
}

bootstrapDirPref<-function(zboot,nreps){
  
  getcoef<-function(n,z){
    z <- sample_frac(z,size= 1,replace=TRUE)
    modR <- lm(meanFR ~ mean.R.H+mean.R.V,data=z)
    modL <- lm(meanFR ~ mean.L.H+mean.L.V,data=z)
    modRL<- lm(meanFR ~ mean.H+mean.V,data=z)
    rn<- c('right','left','cyclopean')
    dt<-as_tibble(rbind(as.numeric(coef(modR)),
                        as.numeric(coef(modL)),
                        as.numeric(coef(modRL))))
    names(dt)<-c('intercept','horizontal','vertical')
    dt$type=rn
    dt$repN=n
    dt 
  }
  
  n<- matrix(1:nreps)
  neuronName<- zboot$neuron[1]
  x<- as.data.frame(rbindlist(lapply(n,getcoef,zboot))) #calls the function n times
  x<- mutate(x,neuron=neuronName)
}

zp %>%
  group_by(neuron) %>%
  do(bootstrapDirPref(.,nreps=1999)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zb

zp %>%
  group_by(neuron) %>%
  do(getObservedcoef(.)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zo

zo<- left_join(zo,zci)

zb %>%
  group_by(neuron,type) %>%
  summarize(lowH=quantile(horizontal,probs=0.025),
            lowV=quantile(vertical,probs=0.025),
            highH=quantile(horizontal,probs=0.975),
            highV=quantile(vertical,probs=0.975)) %>%
  mutate(zerocrossH=lowH*highH < 0,
         zerocrossV=lowV*highV < 0)->
  zci

filter(zci,!zerocrossH & !zerocrossV,type=='right')

unique(filter(zci,!zerocrossH & !zerocrossV,type=='cyclopean')$neuron)

#'TESTING

ggplot(zb)+
  geom_density(aes(abs(vertical),color=neuron))+
  facet_wrap(~monkey,ncol=1)+
  theme(legend.position='none')

ggplot(zb)+
  geom_density(aes(abs(horizontal),color=neuron))+
  facet_wrap(~monkey,ncol=1)+
  theme(legend.position='none')

#plots




ggplot(zo)+
  geom_point(aes(horizontal_o,vertical_o,color=type))+
  coord_fixed()+
  geom_abline()

ggplot(zo %>% filter(type=='right'))+
  geom_point(aes(abs(horizontal_o),abs(vertical_o),color=monkey))+
  coord_fixed()+
  geom_abline()
  
ggplot(zo,aes(horizontal_o,vertical_o))+
  geom_errorbar(aes(ymin=lowV,ymax=highV),size=0.5,width=0,alpha=0.3)+
  geom_errorbarh(aes(xmin=lowH,xmax=highH),height=0,size=0.5,alpha=0.3)+
  geom_point(color='hotpink')



ggplot(zo %>% filter(!zerocrossH,!zerocrossV),aes(abs(horizontal_o),abs(vertical_o)))+
  geom_errorbar(aes(ymin=abs(lowV),ymax=abs(highV)),size=0.5,width=0,alpha=0.3)+
  geom_errorbarh(aes(xmin=abs(lowH),xmax=abs(highH)),height=0,size=0.5,alpha=0.3)+
  geom_point(aes(color=monkey),size=2)+
  geom_abline(size=2,linetype=2,alpha=0.2)+
  coord_fixed()+
  facet_wrap(~type,ncol=3)+
  ylim(c(NA,3.9))+
  theme_minimal()+
  xlab('Sensitivity to Horizontal Eye Position (deg/s)')+
  ylab('Sensitivfity to Vertical Eye Position (deg/s)')

ggplot(zo,aes(abs(horizontal_o),abs(vertical_o)))+
  geom_point(aes(color=monkey),size=2)+
  geom_abline(size=2,linetype=2,alpha=0.2)+
  coord_fixed()+
  facet_wrap(~type,ncol=3)+
  ylim(c(NA,3.9))+
  theme_minimal()+
  xlab('Sensitivity to Horizontal Eye Position (deg/s)')+
  ylab('Sensitivfity to Vertical Eye Position (deg/s)')



ggplot(zo %>% filter(!zerocrossV),aes(abs(horizontal_o),abs(vertical_o)))+
  geom_errorbar(aes(ymin=abs(lowV),ymax=abs(highV)),size=0.5,width=0,alpha=0.3)+
  geom_errorbarh(aes(xmin=lowH*sign(horizontal_o),xmax=highH*sign(horizontal_o)),
                 height=0,size=0.5,alpha=0.3)+
  geom_point(aes(color=monkey),size=2)+
  geom_abline(size=2,linetype=2,alpha=0.2)+
  coord_fixed()+
  facet_wrap(~type,ncol=3)+
  ylim(c(NA,3.9))+
  theme_minimal()+
  xlab('Sensitivity to Horizontal Eye Position (deg/s)')+
  ylab('Sensitivfity to Vertical Eye Position (deg/s)')


ggplot(zo,aes(abs(horizontal_o),abs(vertical_o)))+
  geom_errorbar(aes(ymin=lowV*sign(vertical_o),ymax=highV*sign(vertical_o)),
                 width=0,size=0.5,alpha=0.3)+
  geom_errorbarh(aes(xmin=lowH*sign(horizontal_o),xmax=highH*sign(horizontal_o)),
                 height=0,size=0.5,alpha=0.3)+
  geom_point(aes(color=monkey),size=2)+
  geom_abline(size=2,linetype=2,alpha=0.2)+
  coord_fixed()+
  facet_wrap(~type,ncol=3)+
  ylim(c(NA,3.9))+
  theme_minimal()+
  xlab('Sensitivity to Horizontal Eye Position (deg/s)')+
  ylab('Sensitivfity to Vertical Eye Position (deg/s)')+
  theme(legend.position='bottom')
