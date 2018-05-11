#'This is a library of functions for evaluating the direction preference of burst-tonic cells
#'The first section assesses the fixations
#'The second section assesses the saccades.
#'


cleanFixations<-function(t){
  #'Some of the data were recorded using an SMI visual eye tracker
  #'This means that when the monkey's eye closes, we lose the signal
  #'The data files do not have NA values, so it instead inserts a static position signal
  #'The value that it reports when the signal is lost might not be constant
  #'
  #'The function as currently written looks for periods where the right eye position is over 70 
  #'Those are considered blinks. We then identify saccades, making sure not to include any of the
  #'blink periods in the analysis.
  t%>%
    mutate(blinks=markSaccades(rep,buffer=80,threshold=70),
           #we put an 80ms buffer around each detected blink
           #then we force the velocity to be above threshold for the detected blinks
           #This causes all blinks to be marked as saccades, which means they are not
           #included in any fixation analyses
           #When analyzing blinks, remember to omit saccades where the eye postions are extreme
           dsnum=markSaccadesDouble(replace(conj.velocity,blinks>0,150),
                                    threshold1=30,threshold2=15,maxreject=100000),
           sdf=spikedensity(rasters,10),
           sdf10=lag(sdf,10),
           conj.vert=(repV+lepV)/2,
           conj.vert.vel=(revV+levV)/2,
           conj.hor=(rep+lep)/2,
           conj.hor.v=(rev+lev)/2) %>%
    select(-blinks)->
    t
  
}

expandSaccades<- function(tt,buffer=80){
  #this little function adds a new grouping variable
  #so that we can analyze what happens after saccades
  #without losing our ability to just grab the saccades with group_by
  #This way we can group_by(dsnum) to get just the saccades
  #or group_by(dsnum_extended) to get the saccade plus some period after 
  
  maxtime<- max(tt$time)
  tt %>%
    filter(dsnum>0) %>%
    group_by(dsnum) %>%
    summarize(saccade.end=last(time),
              saccade.start=first(time)) %>%
    filter(saccade.end+buffer<maxtime) %>%
    mutate(saccade.end=saccade.end+80)-> 
    tcount
  
  tcount$saccade.end<-tcount$saccade.end+buffer
  
  jsac<- function(stimes){
    df<- data.frame(time=stimes[['saccade.start']]:stimes[['saccade.end']],
                    dsnum_extended=stimes[['dsnum']])
    return(df)
  }
  
  x<-rbindlist(apply(tcount,1,jsac))
  
  tt<- left_join(tt,x,by='time')
  
}


measureFixations<-function(t) {
  
t %>%
  group_by(neuron)%>%
  mutate(time=row_number()) %>%
  filter(dsnum<0) %>% #fixations only
  group_by(neuron,dsnum) %>%
  summarize(sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            spread=max(conj.velocity)-min(conj.velocity),
            qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
            peak.conj.velocity=maxabs(conj.velocity),
            right.eye.distance=mean(sqrt((rep-thp)^2+(repV-tvp)^2)),
            left.eye.distance =mean(sqrt((lep-thp2)^2+(lepV-tvp2)^2)),
            right.eye.viewing=right.eye.distance<left.eye.distance & right.eye.distance<8,
            left.eye.viewing=left.eye.distance<right.eye.distance & left.eye.distance<8,
            dur=n(),
            starttime=first(time),
            c2eyes=cor(repV,lepV),
            meanFR=sum(rasters)/dur*1000,
            mean.V=mean(conj.vert),
            mean.H=mean(conj.hor),
            mean.R.H=mean(rep),
            mean.L.H=mean(lep),
            mean.R.V=mean(repV),
            mean.L.V=mean(lepV),
            mean.T.V=mean(tvp),
            mean.T.H=mean(thp),
            mean.verg.angle=mean(lep-rep),
            sd.sdf=sd(sdf10),
            asleep=sd.conj.velocity>7.5 || dur>2000)->
  zp
}




calculateDirPrefFixations<-function(x){
  #'The problem with this analysis is that there are just TOO MANY WAYS to assess direction preference
  #'In the code below, I calculate direction preference using the slopes and the relative importance
  #'and I do it separtely for left-eye-only, right-eye-only and the conjugate (mean) eye positions too
  #'
  #'I also calculate the statistics and try to mark things as significant or non siginificant. 
  #'I also came up with two differnt ways of calculating the direction preference based on slopes
  #'One where I just use the raw number from the multiple regression and another where I replace
  #'non-significant slopes with 0. 
  #'
  #
  #Remove bad fixations that are too short or are outside of the normal range of eye movements
  x<- filter(x,dur>200,abs(mean.R.H)<50,abs(mean.L.H)<50)
  
  #calculate linear regression models on horizontal, vertical and combined
  #calculate each eye separately 
  mRH<-lm(meanFR~mean.R.H,data=x)
  mLH<-lm(meanFR~mean.L.H,data=x)
  mRV<-lm(meanFR~mean.R.V,data=x)
  mLV<-lm(meanFR~mean.L.V,data=x)
  mRHV<-lm(meanFR~mean.R.H+mean.R.V,data=x)
  mLHV<-lm(meanFR~mean.L.H+mean.L.V,data=x)
  
  #calculate using mean eye position:
  mH<-lm(meanFR~mean.H,data=x)
  mV<-lm(meanFR~mean.V,data=x)
  mHV<-lm(meanFR~mean.H+mean.R.V,data=x)
  
  
  #Summarize desired information about models
  RH.p=anova(mRH)$'Pr(>F)'[1]
  LH.p=anova(mLH)$'Pr(>F)'[1]
  LV.p<- anova(mLV)$'Pr(>F)'[1]
  RV.p<- anova(mRV)$'Pr(>F)'[1]
  RH<-as.numeric(coef(mRHV)[2])
  RV<-as.numeric(coef(mRHV)[3]) 
  LH<-as.numeric(coef(mLHV)[2])
  LV<-as.numeric(coef(mLHV)[3])  
  
  H.p=anova(mH)$'Pr(>F)'[1]
  V.p<- anova(mV)$'Pr(>F)'[1]
  H<-as.numeric(coef(mHV)[2])
  V<-as.numeric(coef(mHV)[3]) 
  
  
  
  #two methods to calculate direction preferences:
  
  #calculate direction preference using slopes of regression lines
  dir.pref.R<-as.numeric(atan2(RV,RH))*180/pi
  dir.pref.L<-as.numeric(atan2(LV,LH))*180/pi
  dir.pref.slope<-as.numeric(atan2(V,H))*180/pi #conjugate
  
  #calculate direction preference using relative importance of horizontal and vertical
  br=as.numeric(calc.relimp(mRHV)$lmg)
  bl=as.numeric(calc.relimp(mLHV)$lmg)
  dir.imp.R<- atan2(br[2]*sign(RV),br[1]*sign(RH))*180/pi
  dir.imp.L<- atan2(bl[2]*sign(LV),bl[1]*sign(LH))*180/pi
  
  dir.pref.imp<-tryCatch({
    b=as.numeric(calc.relimp(mHV)$lmg)
    dir.pref.imp<- atan2(b[2]*sign(V),b[1]*sign(H))*180/pi
  },error=function(e){return(NA)})
  
  #This implements the significant slopes algorithm.
  #We use only the significant slopes to calculate direction preference.
  #If neither horizontal nor vertical slopes are significant then 
  #there is no direction preference
  RH<-ifelse(RH.p<0.001,RH,0)
  RV<-ifelse(RV.p<0.001,RV,0)
  LH<-ifelse(LH.p<0.001,LH,0)
  LV<-ifelse(LV.p<0.001,LV,0)
  H<-ifelse(H.p<0.001,H,0)
  V<-ifelse(V.p<0.001,V,0)
  
  dir.goodslopes.R<-ifelse(RH+RV==0,NA,as.numeric(atan2(RV,RH))*180/pi)
  dir.goodslopes.L<-ifelse(LV+LH==0,NA,as.numeric(atan2(LV,LH))*180/pi)
  dir.goodslopes<-ifelse(H+V==0,NA,as.numeric(atan2(V,H))*180/pi)
  

  
  any.sig=RV.p<0.001 | LV.p<0.001| LH.p <0.001| RH.p<0.001
  R2R<-summary(mRHV)$r.squared
  R2L<- summary(mLHV)$r.squared
  R2<- summary(mHV)$r.squared
  H.R2<- summary(mH)$r.squared #conj horizontal fit only
  V.R2<- summary(mV)$r.squared #conj vertical fit only
  
  #Calculate RMSE - compare with R-squared?
  # rmseR=sqrt(c(crossprod(mRHV$residuals))/mRHV$df.residual)
  # rmseL=sqrt(c(crossprod(mLHV$residuals))/mLHV$df.residual)
  rmseR=summary(mRHV)$sigma
  rmseL=summary(mLHV)$sigma
  
  #Calculate relationship between firing rate and standard deviation
  msd=lm(sd.sdf~meanFR,data=x)
  sd.int=as.numeric(coef(msd)[1])
  sd.slope=as.numeric(coef(msd)[2])
  
  #This data frame that I am returning is getting kind of big. 
  #I plan to remove the unused measurements once I am past the exploratory phase
  d<-data.frame(neuron=unique(x$neuron),
                RH,RV,LH,LV,
                dir.pref.R,dir.pref.L,dir.pref.slope,
                RH.p,LH.p,RV.p,LV.p,H.p,V.p,
                any.sig,
                R2R,R2L,R2,
                dir.imp.R,dir.imp.L,dir.pref.imp,
                dir.goodslopes,dir.goodslopes.R,dir.goodslopes.L)
}


getObservedcoefMONO<-function(z){
  
  mod<- lm(meanFR ~ mean.R.H+mean.R.V+ mean.L.H+mean.L.V,data=z)
  
  dt<-as.data.frame(rbind(as.numeric(coef(mod))))
  
  names(dt)<-c('intercept_o','RH_o','RV_o','LH_o','LV_o')
  dt
}

bootstrapDirPrefMONO<-function(zboot,nreps){
  
  getcoef<-function(n,z){
    z <- sample_frac(z,size= 1,replace=TRUE)
    mod<- lm(meanFR ~ mean.R.H+mean.R.V+ mean.L.H+mean.L.V,data=z)
    
    #This is kind ofa hack since there is only one row 
    #but it makes it so I can bind it with others later
    dt<-rbind(as.numeric(coef(mod)))
    
    names(dt)<-c('intercept','RH','RV','LH','LV')
    dt 
  }
  
  n<- matrix(1:nreps)
  neuronName<- zboot$neuron[1]
  x<- as.data.frame(rbindlist(lapply(n,getcoef,zboot))) #calls the function n times
  x<- mutate(x,neuron=neuronName)
}

