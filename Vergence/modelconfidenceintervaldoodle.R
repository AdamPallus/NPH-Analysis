
#this is a script I wrote to compare different ways of plotting vergence sensitivity
#the idea was that you can combine the terms either before or after modeling
#the final plot shows the confidence intervals just from the standard error
#I think we underestimate the confidence intervals because the metric treats each point as 
#independent but in reality they are not. We need time series bootstrapping or 
#my saccade bootstrapping thing

#USAGE: run the first code block in SOArevision.Rmd to load the data, choose one cell and 
       #measure the aspects of the saccades

m1<- lm(sdf20~rev+lev+rep+lep,data=z)
b<- calc.relimp(m1)
plot(b)

m2<- lm(sdf20~rev+rep,data=z)
b<- calc.relimp(m2)
plot(b)


m3<- lm(sdf20~lev+lep,data=z)
b<- calc.relimp(m3)
plot(b)

z<- mutate(z,Ec=(rep+lep)/2,Ecv=(rev+lev)/2,
           Ev=(lep-rep),Evv=(lev-rev))
       
mm<- lm(sdf20~rep+rev,data=z)
mc<- lm(sdf20~Ec+Ecv,data=z)
mv<- lm(sdf20~Ev+Evv,data=z)

x <- c(summary(mm)$r.squared,summary(mc)$r.squared,summary(mv)$r.squared)

x<- data.frame(model=c('monocular','conjugate','vergence'),r2=x)

bic<- c(BIC(mm),BIC(mc),BIC(mv))

c<-confint(m1,level=0.95) #for some reason this returns a stupid matrix with dimnames
c<-as.data.frame(c) #convert to data frame
names(c)<-c('lower','upper') #change the stupid names to actual useful names
c$coef=dimnames(c)[[1]] #make the dimnames a usable column
c<-mutate(c,num=row_number()) #add a row number for plotting
c<- filter(c,coef!='(Intercept)') #get rid of this term for plotting since it's way bigger

ggplot(c)+
  geom_segment(aes(x=abs(lower),
                   xend=abs(upper),
                   y=num,
                   yend=num,
                   color=coef),
               size=3)
