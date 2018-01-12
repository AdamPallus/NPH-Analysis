t <- readRDS('SOA-NRTP.RDS')

measureCell<- function(x){
  
  x<-mutate(x,sdf20=lag(sdf,20))
  
  m<- lm(sdf20~verg.velocity+verg.angle,data=x)
  b<- calc.relimp(m)

  imp.Velocity=as.numeric(b$lmg)[1]
  imp.Position=as.numeric(b$lmg)[2]

  slope.Velocity=as.numeric(coef(m)["verg.velocity"])
  slope.Position=as.numeric(coef(m)["verg.angle"])
  
  mscale<- lm(scale(sdf20)~scale(verg.velocity)+scale(verg.angle),data=x)
  
  scale.Velocity=as.numeric(coef(mscale)["scale(verg.velocity)"])
  scale.Position=as.numeric(coef(mscale)["scale(verg.angle)"])
  
  d<- data.frame(x$neuron[1],imp.Velocity,imp.Position,slope.Velocity,slope.Position,
                 scale.Velocity,scale.Position)
  
}

t %>%
  group_by(neuron) %>%
  do(measureCell(.))->
  imp

imp %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE) %>%
  mutate(cellnum=as.numeric(cellnum),
         NRTP=cellnum<100) ->
  imp

imp$Location=as.factor(imp$NRTP)
levels(imp$Location)<- c('SOA','NRTP')


scatter<-ggplot(imp)+
  geom_point(aes(abs(scale.Position),abs(scale.Velocity),
                 color=Location),size=3,alpha=0.8)+
  coord_fixed()+
  geom_abline()+
  xlab('Sensitivity to Vergence Position (scaled)') +
  ylab('Sensitivity to Vergence Velocity (scaled)')+
  theme_minimal()+
  theme(legend.position = 'bottom')

# posDen<-ggplot(imp)+
#   geom_density(aes(abs(scale.Position),fill=Location),
#                alpha=0.3)+
#   theme_minimal()+theme(legend.position = 'none')+
#   xlab('Sensitivity to Vergence Position (scaled)')

posDen<-ggplot(imp)+
  geom_histogram(aes(abs(scale.Position),fill=Location),binwidth=0.05)+
  theme_minimal()+theme(legend.position = 'none')+
  xlab('Sensitivity to Vergence Position (scaled)')+
  facet_wrap(~Location,ncol=1)

# 
# velDen<-ggplot(imp)+
#   geom_density(aes(abs(scale.Velocity),fill=Location),
#                alpha=0.3)+
#   theme_minimal()+theme(legend.position = 'none')+
#   xlab('Sensitivity to Vergence Velocity (scaled)')

velDen<-ggplot(imp)+
  geom_histogram(aes(abs(scale.Velocity),fill=Location),bins=20)+
  theme_minimal()+theme(legend.position = 'none')+
  xlab('Sensitivity to Vergence Velocity (scaled)')+
  facet_wrap(~Location,ncol=1)

grid.arrange(scatter,arrangeGrob(posDen,velDen,ncol=2),heights=c(1,0.5))



scatter<-ggplot(imp)+
  geom_point(aes(abs(imp.Position),abs(imp.Velocity),
                 color=Location),size=3,alpha=0.8)+
  coord_fixed()+
  geom_abline()+
  xlab('Relative Importance Of Vergence Position') +
  ylab('Relative Importance of Vergence Velocity')+
  theme_minimal()+
  theme(legend.position = 'bottom')

# posDen<-ggplot(imp)+
#   geom_density(aes(abs(scale.Position),fill=Location),
#                alpha=0.3)+
#   theme_minimal()+theme(legend.position = 'none')+
#   xlab('Sensitivity to Vergence Position (scaled)')

posDen<-ggplot(imp)+
  geom_histogram(aes(abs(imp.Position),fill=Location),binwidth=0.05)+
  theme_minimal()+theme(legend.position = 'none')+
  xlab('Relative Importance of Vergence Position')+
  facet_wrap(~Location,ncol=1)

# 
# velDen<-ggplot(imp)+
#   geom_density(aes(abs(scale.Velocity),fill=Location),
#                alpha=0.3)+
#   theme_minimal()+theme(legend.position = 'none')+
#   xlab('Sensitivity to Vergence Velocity (scaled)')

velDen<-ggplot(imp)+
  geom_histogram(aes(abs(imp.Velocity),fill=Location),bins=20)+
  theme_minimal()+theme(legend.position = 'none')+
  xlab('Relative Importance of Vergence Velocity')+
  facet_wrap(~Location,ncol=1)

grid.arrange(scatter,arrangeGrob(posDen,velDen,ncol=2),heights=c(1,0.5))

