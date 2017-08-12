soa.ts.fun <- function(tsb,x) {
  fit <- lm(data=tsb,formula='sdf20~verg.angle+verg.velocity')
  return(as.numeric(coef(fit)[3])) }

z %>%
  # filter(neuron=='Bee-111') %>%
  group_by(neuron) %>%
  do(tidy(boot.ci(tsboot(.,soa.ts.fun,R=9,sim="geom",l=500),type='basic')$basic[4:5])) ->
  mm

mm %>%
  group_by(neuron) %>%
  mutate(key=row_number(),
         key=replace(key,key==1,'low'),
         key=replace(key,key==2,'high')) %>%
  spread(key,'x') %>%
  mutate(zero.cross=low*high<0)->
  ms


t %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         sdf=spikedensity(rasters,10),
         sdf20=lag(sdf,20),
         verg.angle=lep-rep,
         lev=parabolicdiff(lep,15),
         rev=parabolicdiff(rep,15),
         verg.velocity=lev-rev
  ) ->
  t


  t %>%
  do(tidy(boot.ci(tsboot(.,soa.ts.fun,R=9,sim="geom",l=500),type='basic')$basic[4:5])) %>%
  group_by(neuron) %>%
  mutate(key=row_number(),
         key=replace(key,key==1,'low'),
         key=replace(key,key==2,'high')) %>%
  spread(key,'x') %>%
  mutate(zero.cross=low*high<0)->
  ms

  