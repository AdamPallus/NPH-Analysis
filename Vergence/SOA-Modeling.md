# SOA Modeling
Adam  
September 14, 2016  







<!-- ```{r LoadDataset} -->
<!-- #I have loaded all the .csv files using the "loadnewcsv" function -->
<!-- #to save time I load them from the r binary file -->

<!-- t<- readRDS('SOA-NRTP.RDS') -->

<!-- t<- dplyr::filter(t, monkey %in% c('Bee','Ozette'),cellnum>100) -->

<!-- #velocity cells -->
<!-- # t<- dplyr::filter(t,neuron %in% -->
<!-- #   c('Bee-103','Bee-104','Bee-107','Bee-108','Bee-110','Bee-112','Bee-113','Bee-202', -->
<!-- #     'Bee-203','Bee-204','Bee-205','Bee-208','Bee-209','211','Bee-215','Ozette-102', -->
<!-- #     'Ozette-114','Ozette-116','Ozette-117','Ozette-118','Ozette-120','Ozette-121','Ozette-122')) -->

<!-- #testing purposes: -->
<!-- # t<- filter(t,neuron %in% c('Bee-101','Ozette-101')) -->


<!-- t<- group_by(t,neuron) %>% mutate(time=row_number()) -->
<!-- ``` -->

<!-- ```{r markEnhancements} -->
<!-- t<- mutate(t,verg.velocity=lev-rev) -->
<!-- t %>% -->
<!--   group_by(neuron) %>% -->
<!--   do(markEnhancement(v=.$verg.velocity,threshold2=12,threshold1=50))-> -->
<!--   zz -->

<!--   zz<- dplyr::select(zz,time,enhancenum) -->

<!--   t<- left_join(t,zz,by=c('neuron','time')) -->
<!--   t<-ungroup(t) -->

<!--   #determine whether it's convergence or divergence -->
<!--   t %>% -->
<!--     mutate(verg.enhance=!is.na(enhancenum), -->
<!--            enhance.type='none', -->
<!--            verg.direction=verg.velocity>0)-> -->
<!--     t -->
<!--   i<- t$verg.enhance & !t$verg.direction -->
<!--   t$enhance.type[i]<- 'divergence' -->
<!--   i<- t$verg.enhance & t$verg.direction -->
<!--   t$enhance.type[i]<- 'convergence' -->
<!--   t$enhance.type<- as.factor(t$enhance.type) -->
<!--   t$verg.direction<- NULL -->

<!--   t %>% group_by(neuron) %>% -->
<!--     do(dynamiclead(p=.,formula='verg.angle+verg.velocity',seq(5,200,by=5))) -> -->
<!--     t -->

<!-- # saveRDS(t,'enhancemarked.RDS') -->
<!-- ``` -->



term                      e          std
--------------  -----------  -----------
(Intercept)      26.7501070   29.3809023
Convergence       0.2109687    0.5940368
Divergence        0.1184498    0.2615979
Slow.Vergence     0.6103187    0.8008179
verg.angle        4.1764651    4.8011398



neuron        (Intercept)   Convergence   Divergence   Slow.Vergence   verg.angle
-----------  ------------  ------------  -----------  --------------  -----------
Bee-101             36.49          0.43         0.43            1.54         7.17
Bee-102             38.49          0.37         0.59            1.52         7.76
Bee-103            -22.52          2.88         0.23            3.14         9.55
Bee-104             49.14         -0.21         0.15           -0.10         8.18
Bee-105             22.98         -0.11         0.08            0.07         3.69
Bee-106             56.28         -0.23         0.09            0.11         5.06
Bee-107             44.31          0.63         0.43            1.85         8.45
Bee-108              9.14          1.46        -0.02            1.66         4.21
Bee-109             40.90          0.50        -0.05            0.46         0.22
Bee-110             46.50          0.26         0.27            0.70         5.10
Bee-111             68.29          0.04        -0.06            0.07         2.59
Bee-112             70.59          0.15        -0.16            0.43         5.65
Bee-113            -35.82         -0.02        -0.15            1.39        11.16
Bee-201             23.40         -0.01         0.00           -0.04        -0.19
Bee-202             16.75          0.50         0.44            0.96         6.54
Bee-203             -1.89          0.30        -0.13            0.68         3.87
Bee-204             24.07         -0.02         0.02           -0.07        -1.49
Bee-205             23.36          0.11        -0.17            0.56         9.23
Bee-206             44.66         -0.23         0.19           -0.19        -3.77
Bee-207            108.53         -0.16        -0.38           -0.36        -5.92
Bee-208             83.48         -0.23         0.20           -0.28        -6.51
Bee-209             76.52         -0.14         0.12           -0.13        -5.23
Bee-210             -7.51          0.36        -0.19            0.69         5.53
Bee-211             -9.80          2.01         0.12            2.75         4.43
Bee-212             10.32          0.01         0.00           -0.01         1.09
Bee-213             11.92          0.02        -0.07            0.22         5.12
Bee-214             15.50          0.22         0.13            0.23         1.25
Bee-215             -1.39          1.25         1.45            2.24        16.03
Bee-216             16.31          0.05         0.09            0.13         1.63
Bee-217              0.79          0.27        -0.04            0.25         0.46
Bee-218             44.96          0.83         0.54            1.89         1.00
Bee-219             51.11         -0.01         0.04           -0.21        -1.59
Bee-220             92.99         -0.32        -0.07           -0.35        -3.11
Ozette-101          70.36         -0.05        -0.09            0.48         4.41
Ozette-102          50.97         -0.14         0.10            0.36         5.79
Ozette-103          16.79         -0.09         0.03            0.40         7.04
Ozette-104          10.56         -0.24         0.05            0.60         7.41
Ozette-105           6.33         -0.04         0.04            0.10         2.10
Ozette-106          21.32         -0.07         0.06            0.18         7.97
Ozette-107          23.93         -0.10         0.13            0.00         7.06
Ozette-108          25.68         -0.23         0.23            0.39         9.52
Ozette-109           1.32         -0.26         0.23            0.31         8.27
Ozette-110          12.39         -0.20         0.09            0.63        13.22
Ozette-111          -0.78         -0.20         0.15            0.38         9.98
Ozette-112          26.95         -0.11         0.01            0.89         7.03
Ozette-113          23.90         -0.13         0.35            0.49         5.11
Ozette-114          14.03         -0.25         0.17            0.52         8.71
Ozette-115          -0.56         -0.11         0.06            0.27         4.60
Ozette-116          -2.75         -0.14         0.02            0.49         5.81
Ozette-117           8.16          0.60         0.17            0.86         1.78
Ozette-118          24.00          0.76         0.03            0.96         2.17
Ozette-119          59.76          0.13         0.03           -0.32        -4.20
Ozette-120          18.62          1.14         0.24            2.06         1.99
Ozette-121          20.07          0.69         0.46            2.00         3.75
Ozette-122         -23.43         -0.07        -0.12            0.43         8.88
Ozette-123          41.52         -0.04         0.06           -0.15        -1.72

<!-- ```{r bootstrapmodel,fig.height=8,fig.width=12} -->

<!-- ##The code below does the boostrap by converting the data.frame to a list of neurons -->
<!-- ##This is because bootstrap doesn't work with grouped data -->
<!-- ##Then we use lapply to apply the bootciEXPERIMENTAL function and save the result -->
<!-- ##Use the command to load 'Boostrap1999results.RDS' to get the saved results of this -->

<!-- # t<- readRDS('enhancemarked.RDS') -->
<!-- # n=unique(t$neuron) -->
<!-- # ci<-readRDS('TempBootstrap1999.RDS') #load the already analyzed cells -->
<!-- # tempci<- do.call('rbind',ci) -->
<!-- # n<- n[!n %in% unique(tempci$neuron)] #limit new analysis to unanalyzed cells -->
<!-- #  -->
<!-- # tlist<-NULL -->
<!-- # for (i in 1:length(n)){ -->
<!-- #   tlist[[i]]<- filter(t,neuron==n[i]) -->
<!-- # } -->
<!-- # t<-NULL -->
<!-- #  -->
<!-- # txx<- lapply(tlist,bootciEXPERIMENTAL,n=1999,alpha=0.05,formula='sdflag~verg.angle+verg.velocity:enhance.type') -->
<!-- # saveRDS(txx,'tempBootstrap1999part2.RDS') -->



<!-- ci<- readRDS('Bootstrap1999results.RDS') -->





<!-- ci %>% -->
<!--   mutate(term=replace(term,term=='verg.velocity:enhance.typenone','Slow.Velocity'), -->
<!--          term=replace(term,term=='verg.velocity:enhance.typeconvergence','Convergence'), -->
<!--          term=replace(term,term=='verg.velocity:enhance.typedivergence','Divergence')) %>% -->
<!--   # group_by(neuron) %>% -->
<!--   filter(term != '(Intercept)') -> -->
<!--   pci -->

<!-- g<-ggplot(pci,aes(factor(term),m)) -->

<!-- g+geom_errorbar(aes(ymin=low,ymax=high,color=neuron),size=2,alpha=1/2,width=0.2)+ -->
<!--   geom_hline(yintercept = 0)+ -->
<!--   ylab('Bootstrap conficence interval for parameter estimate')+ -->
<!--   xlab('Term')+ -->
<!--   # coord_cartesian(ylim=c(-1,1))+ -->
<!--   coord_flip(ylim=c(-0.05,0.05)) -->


<!-- ci %>% -->
<!--    mutate(term=replace(term,term=='verg.velocity:enhance.typenone','Slow.Velocity'), -->
<!--          term=replace(term,term=='verg.velocity:enhance.typeconvergence','Convergence'), -->
<!--          term=replace(term,term=='verg.velocity:enhance.typedivergence','Divergence')) %>% -->
<!--   # group_by(neuron) %>% -->
<!--   mutate(m=(low+high)/2) %>% -->
<!--   filter(term != '(Intercept)', term != 'verg.angle') -> -->
<!--   pci -->

<!-- g<-ggplot(pci,aes(factor(term),m)) -->

<!-- g+geom_errorbar(aes(ymin=low,ymax=high,color=neuron),size=2,alpha=1/2,width=0.2)+ -->
<!--   geom_hline(yintercept = 0)+ -->
<!--   coord_flip()+ -->
<!--   ylab('Bootstrap conficence interval for parameter estimate')+ -->
<!--   xlab('Term') -->


<!-- ``` -->

<!-- ```{r lags,fig.height=8,fig.width=12} -->
<!-- t %>% -->
<!--   group_by(neuron) %>% -->
<!--   summarize(lag=first(dynamiclead)) -> -->
<!--   s -->

<!-- qplot(lag,data=s) -->
<!-- ggplot(s)+geom_bar(aes(as.factor(neuron),lag),stat='identity') -->
<!-- # qplot(neuron,lag,data=s,geom='line') -->
<!-- ``` -->

