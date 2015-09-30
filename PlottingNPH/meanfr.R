meanfr <- function(t){
  library(dplyr)
  t %>%
    filter(rev<1, revV<1, lev<1,levV<1) %>%
    mutate(RHep=round(rep),RVep=round(repV),LHep=round(lep),LVep=round(lepV)) %>%
    group_by(Hep,Vep) %>%
    summarize(fr=mean(sdf)) ->
    s
  return(s)
}