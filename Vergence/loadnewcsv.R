loadnewcsv<- function(referencefile=NULL,path="C:/Users/setup/Desktop/NRTP Vergence/PPRF/"){
  require(stringr)
  require(dplyr)
  #This function loads .csv files in a particular folder. They must have the same columns for rbind
  #Saves time by only reading the csv when necessary
  
  #get names of all files in path
  files <- list.files(path=path,pattern='*.csv')
  #extract neuron name eg. Bee-01
  names<-sapply(files, str_match,"^[a-zA-Z]+-[0-9]+",USE.NAMES=FALSE)
  # check for new cells
  if (!is.null(referencefile)){
    files<-files[!names %in% referencefile$neuron] #comparison
  }
  
  nfiles<-length(files)
  
  if (nfiles>0){
    message(c('New Files: ',files))
    loadedfiles <- lapply(paste(path,files,sep=''),read.csv)
    t<-data.frame()
    # t.old<-NULL
    for (i in 1:nfiles) {
      f<- files[i]
      message(paste('Loading:',f))
      temp=loadedfiles[[i]]
      names<-str_match(f,"(^[a-zA-Z]+)-([0-9]+)")
      temp$neuron<-names[1]
      temp$monkey<-names[2]
      temp$cellnum<-as.numeric(names[3])
      temp$sdf<-spikedensity(temp$rasters,sd=10)
      # leadtime<-dynamiclead(temp)
      message(paste('ms of data:',nrow(temp)))
      mutate(temp,
             # sdflag=lag(sdf,leadtime),
             conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
             # s=markSaccades(conj.velocity,buffer=10,threshold=10),
             # slong=markSaccades(conj.velocity,buffer=longbuffer,threshold=10),
             time=row_number(),
             verg.angle=lep-rep,
             verg.velocity=parabolicdiff(verg.angle,7))->
        temp
      
      t <-rbind(t,temp)
    }
    t<- dplyr::select(t, -thp,-tvp,-time)
  }else{
    message('********NO NEW CELLS********')
    t<-referencefile
  }
  return(t)
}