pauseMAT<- function (plimit=0.05, pausethresh=0.995, burstthresh=0.05, p0=0.05,debug=FALSE){
  #pausethresh: closer to one finds more pauses. Lower number if too many false positives
  #burstthresh: Higher numbers finds fewer bursts. Lower number if too many false positives
  
  #plimit: Once bursts and pauses are detect, reject any that fail to meet this threshold. 
  #Raising this doesn't help find more pauses if pausethresh is too low
  
  #p0: Percent of total ISIs used to calculate moving center. Lower number means it changes more often
  
  require(tcltk2)
  require(R.matlab)
  require(dplyr)
  require(h5)
  require(ggplot2)
  source('RobustGaussianSurprise.R')
  
  
  #Make a popup dialog to select the data file
  w<- tktoplevel()
  filename<- tclvalue(tkgetOpenFile(parent=w,filetypes = "{ {MAT Files} {.mat} }")) 
  tkraise(w)
  tkdestroy(w)
  
  # filename <- tclvalue(tkgetOpenFile(filetypes = "{ {MAT Files} {.mat} }")) 
  
  #Check to see if a file was selected. Then try to load it.
  if (!nchar(filename)) {
    stop('No File Selected')
  } else {
    z<- strsplit(filename,'/')
    z<- z[[1]]
    savename<-z[length(z)]
    message(paste('Loading',savename,'...'))
    savename<- paste(substr(savename,1,nchar(savename)-4),'-pauses.mat',sep='')
    isH5<- FALSE
    
    #In this block we try  loading the file as a pre-7.3 mat file
    #If that fails, set the isH5 flag to be true. 
    isH5<- tryCatch({
      suppressWarnings(m<- readMat(filename))
      isH5<- FALSE
    }, error =function(e) {
      message('Loading as H5...')
      return(TRUE)
    }
    )
  }
  
  #If it's an H5 file, do some complicated trickery to figure out what the variable names are
  if (isH5){
    file<- h5file(filename)
    l<- list.datasets(file)
    s<- strsplit(l, split='/')
    getname<- function(s){ return(s[2])}
    n<- sapply(s, getname)
    fields<- unique(n)
  } else{ #if it's not an H5, the names are right there in names()
    fields <- names(m)
  }
  
  #Make a popup list box that lists all the top-level variable names
  win1 <- tktoplevel()
  win1$env$lst <- tk2listbox(win1, height = 20, selectmode = "single")
  tkgrid(tk2label(win1, text = "Select Spiketimes", justify = "left"),
         padx = 10, pady =c(15, 5), sticky = "w")
    onOK <- function() {
    fieldChoice <- fields[as.numeric(tkcurselection(win1$env$lst)) + 1]
    # spiketimes<- as.vector(m[fruitChoice])
    assign("fieldChoice",fieldChoice, envir = .GlobalEnv)
    tkdestroy(win1)
  }
  tkgrid(win1$env$lst, padx = 10, pady = c(5, 5))
    win1$env$butOK <-tk2button(win1, text = "OK", width = -6, command = onOK)
  tkgrid(win1$env$butOK, padx = 10, pady = c(5, 25))
  
  for (field in fields)
    tkinsert(win1$env$lst, "end", field)
  # Default fruit is Banana.  Indexing starts at zero.
  tkselection.set(win1$env$lst, 2)
  

  

  
  #wait for one of the options in the list to be selected
  tkwait.window(win1)
  
  fieldChoice<- as.character(fieldChoice[[1]])
  
  #If it's an H5 file, try getting the data out of the top level variable
  if (isH5){
    spiketimes<- tryCatch({
    spiketimes<- file[fieldChoice]
    spiketimes<- as.vector(spiketimes[])
#If the numbers aren't there, then assume they are in fieldChoice.times
    
  }, error= function(e) {spiketimes<- file[paste(fieldChoice,'times',sep='/')]
    spiketimes<- as.vector(spiketimes[])},finally={
    h5close(file) #close the original data file
  })

  }else{ #If it's not an H5, load the data from the variable chosen. 
    #I suppose this will fail if the data are hidden in a structure
  spiketimes<- m[fieldChoice]
  spiketimes<- as.vector(spiketimes[[1]])
  # print(head(spiketimes))
  }
  
  #Calculate the interspike interval
  TimeShift<- first(spiketimes)
  isi<- spiketimes[-1]-spiketimes[-(length(spiketimes)-1)]
  spiketimes<-spiketimes[-1]
  
  p<- data.frame(spiketimes=spiketimes,isi=isi)
  ####DEBUG######
  if (debug==TRUE){
    p<- head(p,10000)
  }
  ###############
  message('Calculating bursts and pauses...')
  
  suppressWarnings(
    bp<- f.BPsummary(list(p),
                     Pthresh=plimit,
                     p0=p0,
                     thresh1=qnorm(pausethresh),
                     thresh0=qnorm(burstthresh))
  )

  #Simplify the output 
  bp$burst[[1]] %>%
    group_by(clusid) %>%
    summarise(start_burst=min(start)+TimeShift,
              end_burst=max(end)+TimeShift,
              duration_burst=end_burst-start_burst,
              p_burst=first(adjP)) ->
    bursts
  bp$pause[[1]] %>%
    group_by(clusid) %>%
    summarise(start_pause=min(start)+TimeShift,
              end_pause=max(end)+TimeShift,
              duration_pause=end_pause-start_pause,
              p_pause=first(adjP)) ->
    pauses
  if (debug==TRUE){
    qplot(pauses$start_pause)
  }else{
  #Save the data with another popup
  savefile<- tclvalue(tkgetSaveFile(initialfile= savename, filetypes = "{ {MAT Files} {.mat} }"))
  writeMat(savefile,
           start_pause=pauses$start_pause, 
           end_pause=pauses$end_pause, 
           p_pause=pauses$p_pause, 
           start_burst=bursts$start_burst,
           end_burst=bursts$end_burst,
           p_burst=bursts$p_burst)
  }
}

