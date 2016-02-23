

parabolicdiff <- function(pos,n=7){
  q <- sum(2*((1:n)^2))
  convoutput<- convolve(pos,c(-n:-1, 1:n),type="open")
  convoutput<- convoutput[(n*2):(length(pos)-((n*2)+1))]
  vels<- c(array(convoutput[1],dim=n*2),convoutput,array(convoutput[length(convoutput)],dim=n*2))
  vels <- vels/q*1000
}

