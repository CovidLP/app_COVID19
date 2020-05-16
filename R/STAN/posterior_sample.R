## generate a sample from the predictive distribution
# M: sample size for the predictive sample
# L: number of steps in future
# B: baseline time (last t of the fitted model)
# a: samples of parameter a in time t
# b: samples of parameter b in time t
# c: samples of parameter c in time t
predL <- function(L=100,B,pop,casos,a,b,c,f){
  #save sample size
  M <- length(a)
  Max <- 2*pop
  y.fut <- mu <- matrix(-Inf, ncol=L,nrow=M)

  #gernate sample from the mu's
  for(i in 1:L){
    mu[,i] <-  exp(log(f)+log(a)+log(c)-(c*(B+i))-(f+1)*log(b+exp(-c*(B+i)) ) )
    y.fut[,i] <- rpois(M,mu[,i])
    pos <- which(is.na(y.fut[,i]))
    if(length(pos) > 0) y.fut[pos,i] <- Max
  }
  pos <- which(rowSums(mu)+casos > pop)
  #pos <- unique(which(mu > pop, arr.ind=TRUE)[,1])
  if(length(pos) > 0 && length(pos) < 0.05*M) out <- list(y.fut = y.fut[-pos,], mu.fut = mu[-pos,], pos=pos)
  else out <- list(y.fut = y.fut, mu.fut = mu, pos=NULL)
  return(out)
}

