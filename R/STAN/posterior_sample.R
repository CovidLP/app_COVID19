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


predLW <- function(L=100,B,pop,casos,a,b,c,f,sunday,monday,index, index.s, index.m){
  #save sample size
  M <- length(a)
  Max <- pop
  y.fut <- mu <- matrix(-Inf, ncol=L,nrow=M)
  for(i in index) mu[,i] <-  exp(log(f)+log(a)+log(c)-(c*(B+i))-(f+1)*log(b+exp(-c*(B+i)) ) )
  for(i in index.s) mu[,i] <-  exp(log(f)+log(a)+log(c)-(c*(B+i))-(f+1)*log(b+exp(-c*(B+i)) ) ) * sunday
  for(i in index.m) mu[,i] <-  exp(log(f)+log(a)+log(c)-(c*(B+i))-(f+1)*log(b+exp(-c*(B+i)) ) ) * monday

  #gernate sample from the mu's
  for(i in 1:L){
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


predLWave <- function(L=100,B,pop,casos,a1,b1,c1,alpha1,a2,b2,c2,alpha2,delta1,delta2){
  #save sample size
  M <- length(a1)
  Max <- 2*pop
  y.fut <- mu <- mu1 <- mu2 <- matrix(-Inf, ncol=L,nrow=M)
  
  
  #generate sample from the mu's
  for(i in 1:L){
    mu1[,i] <- exp(log(a1)+log(c1)-(c1*(B+i))-2*log( b1+exp(-c1*(B+i)) ))
    mu2[,i] <- exp(log(a2)+log(c2)-(c2*(B+i))-2*log( b2+exp(-c2*(B+i)) ))
    mu[,i] <-  pnorm(alpha1*((B+i)-delta1),0,1) * mu1[,i] + pnorm(alpha2*((B+i)-delta2),0,1) * mu2[,i];
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
