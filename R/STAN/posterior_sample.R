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


predL3Wave <- function(L=100,B,pop,casos,a1,b1,c1,alpha1,
                                         a2,b2,c2,alpha2,
                                         a3,b3,c3,alpha3,
                                         delta1,delta2,delta3){
  #save sample size
  M <- length(a1)
  Max <- 2*pop
  y.fut <- mu <- mu1 <- mu2 <- mu3 <- matrix(-Inf, ncol=L,nrow=M)
  
  
  #generate sample from the mu's
  for(i in 1:L){
    mu1[,i] <- exp(log(a1)+log(c1)-(c1*(B+i))-2*log( b1+exp(-c1*(B+i)) ))
    mu2[,i] <- exp(log(a2)+log(c2)-(c2*(B+i))-2*log( b2+exp(-c2*(B+i)) ))
    mu3[,i] <- exp(log(a3)+log(c3)-(c3*(B+i))-2*log( b3+exp(-c3*(B+i)) ))
    mu[,i] <-  pnorm(alpha1*((B+i)-delta1),0,1) * mu1[,i] +
               pnorm(alpha2*((B+i)-delta2),0,1) * mu2[,i] +
               pnorm(alpha3*((B+i)-delta3),0,1) * mu3[,i];
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

predLWaveWeekend<- function(L = 100, B, pop, casos,
                            a1, b1, c1, alpha1, delta1,
                            a2, b2, c2, alpha2, delta2,
                            w1, w2, w3, d_1, d_2, d_3){
  #save sample size
  M <- length(a1)
  Max <- 2*pop
  y.fut <- mu <- mu_star <- mu1 <- mu2 <- matrix(-Inf, ncol=L,nrow=M)
  
  pos_w1<- which( ((((B+1):(B+L)) - w1)%%7 == 0) & w1 > 0)
  pos_w2<- which( ((((B+1):(B+L)) - w2)%%7 == 0) & w2 > 0)
  pos_w3<- which( ((((B+1):(B+L)) - w2)%%7 == 0) & w3 > 0)
  pos_remain<- (1:L)[is.na(pmatch(x = 1:L, table = sort(c(pos_w1, pos_w2, pos_w3))))]
  
  #generate sample from the mu's
  for(i in pos_w1){
    mu1[,i] <- exp(log(a1)+log(c1)-(c1*(B+i))-2*log( b1+exp(-c1*(B+i)) ))
    mu2[,i] <- exp(log(a2)+log(c2)-(c2*(B+i))-2*log( b2+exp(-c2*(B+i)) ))
    mu_star[,i] = pnorm(q = alpha1 * ((B+i) - delta1), mean = 0, sd = 1) * mu1[,i] +
      pnorm(q = alpha2 * ((B+i) - delta2), mean = 0, sd = 1) * mu2[,i]
    mu[,i]=  mu_star[,i]*(d_1^w1)
    y.fut[,i] <- rpois(M,mu[,i])
    pos <- which(is.na(y.fut[,i]))
    if(length(pos) > 0) y.fut[pos,i] <- Max
  }
  for(i in pos_w2){
    mu1[,i] <- exp(log(a1)+log(c1)-(c1*(B+i))-2*log( b1+exp(-c1*(B+i)) ))
    mu2[,i] <- exp(log(a2)+log(c2)-(c2*(B+i))-2*log( b2+exp(-c2*(B+i)) ))
    mu_star[,i] = pnorm(q = alpha1 * ((B+i) - delta1), mean = 0, sd = 1) * mu1[,i] +
      pnorm(q = alpha2 * ((B+i) - delta2), mean = 0, sd = 1) * mu2[,i]
    mu[,i]=  mu_star[,i]*(d_2^w2)
    y.fut[,i] <- rpois(M,mu[,i])
    pos <- which(is.na(y.fut[,i]))
    if(length(pos) > 0) y.fut[pos,i] <- Max
  }
  for(i in pos_w3){
    mu1[,i] <- exp(log(a1)+log(c1)-(c1*(B+i))-2*log( b1+exp(-c1*(B+i)) ))
    mu2[,i] <- exp(log(a2)+log(c2)-(c2*(B+i))-2*log( b2+exp(-c2*(B+i)) ))
    mu_star[,i] = pnorm(q = alpha1 * ((B+i) - delta1), mean = 0, sd = 1) * mu1[,i] +
      pnorm(q = alpha2 * ((B+i) - delta2), mean = 0, sd = 1) * mu2[,i]
    mu[,i]=  mu_star[,i]*(d_3^w3)
    y.fut[,i] <- rpois(M,mu[,i])
    pos <- which(is.na(y.fut[,i]))
    if(length(pos) > 0) y.fut[pos,i] <- Max
  }
  
  for(i in pos_remain){
    mu1[,i] <- exp(log(a1)+log(c1)-(c1*(B+i))-2*log( b1+exp(-c1*(B+i)) ))
    mu2[,i] <- exp(log(a2)+log(c2)-(c2*(B+i))-2*log( b2+exp(-c2*(B+i)) ))
    mu_star[,i] = pnorm(q = alpha1 * ((B+i) - delta1), mean = 0, sd = 1) * mu1[,i] +
      pnorm(q = alpha2 * ((B+i) - delta2), mean = 0, sd = 1) * mu2[,i]
    mu[,i]=  mu_star[,i]
    y.fut[,i] <- rpois(M,mu[,i])
    pos <- which(is.na(y.fut[,i]))
    if(length(pos) > 0) y.fut[pos,i] <- Max
  }
  
  pos <- which(rowSums(mu)+sum(casos) > pop)
  #pos <- unique(which(mu > pop, arr.ind=TRUE)[,1])
  if(length(pos) > 0 && length(pos) < 0.05*M) out <- list(y.fut = y.fut[-pos,], mu.fut = mu[-pos,], pos=pos)
  else out <- list(y.fut = y.fut, mu.fut = mu, pos=NULL)
  return(out)
}

