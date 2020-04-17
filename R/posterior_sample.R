## generate a sample from the predictive distribution
# M: sample size for the predictive sample
# L: number of steps in future
# B: baseline time (last t of the fitted model)
# a: samples of parameter a in time t
# b: samples of parameter b in time t
# c: samples of parameter c in time t
# taua : precision of the normal distribution for the future samples of a
# taub : precision of the normal distribution for the future samples of b
# tauc : precision of the normal distribution for the future samples of c
pred <- function(L=100,B,a,b,c,taua,taub,tauc){
  #save sample size
  M.save <- M <- length(a)

  mu <- a.fut <- b.fut <- c.fut <- matrix(-Inf, ncol=L+1,nrow=M)
  
  #initialize process with first information
  a.fut[,1] <- a
  b.fut[,1] <- b
  c.fut[,1] <- c
  
  mu[,1] <-  (a.fut[,1]*exp(c.fut[,1]*(B+1))) / (1+b.fut[,1]*exp(c.fut[,1]*(B+1)))

  #limit attempt for the rejection procedure
  limit <- 10
  for(t in 2:(L+1)){
    #counter for how many attempts 
    count <- 1
    M <- M.save
    pos <- 1:M
    while(count <= limit){
       
       #generate sample for w's
       wa <- rnorm(M,0,sd=sqrt(1/taua))
       wb <- rnorm(M,0,sd=sqrt(1/taub))
       wc <- rnorm(M,0,sd=sqrt(1/tauc))

       #calculate a,b and c parameters
       a.fut[pos,t] <- exp(log(a.fut[pos,t-1]) + wa)
       b.fut[pos,t] <- exp(log(b.fut[pos,t-1]) + wb)       
       c.fut[pos,t] <- exp(log(c.fut[pos,t-1]) + wc)

       #calculate mu
       mu[pos,t] <- (a.fut[pos,t]*exp(c.fut[pos,t]*(B+t))) / (1+b.fut[pos,t]*exp(c.fut[pos,t]*(B+t)))

       #check if mu satisify condition
       cond <- mu[,t]-mu[,t-1]
       pos <- which(cond <= 0)
       if(length(pos) != 0){
          M <- length(pos)
          count <- count + 1
          if(count == limit+1) mu[pos,t] <- mu[pos,t-1] + 1e-7
       }
       else count <- limit + 1
    }
  }

  M <- M.save
  y.fut <- matrix(-Inf, ncol=L,nrow=M)
  #gernate sample from the mu's
  for(i in 1:L) y.fut[,i] <- rpois(M,mu[,i+1]-mu[,i])

  out <- list(y.fut <- y.fut, mu.fut <- mu,  a.fut <- a.fut, b.fut <- b.fut, c.fut <- c.fut)
  return(out)
}

