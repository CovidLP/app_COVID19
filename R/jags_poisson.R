mu_model <- function(a,b,c,i) {a*exp(c*i)/(1+b*exp(c*i))}



###############################################################################
### M0
###############################################################################
mod_string <- "model{

  for(i in 1:t) {
    y[i] ~ dpois(mu[i])
    mu[i] = a*exp(c*i)/(1+b*exp(c*i))
  }

  assint = a/b
  a ~ dgamma(0.1, 0.1)
  b ~ dgamma(1.5, 1.5)
  c ~ dgamma(0.1, 0.1)

  for(j in 1:L){
    yfut[j] ~ dpois(mufut[j])
    mufut[j] = a*exp(c*(t+j))/(1+b*exp(c*(t+j))) - a*exp(c*(t+j-1))/(1+b*exp(c*(t+j-1))) + 1e-7
  }

}"



###############################################################################
### M1 (one change point)
###############################################################################
mod_string_cp1 <- "model{

  for(i in 1:t) {
    y[i] ~ dpois(mu[i])
    mu[i] = ifelse( exp(c2*i)-b2^2*exp(3*c2*i)>=0 , a1*exp(c1*i)/(1+b1*exp(c1*i)) , a2*exp(c2*i)/(1+b2*exp(c2*i)) )
    change[i] = ifelse( exp(c2*i)-b2^2*exp(3*c2*i)>=0 , 0 , 1 )
  }

  a1 ~ dgamma(0.1, 0.1)
  b1 ~ dgamma(1.5, 1.5)
  c1 ~ dgamma(0.1, 0.1)

  assint = a2/b2
  a2 ~ dgamma(0.1, 0.1)
  b2 ~ dgamma(1.5, 1.5)
  c2 ~ dgamma(0.1, 0.1)


  for(j in 1:L){
    yfut[j] ~ dpois(mufut[j])
    mufut[j] = a2*exp(c2*(t+j))/(1+b2*exp(c2*(t+j))) - a2*exp(c2*(t+j-1))/(1+b2*exp(c2*(t+j-1))) + 1e-7
  }

}"



###############################################################################
### M2 (DM)
###############################################################################
mod_string_dm <- "model{

  for(i in 1:t) {
    y[i] ~ dpois(mu[i])
    mu[i] = a[i]*exp(c[i]*i)/(1+b[i]*exp(c[i]*i))
  }

  # prioris para os vetores de parametros
  a[1] ~ dgamma(0.1, 0.1)
  b[1] ~ dgamma(0.001, 1)
  c[1] ~ dgamma(0.1, 0.1)

  for (i in 2:t) {
    a[i] = a[i-1] * exp(wa[i-1])
    b[i] = b[i-1] * exp(wb[i-1])
    c[i] = c[i-1] * exp(wc[i-1])

    wa[i-1] ~ dnorm(0,Wa)
    wb[i-1] ~ dnorm(0,Wb)
    wc[i-1] ~ dnorm(0,Wc)
  }

#  for(j in 1:L){
#    yfut[j] ~ dpois(mufut[j])
#    mufut[j] = ifelse( a[t+j]*exp(c[t+j]*(t+j))/(1+b[t+j]*exp(c[t+j]*(t+j))) <= a[t+j-1]*exp(c[t+j-1]*(t+j-1))/(1+b[t+j-1]*exp(c[t+j-1]*(t+j-1))) , 1e-20 , a[t+j]*exp(c[t+j]*(t+j))/(1+b[t+j]*exp(c[t+j]*(t+j))) - a[t+j-1]*exp(c[t+j-1]*(t+j-1))/(1+b[t+j-1]*exp(c[t+j-1]*(t+j-1))) )
    
#    a[t+j] = a[t+j-1] * exp(wa[t+j-1])
#    b[t+j] = b[t+j-1] * exp(wb[t+j-1])
#    c[t+j] = c[t+j-1] * exp(wc[t+j-1])

#    wa[t+j-1] ~ dnorm(0,Wa*fa)
#    wb[t+j-1] ~ dnorm(0,Wb*fb)
#    wc[t+j-1] ~ dnorm(0,Wc*fc)
#  }

}"


###############################################################################
### M3 (DM)
###############################################################################
mod_string_dm2 <- "model{

 #distribuição dos dados y[i]
 for (i in 1:t){
   y[i] ~ dpois( mu[i])
   a[i] = exp(sum(wa[1:i]))
   b[i] = exp(sum(wb[1:i]))
   c[i] = exp(sum(wc[1:i]))
   mu[i] = a[i]*exp(c[i]*i)/(1+b[i]*exp(c[i]*i))
 }
 
 # prioris para o início da dinâmica
   wa[1] ~ dnorm(0, 0.1)#dgamma (0.1,0.1)
   wb[1] ~ dnorm(0, 0.1)#dgamma (0.1,0.1)
   wc[1] ~ dnorm(0, 0.1)#dgamma (0.1,0.1)

#prioris para as perturbações da evolução
  for (i in 2:t){
   wa[i] ~ dnorm(0, Wa )
   wb[i] ~ dnorm(0, Wb )
   wc[i] ~ dnorm(0, Wc )
  }

}"

