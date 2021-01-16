###############################################################################
### M0 (new)
###############################################################################
mod_string_new <- "model{

   for(i in 1:t) {
      y[i] ~ dpois(mu[i])
      mu[i] = f*a*c*exp(-c*i)/(b+exp(-c*i))^(f+1)
   }

   a ~ dgamma(0.1, 0.1)
   b ~ dgamma(0.1, 0.1)
   c ~ dbeta(2,6)
   f = exp(f1)
   f1 ~ dnorm(0,.1)
   # f ~ dgamma(1,1)
   # f ~ dgamma(0.1,0.1)
   
#   for(j in 1:L){
#      yfut[j] ~ dpois(mu[t+j])
#      mu[t+j] = f*a*c*exp(-c*(t+j))/(b+exp(-c*(t+j)))^(f+1)
#   }

}"




###############################################################################
### M0 (new DM)
###############################################################################
mod_string_new_dm <- "model{

   for(i in 1:t) {
      y[i] ~ dpois(mu[i])
      a[i] = exp(sum(wa[1:i]))
      mu[i] = f*a[i]*c*exp(-c*i)/(b+exp(-c*i))^(f+1)
   }
   
   b ~ dgamma(0.1, 0.1)
   c ~ dbeta(2,6)
   f = exp(f1)
   f1 ~ dnorm(0,.1)
   # f ~ dgamma(1,1)
   # f ~ dgamma(0.1,0.1)
   # f = 1
   # Wa = 1e1
   # Ka = 1 # 1e1
   Wa ~ dgamma(0.01, 0.01)

   # prioris para o inicio da dinamica
   wa[1] ~ dnorm(-5, 0.1)
   # prioris para as perturbacoes da evolucao
   for (i in 2:t){
      # wa[i] ~ dnorm(0, Wa)
      wa[i] ~ dnorm(-1/(2*Wa),Wa)
   }
   for (j in 1:L){
      # wa[t+j] ~ dnorm(0, Wa)
      # wa[t+j] ~ dnorm(0, Wa/Ka)
      wa[t+j] ~ dnorm(-1/(2*Wa),Wa)
      # wa[t+j] ~ dnorm(-Ka/(2*Wa),Wa/Ka)
   }

   for(j in 1:L){
      yfut[j] ~ dpois(mu[t+j])
      a[t+j] = exp(sum(wa[1:(t+j)]))
      mu[t+j] = f*a[t+j]*c*exp(-c*(t+j))/(b+exp(-c*(t+j)))^(f+1)
   }

}"


