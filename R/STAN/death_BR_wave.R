rm(list=ls())

setwd("/home/marcosop/Covid/R/STAN")

###################################################################
### Packages
###################################################################

library(doMC)
library(dplyr)
library(foreach)
library(matrixStats)
library(mcmcplots)
library(MCMCvis)
library(rstan)                              
library(tidyr)
library(covid19br)

Sys.setenv(LANGUAGE='en')
rstan_options(auto_write = TRUE)
###################################################################
### Data sets: https://github.com/CSSEGISandData
###################################################################
covid19 <- downloadCovid19(level = "states") %>%
  select(date,n=accumCases,d=accumDeaths,n_new=newCases, d_new=newDeaths,state) %>%
  arrange(state,date) 

uf <- distinct(covid19,state)

#br_pop <- read.csv(file = "https://raw.githubusercontent.com/CovidLP/app_COVID19/master/R/pop/pop_BR.csv")
br_pop <- read.csv("../pop/pop_BR.csv")

###########################################################################
###### JAGS/STAN 
###########################################################################
#register cores
registerDoMC(cores = 24)    # Alternativa Linux


#complie stan model
model="model_stan_waves_weekend.stan"    #modelo STAN 
mod<- try(stan_model(file = model,verbose=FALSE))


state_list <- c("AC", "AL", "AM", "AP", "BA",
                "DF", "ES", "GO", "MG", "MS",
                "MT", "PA", "PB", "PE", "PI",
                "PR", "RJ", "RN", "RO", "RS",
                "SC", "SE", "SP", "TO") # 24

state_vec <- which(uf$state %in% state_list)

obj <- foreach( s = state_vec ) %dopar% {

  #s<- 18
i = 5 # (2: confirmed, 3: deaths, 4: new confirmed, 5: new deaths)
L = 1000
#t0 = Sys.time()
estado = uf$state[s] #; estado

Y <- covid19 %>% filter(state==estado)

{if(sum(duplicated(Y$date)) > 0){
  Y <- Y[-which(duplicated(Y$date)),]
}}

#Y <- covid19 %>% filter(state==estado) %>%
#  mutate(n_new = n - lag(n, default=0),
#         d_new = d - lag(d, default=0)) %>%
#  select(date, n, d, n_new, d_new, state) %>%
#  arrange(date) %>% filter(date>='2020-01-23')

#Y = covid19 %>% filter(state==uf$state[s])

while(any(Y$d_new <0)){
  pos <- which(Y$d_new <0)
  for(j in pos){
    Y$d_new[j-1] = Y$d_new[j] + Y$d_new[j-1]
    Y$d_new[j] = 0
    Y$d[j-1] = Y$d[j]
  }
}

#Y_aux<- data.frame(date = as.Date(c("2020-02-23", "2020-02-24")), d = c(0, 0), n_new = c(0, 0), d_new = c(0, 0), state = estado)
#Y<- full_join(x = Y_aux, y = Y)

###
Sys.setlocale("LC_TIME", "C")
Y$week_days<- factor(x = weekdays(Y$date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


pop <- br_pop$pop[which(br_pop$uf == uf$state[s])]

t = dim(Y)[1]

params = c("a1", "b1", "c1", "alpha1", "delta1",
           "a2", "b2", "c2", "alpha2", "delta2",
           "mu", "d_1", "d_2", "d_3")


burn_in= 5e3
lag= 3
sample_size= 1e3
number_iterations= burn_in + lag*sample_size
number_chains = 1

data_stan = list(y = Y[[i]], n = t, pop = 0.08*pop, p = 0.25,
                 w1 = 1, w2 = 2, w3 = 0)

init <- list(
  list(a1 = 150, b_aux1 = log(1), c1 = .5, a2 = 150, b_aux2 = log(1), c2 = .5, alpha1=0.5, alpha2=0.5, 
       delta1=1, delta2=90, d_1=1, d_2=1, d_3=1)
)

mod_sim<- try(sampling(object = mod, data = data_stan,
                       pars = params,
                       chains = number_chains,
                       init = init,
                       iter = number_iterations, warmup = burn_in, thin = lag, 
                       control = list(max_treedepth = 35, adapt_delta=0.999),
                       verbose = TRUE, open_progress=FALSE, show_messages=FALSE))


mod_chain = as.data.frame(mod_sim)
a_pos<- paste0("a", 1:2)
b_pos<- paste0("b", 1:2)
c_pos<- paste0("c", 1:2)

alpha_pos<- paste0("alpha", 1:2)
delta_pos<- paste0("delta", 1:2)

d_pos<- paste0("d_", 1:3)

mu_pos = paste0("mu[",1:t,"]")

source("posterior_sample.R")
fut<- predLWaveWeekend(L = L, B = t, pop = pop, casos = data_stan$y,
                       a1 = c(mod_chain[[a_pos[1]]]), b1 = c(mod_chain[[b_pos[1]]]), c1 = c(mod_chain[[c_pos[1]]]),
                       alpha1 = c(mod_chain[[alpha_pos[1]]]), delta1 = c(mod_chain[[delta_pos[1]]]),
                       a2 = c(mod_chain[[a_pos[2]]]), b2 = c(mod_chain[[b_pos[2]]]), c2 = c(mod_chain[[c_pos[2]]]),
                       alpha2 = c(mod_chain[[alpha_pos[2]]]), delta2 = c(mod_chain[[delta_pos[2]]]),
                       w1 = data_stan$w1, w2 = data_stan$w2, w3 = data_stan$w3,
                       d_1 = c(mod_chain[[d_pos[1]]]), d_2 = c(mod_chain[[d_pos[2]]]), d_3 = c(mod_chain[[d_pos[3]]]))


mod_chain_y = fut$y.fut
#mod_chain_y = as.matrix(mod_chain[yfut_pos])
mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[3]][t]

L0 = 14  

### list output
df_predict <- data.frame( date = as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01"),
                          q25  = colQuantiles(mod_chain_cumy[,1:L0], prob=.025),
                          med  = colQuantiles(mod_chain_cumy[,1:L0], prob=.5),
                          q975 = colQuantiles(mod_chain_cumy[,1:L0], prob=.975),
                          m    = colMeans(mod_chain_cumy[,1:L0]))
row.names(df_predict) <- NULL 

lt_predict <- lt_summary <- NULL

#longterm
L0 = 1000

#acha a curva de quantil 
if(Y[[3]][t] > 1000){
  #acha a curva de quantil 
  lowquant <- colQuantiles(mod_chain_y[,1:L0], prob=.025)
  medquant <- colQuantiles(mod_chain_y[,1:L0], prob=.5)
  highquant <- colQuantiles(mod_chain_y[,1:L0], prob=.975)
}
else{
  lowquant <- c(Y[[3]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.025))
  lowquant <- (lowquant-lag(lowquant,default=0))[-1]
  medquant <- c(Y[[3]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.5))
  medquant <- (medquant-lag(medquant,default=0))[-1]
  highquant <- c(Y[[3]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.975))
  highquant <- (highquant-lag(highquant,default=0))[-1]
}

NTC25 =sum(lowquant)+Y[[3]][t]
NTC500=sum(medquant)+Y[[3]][t]
NTC975=sum(highquant)+Y[[3]][t]

##flag
cm <- pop * 0.08 * 0.25
ch <- pop * 0.12 * 0.25  
flag <- 0 #tudo bem
{if(NTC500 > cm) flag <- 2 #nao plotar
  else{if(NTC975 > ch){flag <- 1; NTC25 <- NTC975 <- NULL}}} #plotar so mediana

#vetor de data futuras e pega a posicao do maximo do percentil 25.
dat.vec <- as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01")
dat.full <- c(Y[[1]],dat.vec)


Dat25 <- Dat500 <- Dat975 <- NULL
dat.low.end <- dat.med.end <- dat.high.end <- NULL

#mod_chain_mu = as.matrix(mod_chain[mu_pos])
ifelse(length(fut$pos) > 0,
       mod_chain_mu <- cbind(as.matrix(mod_chain[mu_pos])[-fut$pos,],fut$mu.fut),
       mod_chain_mu <- cbind(as.matrix(mod_chain[mu_pos]),fut$mu.fut))
mu50 <- apply(mod_chain_mu,2,quantile, probs=0.5)
Dat500 <- dat.full[which.max(mu50[1:(t+L0)])]

q <- .99
med.cum <- mu50#c(medquant[1]+Y[[2]][t],medquant[2:length(medquant)])
med.cum <- colCumsums(as.matrix(med.cum))
med.cum <- med.cum/med.cum[length(med.cum)]
med.end <- which(med.cum - q > 0)[1]
#dat.med.end <- dat.vec[med.end]
dat.med.end <- dat.full[med.end]  

#if(flag == 0){
#definicao do pico usando a curva das medias
mu25 <- apply(mod_chain_mu,2,quantile, probs=0.025)
mu975 <- apply(mod_chain_mu,2,quantile, probs=.975)

posMax.q25 <- which.max(mu25[1:(t+L0)]) 
aux <- mu975 - mu25[posMax.q25]
aux2 <- aux[posMax.q25:(t+L0)]
val <- ifelse(length(aux2[aux2<0]) > 0, min(aux2[aux2>0]), aux[length(aux)])
dat.max <- which(aux == val)

aux <- mu975 - mu25[posMax.q25]
aux2 <- aux[1:posMax.q25]
val <- min(aux2[aux2>0]) 
dat.min <- which(aux == val)

Dat25 <- dat.full[dat.min]
Dat975 <- dat.full[dat.max]

#calcula o fim da pandemia
low.cum <- mu25 #c(lowquant[1]+Y[[2]][t],lowquant[2:length(lowquant)])
low.cum <- colCumsums(as.matrix(low.cum))
low.cum <- low.cum/low.cum[length(low.cum)]
low.end <- which(low.cum - q > 0)[1]
#dat.low.end <- dat.vec[low.end]
dat.low.end <- dat.full[low.end]

high.cum <- mu975 #c(highquant[1]+Y[[2]][t],highquant[2:length(highquant)])
high.cum <- colCumsums(as.matrix(high.cum))
high.cum <- high.cum/high.cum[length(high.cum)]
high.end <- which(high.cum - q > 0)[1]
#dat.high.end <- dat.vec[high.end]
dat.high.end <- dat.full[high.end]
#}

L0 <- 500
lt_predict <- data.frame( date = dat.vec[1:L0],
                          q25  = lowquant[1:L0],
                          med  = medquant[1:L0],
                          q975 = highquant[1:L0],
                          m    = colMeans(mod_chain_y[,1:L0]))     
row.names(lt_predict) <- NULL

lt_summary <- list(NTC25=NTC25,
                   NTC500=NTC500,
                   NTC975=NTC975,
                   high.dat.low=Dat25,
                   high.dat.med=Dat500,
                   high.dat.upper=Dat975,
                   end.dat.low = dat.low.end,
                   end.dat.med = dat.med.end,
                   end.dat.upper = dat.high.end)


muplot <- data.frame(date = dat.full[1:(t+L0)], mu = mu50[1:(t+L0)])
list_out <- list( df_predict = df_predict, lt_predict=lt_predict, lt_summary=lt_summary, 
                  mu_plot = muplot, flag=flag, mod_chain_y = mod_chain_y, mod_chain_mu = mod_chain_mu)

### saveRDS
results_directory = "/home/marcosop/Covid/app_COVID19/STpredictions/"
# results_directory = 'C:/Users/ricar/Dropbox/covid19/R/predict/'
file_id <- ifelse(uf$state[s]=='BR', colnames(Y)[3] , paste0(uf$state[s],'_',colnames(Y)[3],'e'))
saveRDS(list_out, file=paste0(results_directory,'Brazil_',file_id,'.rds'))

}
