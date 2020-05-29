
############  SCRIPT PARCIAL   PAÍSES:  CASOS NOVOS CONFIRMADOS

rm(list=ls())

setwd("/run/media/marcos/OS/UFMG/Pesquisa/Covid/R/STAN")

###################################################################
### Packages
###################################################################
library(dplyr)
library(tidyr)
library(matrixStats)
library(mcmcplots)
library(MCMCvis)
library(foreach)
library(doMC)
library(rstan)                              

###################################################################
### Data sets: https://github.com/CSSEGISandData
###################################################################
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
source("loadData.R")

covid19_confirm <- loadData("time_series_covid19_confirmed_global.csv", "confirmed")
# covid19_recover <- loadData("time_series_covid19_recovered_global.csv", "recovered")
covid19_deaths <- loadData("time_series_covid19_deaths_global.csv", "deaths")

covid19 <- covid19_confirm %>%  left_join(covid19_deaths)

#countrylist = "Korea, South"
countrylist <- c("Argentina","Australia","Belgium","Bolivia","Canada","Chile","China","Colombia","Ecuador","France","Germany","Greece", "India", "Ireland", "Italy", "Japan", "Korea, South", "Mexico", "Netherlands", "New Zealand", "Norway", "Peru", "Paraguay", "Poland", "Portugal", "Russia", "South Africa", "Spain","United Kingdom", "Uruguay", "Sweden", "Switzerland", "US", "Turkey", "Venezuela")                    

#countrylist <- c("Argentina","Bolivia","Canada","Chile","Colombia","Ecuador", "Greece", "India", "Japan", "Korea, South", "Mexico", "Peru", "Paraguay", "Poland", "Russia", "South Africa", "United Kingdom", "Uruguay", "Sweden", "US", "Venezuela")                    

country_pop <- read.csv("../pop/pop_WR.csv")

#register cores
registerDoMC(cores = detectCores()-1)    # Alternativa Linux

#complie stan model
model="stan_model_poisson_gen.stan"    #modelo STAN 
mod<- try(stan_model(file = model,verbose=FALSE))

if(class(mod) == "try-error") stop("STAN DID NOT COMPILE")

#for(country_name in countrylist){
obj <- foreach(s = 1:length(countrylist) ) %dopar% {
  
  country_name <- countrylist[s]
  
  pop <- country_pop$pop[which(country_pop$country == country_name)]
  
   covid_states <- covid19 %>% filter(country==country_name) %>%
          mutate(confirmed_new = confirmed - lag(confirmed, default=0),
          # recovered_new = recovered - lag(recovered, default=0),
          deaths_new = deaths - lag(deaths, default=0)) %>%
          arrange(date,state)

   covid_country <- covid_states %>% group_by(date) %>%
          summarize(n = sum(confirmed, na.rm=T),
              d = sum(deaths, na.rm=T),
              n_new = sum(confirmed_new, na.rm=T),
              d_new = sum(deaths_new, na.rm=T)) %>%
              arrange(date) %>% filter(date>='2020-01-23')

#   covid_country <- covid19 %>% filter(country==country_name) %>%
#          mutate(n_new = confirmed - lag(confirmed, default=0),
#          d_new = deaths - lag(deaths, default=0)) %>%
#          rename(n = confirmed,
#                 d = deaths) %>%
#          arrange(date) %>% filter(date>='2020-01-01') %>% 
#          select(-c("state","country"))

  # covid_country %>% print(n=Inf)
  
  ###########################################################################
  ###### JAGS/STAN
  ###########################################################################
  Y = covid_country
  
  while(any(Y$n_new <0)){
    pos <- which(Y$n_new <0)
    for(j in pos){
      Y$n_new[j-1] = Y$n_new[j] + Y$n_new[j-1]
      Y$n_new[j] = 0
      Y$n[j-1] = Y$n[j]
    }
  }

t = dim(Y)[1]

#source("jags_poisson.R")

i = 4 # (2: confirmed, 3: deaths, 4: new confirmed, 5: new deaths)
L = 300
#t0 = Sys.time()


#use static to provide initial values
  params = c("a","b","c","f","mu")
  
  burn_in= 2e3
  lag= 3
  sample_size= 1e3
  number_iterations= burn_in + lag*sample_size
  number_chains= 1
  
  data_stan = list(y=Y[[i]], n=t, L=L, pop=1.1*pop)
  
  init <- list(
    list(a = 100, b1 = log(1), c = .5, f = 1)
  )

  mod_sim<- try(sampling(object = mod, data = data_stan,
                         pars = params,
                         chains = number_chains,
                         init = init,
                         iter = number_iterations, warmup = burn_in, thin = lag, 
                         control = list(max_treedepth = 15, adapt_delta=0.995),
                         verbose = FALSE, open_progress=FALSE, show_messages=FALSE))

if(class(mod_sim) != "try-error"){
  mod_chain = as.data.frame(mod_sim)
  # names(mod_chain)
  
  a_pos = "a"
  b_pos = "b"
  c_pos = "c"
  f_pos = "f"
  #mu_pos = c(paste0("mu[",1:t,"]"),paste0("mufut[",1:L,"]"))
  mu_pos = paste0("mu[",1:t,"]")
  yfut_pos = paste0("yfut[",1:L,"]")

  source("posterior_sample.R")
  fut <- predL(L=L,t,pop,Y[[2]][t],c(mod_chain[[a_pos]]),c(mod_chain[[b_pos]]),c(mod_chain[[c_pos]]),c(mod_chain[[f_pos]]))

    
  mod_chain_y = fut$y.fut
  #mod_chain_y = as.matrix(mod_chain[yfut_pos])
  mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[2]][t]

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
  L0 = 300
  
  #acha a curva de quantil 
    if(Y[[2]][t] > 1000){
      #acha a curva de quantil 
      lowquant <- colQuantiles(mod_chain_y[,1:L0], prob=.025)
      medquant <- colQuantiles(mod_chain_y[,1:L0], prob=.5)
      highquant <- colQuantiles(mod_chain_y[,1:L0], prob=.975)
    }
    else{
      lowquant <- c(Y[[2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.025))
      lowquant <- (lowquant-lag(lowquant,default=0))[-1]
      medquant <- c(Y[[2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.5))
      medquant <- (medquant-lag(medquant,default=0))[-1]
      highquant <- c(Y[[2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.975))
      highquant <- (highquant-lag(highquant,default=0))[-1]
    }
  
  NTC25 =sum(lowquant)+Y[[2]][t]
  NTC500=sum(medquant)+Y[[2]][t]
  NTC975=sum(highquant)+Y[[2]][t]
  
  
  ##flag
  cm <- pop * 0.05
  ch <- pop * 0.055 
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
  med.cum <- c(medquant[1]+Y[[2]][t],medquant[2:length(medquant)])
  med.cum <- colCumsums(as.matrix(med.cum))
  med.cum <- med.cum/med.cum[length(med.cum)]
  med.end <- which(med.cum - q > 0)[1]
  dat.med.end <- dat.vec[med.end]
  
  if(flag == 0){
    #definicao do pico usando a curva das medias
    mu25 <- apply(mod_chain_mu,2,quantile, probs=0.025)
    mu975 <- apply(mod_chain_mu,2,quantile, probs=.975)
    
    posMax.q25 <- which.max(mu25[1:(t+L0)]) 
    aux <- mu975 - mu25[posMax.q25]
    aux2 <- aux[posMax.q25:(t+L0)]
    val <- min(aux2[aux2>0]) 
    dat.max <- which(aux == val)
    
    aux <- mu975 - mu25[posMax.q25]
    aux2 <- aux[1:posMax.q25]
    val <- min(aux2[aux2>0]) 
    dat.min <- which(aux == val)
    
    Dat25 <- dat.full[dat.min]
    Dat975 <- dat.full[dat.max]
    
    #calcula o fim da pandemia
    low.cum <- c(lowquant[1]+Y[[2]][t],lowquant[2:length(lowquant)])
    low.cum <- colCumsums(as.matrix(low.cum))
    low.cum <- low.cum/low.cum[length(low.cum)]
    low.end <- which(low.cum - q > 0)[1]
    dat.low.end <- dat.vec[low.end]
    
    high.cum <- c(highquant[1]+Y[[2]][t],highquant[2:length(highquant)])
    high.cum <- colCumsums(as.matrix(high.cum))
    high.cum <- high.cum/high.cum[length(high.cum)]
    high.end <- which(high.cum - q > 0)[1]
    dat.high.end <- dat.vec[high.end]
  }
  
  lt_predict <- data.frame( date = dat.vec,
                            q25  = lowquant,
                            med  = medquant,
                            q975 = highquant,
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
  
  muplot <- data.frame(date = dat.full, mu = mu50[1:(t+L0)])
  list_out <- list( df_predict = df_predict, lt_predict=lt_predict, lt_summary=lt_summary, mu_plot = muplot, flag=flag)
  name.to.save <- gsub(" ", "-", country_name)
  
  ### saveRDS
  results_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/"
  #results_directory = getwd()#'C:/Users/ricar/Dropbox/covid19/R/predict/'
  name.file <- paste0(results_directory,name.to.save,'_',colnames(Y)[2],'.rds')
  saveRDS(list_out, file=name.file)
  
  source("mcmcplot_country.R")
  report_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/reports"
  #mcmcplot_country(mcmcout = mod_sim, parms = c(paste0("a[",t,"]"), paste0("b[",t,"]"), paste0("c[",t,"]")),
  mcmcplot_country(mcmcout = MCMCchains(object = mod_sim, mcmc.list = TRUE), parms = c("a", "b", "c", "f"),
                   dir = report_directory,
                   filename = paste0(country_name,'_',colnames(Y)[i],'_diagnostics'),
                   heading = paste0(country_name,'_',colnames(Y)[i]),
                   extension = "html", greek = TRUE,
                   country = country_name,
                   type = colnames(Y)[i])
}
else print(paste0("ERROR:",country_name))
}
