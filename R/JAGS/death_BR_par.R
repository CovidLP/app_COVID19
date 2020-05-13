rm(list=ls())

setwd("/run/media/marcos/OS/UFMG/Pesquisa/Covid/R/JAGS")

###################################################################
### Packages
###################################################################
library(dplyr)
library(tidyr)
library("rjags")
library(matrixStats)
library(mcmcplots)
library(foreach)
library(doMC)
###################################################################
### Data sets: https://github.com/CSSEGISandData
###################################################################
baseURLbr = "https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados"

covid19uf <- read.csv(file.path(baseURLbr,"EstadosCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
  rename(state = estado,
         date = data,
         n = casos.acumulados,
         d = obitos.acumulados,
         n_new = novos.casos,
         d_new = obitos.novos) %>%
  mutate(date = as.Date(date)) %>%
  select(date, n, d, -n_new, -d_new, state) %>%
  arrange(state,date) %>% filter(date>='2020-01-23')

covid19br <- read.csv(file.path(baseURLbr,"BrasilCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
  mutate(state = 'BR') %>%
  rename(date = data,
         n = casos.acumulados,
         d = obitos.acumulados,
         n_new = novos.casos,
         d_new = obitos.novos) %>%
  mutate(date = as.Date(date)) %>%
  select(date, n, d, -n_new, -d_new, state) %>%
  arrange(date) %>% filter(date>='2020-01-23')

covid19 <- bind_rows(covid19uf,covid19br)
uf <- distinct(covid19,state)

br_pop <- read.csv("../pop/pop_BR.csv")

###########################################################################
###### JAGS 
###########################################################################
#register cores
registerDoMC(cores = detectCores()-1)    # Alternativa Linux

#for ( s in 1:dim(uf)[1] ) {
obj <- foreach( s = 1:dim(uf)[1] ) %dopar% {
  #obj <- foreach( s = 1:3 ) %dopar% {
  source("jags_poisson.R")
  
  i = 5 # (2: confirmed, 3: deaths, 4: new confirmed, 5: new deaths)
  L = 300
  #t0 = Sys.time()
  estado = uf$state[s]
    
  Y <- covid19 %>% filter(state==estado) %>%
          mutate(n_new = n - lag(n, default=0),
          d_new = d - lag(d, default=0)) %>%
          select(date, n, d, n_new, d_new, state) %>%
          arrange(date) %>% filter(date>='2020-01-23')

  #Y = covid19 %>% filter(state==uf$state[s])

  while(any(Y$d_new <0)){
     pos <- which(Y$d_new <0)
     for(j in pos){
        Y$d_new[j-1] = Y$d_new[j] + Y$d_new[j-1]
        Y$d_new[j] = 0
        Y$d[j-1] = Y$d[j]
     }
   }

  pop <- br_pop$pop[which(br_pop$uf == uf$state[s])]

  t = dim(Y)[1]
  
  #use static to provide initial values
  params = c("a","b","c","f","yfut","mu")
  nc = 1 # 3
  nb = 90e3 # 5e4
  thin = 10
  ni = 10e3 # 5e4
  data_jags = list(y=Y[[i]], t=t, L=L)
  mod = try(jags.model(textConnection(mod_string_new), data=data_jags, n.chains=nc, n.adapt=nb, quiet=TRUE))
  try(update(mod, n.iter=ni, progress.bar="none"))
  mod_sim = try(coda.samples(model=mod, variable.names=params, n.iter=ni, thin=thin,progress.bar="none"))
  
  if(class(mod_sim) != "try-error" && class(mod) != "try-error"){
    
    mod_chain = as.data.frame(do.call(rbind, mod_sim))
    
    a_pos = "a"
    b_pos = "b"
    c_pos = "c"
    f_pos = "f"
    mu_pos = paste0("mu[",1:(t+L),"]")
    yfut_pos = paste0("yfut[",1:L,"]")
    L0 = 14
    
    mod_chain_y = as.matrix(mod_chain[yfut_pos])
    mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[3]][t]
    
    
    ### list output
    df_predict <- data.frame( date = as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01"),
                              q25  = colQuantiles(mod_chain_cumy[,1:L0], prob=.025),
                              med  = colQuantiles(mod_chain_cumy[,1:L0], prob=.5),
                              q975 = colQuantiles(mod_chain_cumy[,1:L0], prob=.975),
                              m    = colMeans(mod_chain_cumy[,1:L0]))
    row.names(df_predict) <- NULL 
    
    lt_predict <- lt_summary <- NULL
 
    #longterm
      L0 = 200
  
      #acha a curva de quantil 
      lowquant <- colQuantiles(mod_chain_y[,1:L0], prob=.025)
      medquant <- colQuantiles(mod_chain_y[,1:L0], prob=.5)
      highquant <- colQuantiles(mod_chain_y[,1:L0], prob=.975)

      NTC25 =sum(lowquant)+Y[[3]][t]
      NTC500=sum(medquant)+Y[[3]][t]
      NTC975=sum(highquant)+Y[[3]][t]


      ##flag
      cm <- pop * 0.025 * 0.12
      ch <- pop * 0.03 * 0.15
      flag <- 0 #tudo bem
      {if(NTC500 > cm) flag <- 2 #nao plotar
      else{if(NTC975 > ch){flag <- 1; NTC25 <- NTC975 <- NULL}}} #plotar so mediana

      #vetor de data futuras e pega a posicao do maximo do percentil 25.
      dat.vec <- as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01")
      dat.full <- c(Y[[1]],dat.vec)


      Dat25 <- Dat500 <- Dat975 <- NULL
      dat.low.end <- dat.med.end <- dat.high.end <- NULL

      mod_chain_mu = as.matrix(mod_chain[mu_pos])
      mu50 <- apply(mod_chain_mu,2,quantile, probs=0.5)
      Dat500 <- dat.full[which.max(mu50[1:(t+L0)])]

      q <- .99
      med.cum <- c(medquant[1]+Y[[3]][t],medquant[2:length(medquant)])
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
         low.cum <- c(lowquant[1]+Y[[3]][t],lowquant[2:length(lowquant)])
         low.cum <- colCumsums(as.matrix(low.cum))
         low.cum <- low.cum/low.cum[length(low.cum)]
         low.end <- which(low.cum - q > 0)[1]
         dat.low.end <- dat.vec[low.end]

         high.cum <- c(highquant[1]+Y[[3]][t],highquant[2:length(highquant)])
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
         

    list_out <- list( df_predict = df_predict, lt_predict=lt_predict, lt_summary=lt_summary, mu_plot = mu50[1:(t+L0)], flag=flag)
        
    ### saveRDS
    results_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/"
    # results_directory = 'C:/Users/ricar/Dropbox/covid19/R/predict/'
    file_id <- ifelse(uf$state[s]=='BR', colnames(Y)[3] , paste0(uf$state[s],'_',colnames(Y)[3],'e'))
    saveRDS(list_out, file=paste0(results_directory,'Brazil_',file_id,'.rds'))
    
    ### report
    source("mcmcplot_country.R")
    report_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/reports"
    # report_directory = 'C:/Users/ricar/Dropbox/covid19/R/predict/report'
    #mcmcplot_country(mcmcout = mod_sim, parms = c(paste0("a[",t,"]"), paste0("b[",t,"]"), paste0("c[",t,"]")),
	    mcmcplot_country(mcmcout = mod_sim, parms = c("a", "b", "c", "f"),
		             dir = report_directory,
		             filename = paste0('Brazil_',file_id,'_diagnostics'),
		             heading = paste0('Brazil_',file_id),
		             extension = "html", greek = TRUE,
		             country = 'Brazil',
		             type = file_id)
    
    ### run time
    #run_time = round(as.numeric(Sys.time()-t0, units="mins"),2)
    #print(noquote(paste(run_time, "minutes to", uf$state[s])))
    
  }
  
  else print(paste0("ERROR:",uf$state[s]))
  
}

