rm(list=ls())

setwd("/run/media/marcos/OS/UFMG/Pesquisa/Covid/R")

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
  select(date, n, d, n_new, d_new, state) %>%
  arrange(state,date) %>% filter(date>'2020-02-01')

covid19br <- read.csv(file.path(baseURLbr,"BrasilCov19.csv"), check.names=FALSE, stringsAsFactors=FALSE) %>%
  mutate(state = 'BR') %>%
  rename(date = data,
         n = casos.acumulados,
         d = obitos.acumulados,
         n_new = novos.casos,
         d_new = obitos.novos) %>%
  mutate(date = as.Date(date)) %>%
  select(date, n, d, n_new, d_new, state) %>%
  arrange(date) %>% filter(date>'2020-02-01')

covid19 <- bind_rows(covid19uf,covid19br)
uf <- distinct(covid19,state)

# class(covid19)
# covid19 %>% tbl_df %>% print(n=Inf)

###########################################################################
###### JAGS 
###########################################################################
#register cores
registerDoMC(cores = detectCores()-1)    # Alternativa Linux

#for ( s in 1:dim(uf)[1] ) {
obj <- foreach( s = 1:dim(uf)[1] ) %dopar% {
  #obj <- foreach( s = 1:3 ) %dopar% {
  source("jags_poisson.R")
  
  i = 4 # (2: confirmed, 3: deaths, 4: new confirmed, 5: new deaths)
  L = 100
  #t0 = Sys.time()
  
  Y = covid19 %>% filter(state==uf$state[s])
  t = dim(Y)[1]
  
  #use static to provide initial values
  params = c("a","b","c","f")
  # Wa = 1e4
  # Wb = 1e4 
  # Wc = 1e4
  nc = 1 # 3
  nb = 20e3 # 5e4
  thin = 1
  ni = 1e3 # 5e4
  data_jags = list(y=Y[[i]], t=t)
  mod = jags.model(textConnection(mod_string_new), data=data_jags, n.chains=nc, n.adapt=nb, quiet=TRUE)
  update(mod, n.iter=ni, progress.bar="none")
  mod_sim = coda.samples(model=mod, variable.names=params, n.iter=ni, thin=thin,progress.bar="none")
  mod_chain = as.data.frame(do.call(rbind, mod_sim))
  
  a.init <- median(mod_chain[["a"]])
  b.init <- median(mod_chain[["b"]])
  c.init <- median(mod_chain[["c"]])
  f.init <- median(mod_chain[["f"]])
  
  model <- "mod_string_new_dm"
  
  params = c("a","b","c","f","mu","yfut","Wa")
  # Wa = 1e4
  # Wb = 1e4 
  # Wc = 1e4
  nc = 1 # 3
  nb = 90e3 # 5e4
  thin = 10
  ni = 10e3 # 5e4
  #data_jags = list(y=Y[[i]], t=t, Wa=Wa, Wb=Wb, Wc=Wc)
  data_jags = list(y=Y[[i]], t=t, L=L)
  
  inits=list(
    #list(wa = rep(-2.3,t), wb=rep(-13.82,t), wc=rep(-2.3,t)) #chain 1
    # list(wa = c(log(a.init),rep(0,t-1)), b=b.init, wc=c(log(c.init),rep(0,t-1)), Wa=Wa, Wc=Wc) #chain 1
    list( wa=c(log(a.init),rep(0,t+L-1)) , b=b.init , c=c.init , f1=log(f.init) )
  ) #end of inits list
  
  # set.seed(100)
  mod = jags.model(textConnection(get(model)), data=data_jags, inits=inits, n.chains=nc, n.adapt=nb, quiet=TRUE)
  update(mod, n.iter=ni, progress.bar="none")
  mod_sim = try(coda.samples(model=mod, variable.names=params, n.iter=ni, thin=thin,progress.bar="none"))
  
  if(class(mod_sim) != "try-error"){
    
    mod_chain = as.data.frame(do.call(rbind, mod_sim))
    # names(mod_chain)
    
    Wa_pos = 1
    a_pos = Wa_pos + 1:(t+L)
    b_pos = max(a_pos) + 1
    c_pos = b_pos + 1
    f_pos = c_pos + 1
    mu_pos = f_pos + 1:(t+L)
    yfut_pos = max(mu_pos) + 1:L
    
    L0 = 14
    # source("posterior_sample.R")
    # #future <- pred(L=L0, B=t, a=mod_chain[[t]], b=mod_chain[[2*t]], c=mod_chain[[3*t]], taua=Wa, taub=Wb, tauc=Wc)
    # future <- pred(L=L0, B=t, a=mod_chain[[paste0("a[",t,"]")]], b=mod_chain[["b"]], c=mod_chain[[paste0("c[",t,"]")]], taua=mod_chain[["Wa"]], taub=Wb, tauc=mod_chain[["Wc"]])
    # mod_chain_y = future[[1]]
    # mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[i]][t]
    
    mod_chain_y = as.matrix(mod_chain[yfut_pos])
    mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[2]][t]
    
    
    ### list output
    df_predict <- data.frame( date = as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01"),
                              q25  = colQuantiles(mod_chain_cumy[,1:L0], prob=.05),
                              med  = colQuantiles(mod_chain_cumy[,1:L0], prob=.5),
                              q975 = colQuantiles(mod_chain_cumy[,1:L0], prob=.95),
                              m    = colMeans(mod_chain_cumy[,1:L0]))
    row.names(df_predict) <- NULL 
    
    lt_predict <- lt_summary <- NULL
    if(TRUE){
      # #longterm
      L0 = 100
      # Wa = 1e25
      # Wb = 1e25 
      # Wc = 1e25
      
      # #faz a predicao de longo termo
      # #future <- pred(L=L0, B=t, a=mod_chain[[t]], b=mod_chain[[2*t]], c=mod_chain[[3*t]], taua=Wa, taub=Wb, tauc=Wc)
      # future <- pred(L=L0, B=t, a=mod_chain[[paste0("a[",t,"]")]], b=mod_chain[["b"]], c=mod_chain[[paste0("c[",t,"]")]], taua=Wa, taub=Wb, tauc=Wc)
      # mu_n_new =  future[[2]]
      # mod_cumMu = rowSums(mu_n_new) + Y[[i]][t]

      mu_n_new = as.matrix(mod_chain[mu_pos])[,-(1:t)]
      mod_cumMu = rowSums(mu_n_new)
            
      u <- sort(mod_cumMu)
      q <- c(round(nrow(mu_n_new)*.05,0),round(nrow(mu_n_new)*.5,0),round(nrow(mu_n_new)*.95,0))
      pos <- rep(0,length(q))
      for(k in 1:length(q)) pos[k] <- which(mod_cumMu == u[k])
      
      #vetor de data futuras e pega a posicao do maximo do percentil 25.
      dat.vec <- as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01")
      posMax.q25 <- which.max(mu_n_new[pos[1],])
      
      #minimos de dias no futuro para aceitar que o pico ainda não chegou
      Dat25 <- Dat500 <- Dat975 <- NULL
      days <- 5
      if(dat.vec[posMax.q25] > max(Y$date)+days){
        Dat25 <- dat.vec[posMax.q25]
        Dat500 <- dat.vec[which.max(mu_n_new[pos[2],])]
        Dat975 <- dat.vec[which.max(mu_n_new[pos[3],])]
      }
      
      lt_predict <- data.frame( date = dat.vec,
                                q25  = mu_n_new[pos[1],],
                                med  = mu_n_new[pos[2],],
                                q975 = mu_n_new[pos[3],])
      row.names(lt_predict) <- NULL
      lt_summary <- list(NTC25 =mod_cumMu[pos[1]],
                         NTC500=mod_cumMu[pos[2]],
                         NTC975=mod_cumMu[pos[3]],
                         Dat25=Dat25,
                         Dat500=Dat500,
                         Dat975=Dat975)
    }
    list_out <- list( df_predict = df_predict, lt_predict=lt_predict, lt_summary=lt_summary)
    
    #list_out <- list( df_predict = df_predict )
    
    
    ### saveRDS
    results_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/"
    # results_directory = 'C:/Users/ricar/Dropbox/covid19/R/predict/'
    file_id <- ifelse(uf$state[s]=='BR', colnames(Y)[i] , paste0(uf$state[s],'_',colnames(Y)[i],'e'))
    saveRDS(list_out, file=paste0(results_directory,'Brazil_',file_id,'.rds'))
    
    ### report
    source("mcmcplot_country.R")
    report_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/reports"
    # report_directory = 'C:/Users/ricar/Dropbox/covid19/R/predict/report'
    #mcmcplot_country(mcmcout = mod_sim, parms = c(paste0("a[",t,"]"), paste0("b[",t,"]"), paste0("c[",t,"]")),
    mcmcplot_country(mcmcout = mod_sim, parms = c(paste0("a[",t,"]"), paste0("b"), "c", "f", "Wa"),
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

