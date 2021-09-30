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
source("jags_poisson.R")
i = 2 # (2: confirmed, 3: deaths)
L = 200
params = c("a","b","c","assint","yfut")

for ( s in 1:dim(uf)[1] ) {

  #t0 = Sys.time()
  
  Y = covid19 %>% filter(state==uf$state[s])
  t = dim(Y)[1]
  data_jags = list(y=Y[[i]], t=t, L=L)
  nc = 3
  ni = 5e4
  
#  set.seed(100)
  mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=nc)
  update(mod, n.iter=5e3)
  mod_sim = try(coda.samples(model=mod, variable.names=params, n.iter=ni))
 
if(class(mod_sim) != "try-error"){ 
  mod_chain = as.data.frame(do.call(rbind, mod_sim))
  # names(mod_chain)
  
  npar = 4
  mod_chain_y = as.matrix(mod_chain %>% select(-names(mod_chain)[1:npar]))
  mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[i]][t]
  
  
  ### list output
  L0 = 14
  df_predict <- data.frame( date = as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01"),
                            q25  = colQuantiles(mod_chain_cumy[,1:L0], prob=.025),
                            med  = colQuantiles(mod_chain_cumy[,1:L0], prob=.5),
                            q975 = colQuantiles(mod_chain_cumy[,1:L0], prob=.975),
                            m    = colMeans(mod_chain_cumy[,1:L0]))
  row.names(df_predict) <- NULL
  
  list_out <- list( df_predict = df_predict )
  
  
  ### saveRDS
  results_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/"
  file_id <- ifelse(uf$state[s]=='BR', colnames(Y)[i] , paste0(uf$state[s],'_',colnames(Y)[i],'e'))
  saveRDS(list_out, file=paste0(results_directory,'Brazil_',file_id,'.rds'))
  
  
  ### report
  source("mcmcplot_country.R")
  report_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/reports"
  #report_directory = 'C:/Users/ricar/Dropbox/covid19/R/predict/report'
  mcmcplot_country(mcmcout = mod_chain, parms = c("a", "b", "c", "assint"),
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
else print(paste0("ERROR:",country_name))

}


