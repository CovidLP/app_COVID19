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
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
source("loadData.R")

covid19_confirm <- loadData("time_series_covid19_confirmed_global.csv", "confirmed")
# covid19_recover <- loadData("time_series_covid19_recovered_global.csv", "recovered")
covid19_deaths <- loadData("time_series_covid19_deaths_global.csv", "deaths")

covid19 <- covid19_confirm %>%  left_join(covid19_deaths)

#countrylist = "Korea, South"
#countrylist <- c("Argentina","Australia","Belgium","Bolivia","Canada","Chile","China","Colombia","Ecuador","France","Germany","Greece", "India", "Iran", "Ireland", "Italy", "Japan", "Korea, South", "Mexico", "Netherlands", "New Zealand", "Norway", "Peru", "Paraguay", "Poland", "Portugal", "Russia", "South Africa", "Spain","United Kingdom", "Uruguay", "Sweden", "Switzerland", "US", "Turkey", "Venezuela")                    

countrylist <- c("Argentina","Bolivia","Canada","Chile","Colombia","Ecuador", "Greece", "India", "Japan", "Korea, South", "Mexico", "Peru", "Paraguay", "Poland", "Russia", "South Africa", "United Kingdom", "Uruguay", "Sweden", "US", "Venezuela")                    

#register cores
registerDoMC(cores = detectCores()-1)    # Alternativa Linux

#for(country_name in countrylist){
obj <- foreach(s = 1:length(countrylist) ) %dopar% {

   country_name <- countrylist[s]

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
              arrange(date) %>% filter(date>'2020-02-01')

# covid_country %>% print(n=Inf)

###########################################################################
###### JAGS 
###########################################################################
  Y = covid_country
  t = dim(Y)[1]

  source("jags_poisson.R")

  i = 2 # (2: confirmed, 3: deaths)
  #L = 150
  #t0 = Sys.time()
  
  #use static to provide initial values
  params = c("a","b","c")
  Wa = 1e4
  Wb = 1e4 
  Wc = 1e4
  nc = 1 # 3
  nb = 20e3 # 5e4
  thin = 1
  ni = 1e3 # 5e4
  data_jags = list(y=Y[[i]], t=t)
  mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=nc, n.adapt=nb, quiet=TRUE)
  update(mod, n.iter=ni, progress.bar="none")
  mod_sim = coda.samples(model=mod, variable.names=params, n.iter=ni, thin=thin,progress.bar="none")
  mod_chain = as.data.frame(do.call(rbind, mod_sim))

  a.init <- median(mod_chain[["a"]])
  b.init <- median(mod_chain[["b"]])
  c.init <- median(mod_chain[["c"]])

  model <- "mod_string_dm3"

  params = c("a","b","c","Wa","Wc")
  Wa = 1e4
  Wb = 1e4 
  Wc = 1e4
  nc = 1 # 3
  nb = 90e3 # 5e4
  thin = 10
  ni = 10e3 # 5e4
  #data_jags = list(y=Y[[i]], t=t, Wa=Wa, Wb=Wb, Wc=Wc)
  data_jags = list(y=Y[[i]], t=t)

  inits=list(
   #list(wa = rep(-2.3,t), wb=rep(-13.82,t), wc=rep(-2.3,t)) #chain 1
   list(wa = c(log(a.init),rep(0,t-1)), b=b.init, wc=c(log(c.init),rep(0,t-1)), Wa=Wa, Wc=Wc) #chain 1
  ) #end of inits list

  # set.seed(100)
  mod = jags.model(textConnection(get(model)), data=data_jags, inits=inits, n.chains=nc, n.adapt=nb, quiet=TRUE)
  update(mod, n.iter=ni, progress.bar="none")
  mod_sim = try(coda.samples(model=mod, variable.names=params, n.iter=ni, thin=thin,progress.bar="none"))

  if(class(mod_sim) != "try-error"){
         mod_chain = as.data.frame(do.call(rbind, mod_sim))
         L0 = 14

         source("posterior_sample.R")
         #future <- pred(L=L0, B=t, a=mod_chain[[t]], b=mod_chain[[2*t]], c=mod_chain[[3*t]], taua=Wa, taub=Wb, tauc=Wc)
         future <- pred(L=L0, B=t, a=mod_chain[[paste0("a[",t,"]")]], b=mod_chain[["b"]], c=mod_chain[[paste0("c[",t,"]")]], taua=mod_chain[["Wa"]], taub=Wb, tauc=mod_chain[["Wc"]])
         mod_chain_y = future[[1]]
         mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[i]][t]

         ### list output
        
         df_predict <- data.frame( date = as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01"),
                                   q25  = colQuantiles(mod_chain_cumy[,1:L0], prob=.025),
                                   med  = colQuantiles(mod_chain_cumy[,1:L0], prob=.5),
                                   q975 = colQuantiles(mod_chain_cumy[,1:L0], prob=.975),
                                   m    = colMeans(mod_chain_cumy[,1:L0]))
         row.names(df_predict) <- NULL

         lt_predict <- lt_summary <- NULL
         if(FALSE){
#         {if(country_name %in% c("China","Canada","Japan","India")){
          #longterm
          L0 = 100
          Wa = 1e25
          Wb = 1e25 
          Wc = 1e25

        #faz a predicao de longo termo
          #future <- pred(L=L0, B=t, a=mod_chain[[t]], b=mod_chain[[2*t]], c=mod_chain[[3*t]], taua=Wa, taub=Wb, tauc=Wc)
          future <- pred(L=L0, B=t, a=mod_chain[[paste0("a[",t,"]")]], b=mod_chain[["b"]], c=mod_chain[[paste0("c[",t,"]")]], taua=Wa, taub=Wb, tauc=Wc)
          mu_n_new =  future[[2]]
          mod_cumMu = rowSums(mu_n_new) + Y[[i]][t]

          u <- sort(mod_cumMu)
          q <- c(round(nrow(mu_n_new)*.025,0),round(nrow(mu_n_new)*.5,0),round(nrow(mu_n_new)*.975,0))
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
#         }
#         else list_out <- list( df_predict = df_predict)
#        }
         name.to.save <- gsub(" ", "-", country_name)

         ### saveRDS
         results_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/"
         #results_directory = getwd()#'C:/Users/ricar/Dropbox/covid19/R/predict/'
         name.file <- paste0(results_directory,name.to.save,'_',colnames(Y)[i],'.rds')
         saveRDS(list_out, file=name.file)

         source("mcmcplot_country.R")
         report_directory = "/run/media/marcos/OS/UFMG/Pesquisa/Covid/app_COVID19/STpredictions/reports"
         #mcmcplot_country(mcmcout = mod_sim, parms = c(paste0("a[",t,"]"), paste0("b[",t,"]"), paste0("c[",t,"]")),
         mcmcplot_country(mcmcout = mod_sim, parms = c(paste0("a[",t,"]"), paste0("b"), paste0("c[",t,"]")),
                          dir = report_directory,
                          filename = paste0(country_name,'_',colnames(Y)[i],'_diagnostics'),
                          heading = paste0(country_name,'_',colnames(Y)[i]),
                          extension = "html", greek = TRUE,
                          country = country_name,
                          type = colnames(Y)[i])
    }
    else print(paste0("ERROR:",country_name))
}
