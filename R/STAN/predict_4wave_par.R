rm(list=ls())
setwd("/home/marcosop/Covid/R/STAN")

###################################################################
### Packages
###################################################################
library(PandemicLP)
library(foreach)
library(doMC)

Sys.setenv(LANGUAGE='en')
rstan_options(auto_write = TRUE)

###################################################################
### Data sets: https://github.com/CSSEGISandData
###################################################################
countrylist <- c("Bolivia", "Costa Rica", "France", "India", "Iraq",
                 "Mexico", "Paraguay", "Peru", "Russia", "Saudi Arabia",
                 "South Africa", "Sweden", "Switzerland", "Turkey") # 14

#register cores
#registerDoMC(cores = detectCores()-1)    # Alternativa Linux
registerDoMC(cores = min(63,length(countrylist)))    # Alternativa Linux


obj <- foreach(s = 1:length(countrylist)) %dopar% {
  
  country_name <- countrylist[s]
  covid_country <- load_covid(country_name=country_name) # load data 
  
  nwaves = 4
  init <- list(
    list(a=rep(150,nwaves), b = rep(1,nwaves), c = rep(0.5,nwaves), 
         alpha=rep(0.01,nwaves), delta=c(1,100,250,400))
  )
  
  mod <- pandemic_model(covid_country,case_type = "confirmed", p = 0.08,
                        n_waves = nwaves, 
                        warmup = 10e3, thin = 3, sample_size = 1e3,
                        init=init, covidLPconfig = FALSE) # run the model
  
  pred <- posterior_predict(mod,horizonLong = 1000,horizonShort = 14) # do predictions
  
  stats <- pandemic_stats(pred) # calculate stats
  stats[[1]] <-NULL # removing the data (the app use the data coming from other object)
  names(stats) <- c("df_predict","lt_predict","lt_summary","mu_plot")
  
  names(stats$df_predict) <- c("date", "q25",  "med",  "q975", "m") 
  names(stats$lt_predict) <- c("date", "q25",  "med",  "q975", "m") 
  names(stats$lt_summary) <- c("NTC25","NTC500","NTC975","high.dat.low","high.dat.med","high.dat.upper","end.dat.low",
                               "end.dat.med","end.dat.upper") 
  
  list_out <- list(df_predict = stats$df_predict, lt_predict=stats$lt_predict, lt_summary=stats$lt_summary, 
                   mu_plot = stats$mu_plot, flag = 0)
  name.to.save <- gsub(" ", "-", country_name)
  
  ### saveRDS
  results_directory = "/home/marcosop/Covid/app_COVID19/STpredictions/"
  names(covid_country$data) <- c("date","n","d","n_new","d_new")
  name.file <- paste0(results_directory,name.to.save,'_',colnames(covid_country$data)[2],'.rds') 
  saveRDS(list_out, file=name.file)
}
