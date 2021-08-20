rm(list=ls())
setwd("/home/marcosop/Covid/R/STAN")

###################################################################
### Packages
###################################################################
library(PandemicLP)
library(foreach)
library(doMC)
library(covid19br)
library(dplyr)

Sys.setenv(LANGUAGE='en')
rstan_options(auto_write = TRUE)

###################################################################
### Data sets: https://github.com/CSSEGISandData
###################################################################
covid19 <- downloadCovid19(level = "states") %>%
  select(date,n=accumCases,d=accumDeaths,n_new=newCases, d_new=newDeaths,state) %>%
  arrange(state,date) 

uf <- distinct(covid19,state)

br_pop <- read.csv("../pop/pop_BR.csv")


state_list <- c("AM", "MA", "MG", "MT", "PE",
                "PR", "RN", "RS", "SP", "TO") # 10

#register cores
#registerDoMC(cores = detectCores()-1)    # Alternativa Linux
#registerDoMC(cores = 15)    # Alternativa Linux
registerDoMC(cores = min(63,length(state_list)))    # Alternativa Linux

obj <- foreach(s = 1:length(state_list)) %dopar% {
  
  estado <- state_list[s]
  data <- covid19 %>% filter(state== estado) %>% 
          select(date=date, cases=n, deaths=d, new_cases=n_new, new_deaths=d_new,-state)
  
  #remove duplicated data
  {if(sum(duplicated(data$date)) > 0){
    data <- data[-which(duplicated(data$date)),]
  }}
  
  while(any(data$new_deaths <0)){
    pos <- which(data$new_deaths <0)
    for(j in pos){
      data$new_cases[j-1] = data$new_deaths[j] + data$new_deaths[j-1]
      data$new_deaths[j] = 0
    }
  }
  

  pop <- br_pop$pop[which(br_pop$uf == estado)]
  names <- paste("Brazil",estado,sep="_")
  
  covid_state <- list(data=as.data.frame(data), name = names, population = pop)

  nwaves = 3
  init <- list(
    list(a=rep(150,nwaves), b = rep(1,nwaves), c = rep(0.5,nwaves), 
         alpha=rep(0.01,nwaves), delta=round(seq(1,nrow(covid_state$data),length.out = nwaves+1),0)[-(nwaves+1)], 
         d_1=rep(1,nwaves), d_2=rep(1,nwaves),d_3=rep(1,nwaves))
  )
  
  mod <- pandemic_model(covid_state,case_type = "deaths", p = 0.08*0.25,
                        seasonal_effect=c("sunday","monday"),n_waves = nwaves, 
                        warmup = 5e3, thin = 3, sample_size = 1e3,
                        init=init, covidLPconfig = FALSE) # run the model
  
  pred <- posterior_predict(mod,horizonLong = 1000,horizonShort = 14) # do predictions
  
  stats <- pandemic_stats(pred) # calculate stats
  stats[[1]] <-NULL # removing the data (the app use the data coming from other object)
  names(stats) <- c("df_predict","lt_predict","lt_summary","mu_plot")
  
  names(stats$df_predict) <- c("date", "q25",  "med",  "q975", "m") 
  names(stats$lt_predict) <- c("date", "q25",  "med",  "q975", "m") 
  names(stats$lt_summary) <- c("NTC25","NTC500","NTC975","high.dat.low","high.dat.med","high.dat.upper","end.dat.low",
                               "end.dat.med","end.dat.upper") 
  
  list_out <- list( df_predict = stats$df_predict, lt_predict=stats$lt_predict, lt_summary=stats$lt_summary, 
                    mu_plot = stats$mu_plot, residuals = cbind(mod$nominal_errors, mod$relative_errors), flag = 0)
  
  
  ### saveRDS
  results_directory = "/home/marcosop/Covid/app_COVID19/STpredictions/"
  names(covid_state$data) <- c("date","n","d","n_new","d_new")
  file_id <- paste0(state_list[s],'_',colnames(covid_state$data)[3],'e')
  saveRDS(list_out, file=paste0(results_directory,'Brazil_',file_id,'.rds'))
  
  
  ### saveRDS - THE POSTERIOR PREDICT (posterior_predict object)
  results_directory = "/home/marcosop/TMP/STaux/"
  file_id <- paste0(state_list[s],'_posterior_predict_',colnames(covid_state$data)[3],'e')
  saveRDS(pred,file = paste0(results_directory,file_id,'.rds'))
  
  ### saveChain
  results_directory = "/home/marcosop/Covid/chains/"
  file_id <- paste0(state_list[s],'_',colnames(covid_state$data)[3],'e')
  saveRDS(mod, file=file_id)
}
