rm(list=ls())

setwd("/home/marcosop/Covid/R/STAN")

###################################################################
### Packages
###################################################################
library(dplyr)
library(tidyr)
library(matrixStats)
library(data.table)

dir_rds <- "/home/marcosop/Covid/app_COVID19/STpredictions" # <- DIRETORIO DO GITHUB

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
source("loadData.R")

covid19_confirm <- loadDataUS_cases("time_series_covid19_confirmed_US.csv", "confirmed")
covid19_deaths <- loadDataUS_deaths("time_series_covid19_deaths_US.csv", "deaths")

covid19 <- left_join(covid19_confirm,covid19_deaths, by=c('state','country','date'))

uf_pop = as_tibble(covid19 %>% dplyr::distinct(state,pop)) %>% select(state,pop) %>%
  filter(!state %in% c("Diamond Princess","Grand Princess"))

Y <- covid19 %>% filter(country=="US") %>%
  mutate(state = "US") %>%
  group_by(state,country,date) %>%
  summarise(n = sum(confirmed, na.rm=T),
            d = sum(deaths, na.rm=T)) %>%
  mutate(n_new = n - lag(n, default=0),
         d_new = d - lag(d, default=0)) %>%
  select(date, n, d, n_new, d_new, state, country) %>%
  arrange(date)


while(any(Y$n_new <0)){
  pos <- which(Y$n_new <0)
  for(j in pos){
    Y$n_new[j-1] = Y$n_new[j] + Y$n_new[j-1]
    Y$n_new[j] = 0
    Y$n[j-1] = Y$n[j]
  }
}

t = dim(Y)[1]


# br_pop <- read.csv("../pop/pop_BR.csv")
# pop <- br_pop$pop[which(br_pop$uf == "BR")]
uf  <- uf_pop$state
pop <- sum(uf_pop$pop)


# u = uf[1]
for (u in uf) {

  # rds import
  # data_uf <- readRDS(paste0(dir_rds,"/Brazil_",u,"_ne.rds"))
  data_uf <- readRDS(paste0(dir_rds,"/US_",u,"_ne.rds"))
  
  # create yfut_UF
  dates <- as.Date((Y$date[length(Y$date)]+1):max(data_uf$lt_predict$date), origin="1970-01-01")
  yfut_UF0 <- data.frame(date = dates, UF = u)
  {if (u == uf[1]) {
    yfut_UF <- yfut_UF0 %>% bind_cols(as_tibble(t(data_uf$mod_chain_y)))
  } else {
    yfut_UF <- yfut_UF %>% bind_rows(yfut_UF0 %>% bind_cols(as_tibble(t(data_uf$mod_chain_y))))
  }}
  
  # create mu_UF
  dates <- data_uf$mu_plot$date
  mu_UF0 <- data.frame(date = dates, UF = u)
  {if (u == uf[1]) {
    mu_UF <- mu_UF0 %>% bind_cols(as_tibble(t(data_uf$mod_chain_mu)))
  } else {
    mu_UF <- mu_UF %>% bind_rows(mu_UF0 %>% bind_cols(as_tibble(t(data_uf$mod_chain_mu))))
  }}
  
  #print(which(u==uf$state))
}

i = 4 
# names(yfut_UF)
yfut_UF_df <- data.table(yfut_UF %>% dplyr::select(-UF))
yfut_sum <- yfut_UF_df[, lapply(.SD, sum, na.rm=T), by=date]

# names(mu_UF)
mu_UF_df <- data.table(mu_UF %>% dplyr::select(-UF)) # dim(mu_UF_df)
mu_sum <- mu_UF_df[, lapply(.SD, sum, na.rm=T), by=date]
# mu_US$date


mod_chain_y = t(yfut_sum[,-1])
mod_chain_cumy = rowCumsums(mod_chain_y) + Y[[i-2]][t]
L0 = 14  

### list output
df_predict <- data.frame( date = as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01"),
                          q25  = colQuantiles(mod_chain_cumy[,1:L0], prob=.025),
                          med  = colQuantiles(mod_chain_cumy[,1:L0], prob=.5),
                          q975 = colQuantiles(mod_chain_cumy[,1:L0], prob=.975),
                          m    = colMeans(mod_chain_cumy[,1:L0]))
row.names(df_predict) <- NULL 
lt_predict <- lt_summary <- NULL

#long term
L0 = 300

#acha a curva de quantil 
if(Y[[i-2]][t] > 1000){
  #acha a curva de quantil 
  lowquant <- colQuantiles(mod_chain_y[,1:L0], prob=.025)
  medquant <- colQuantiles(mod_chain_y[,1:L0], prob=.5)
  highquant <- colQuantiles(mod_chain_y[,1:L0], prob=.975)
} else{
  lowquant <- c(Y[[i-2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.025))
  lowquant <- (lowquant-lag(lowquant,default=0))[-1]
  medquant <- c(Y[[i-2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.5))
  medquant <- (medquant-lag(medquant,default=0))[-1]
  highquant <- c(Y[[i-2]][t],colQuantiles(mod_chain_cumy[,1:L0], prob=.975))
  highquant <- (highquant-lag(highquant,default=0))[-1]
}

NTC25 =sum(lowquant)+Y[[i-2]][t]
NTC500=sum(medquant)+Y[[i-2]][t]
NTC975=sum(highquant)+Y[[i-2]][t]

#flag
cm <- pop * 0.08
ch <- pop * 0.12 
flag <- 0 #tudo bem
if (NTC500 > cm) {
  flag <- 2 #nao plotar
} else {
  if(NTC975 > ch){flag <- 1; NTC25 <- NTC975 <- NULL}
} #plotar so mediana

#vetor de data futuras e pega a posicao do maximo do percentil 25.
dat.vec <- as.Date((max(Y$date)+1):(max(Y$date)+L0), origin="1970-01-01")
dat.full <- c(Y[[1]],dat.vec)

index_week <- which(!(weekdays(dat.full) %in% c("domingo", "segunda"))) #change

Dat25 <- Dat500 <- Dat975 <- NULL
dat.low.end <- dat.med.end <- dat.high.end <- NULL

# mod_chain_mu = as.matrix(mod_chain[mu_pos])
mod_chain_mu <- as.matrix(t(mu_sum[,-1]))

mu50 <- apply(mod_chain_mu,2,quantile, probs=0.5)
Dat500 <- dat.full[which.max(mu50[1:(t+L0)])]

q <- .99
med.cum <- mu50
med.cum <- colCumsums(as.matrix(med.cum))
med.cum <- med.cum/med.cum[length(med.cum)]
med.end <- which(med.cum - q > 0)[1]
# dat.med.end <- dat.vec[med.end]
dat.med.end <- dat.full[med.end]

if(flag == 0){
  #definicao do pico usando a curva das medias
  mu25 <- apply(mod_chain_mu,2,quantile, probs=0.025)
  mu975 <- apply(mod_chain_mu,2,quantile, probs=.975)
  
  #posMax.q25 <- which.max(mu25[1:(t+L0)]) 
  posMax.q25 <- which.max(mu25) 
  aux <- mu975 - mu25[posMax.q25] 
  aux2 <- aux[posMax.q25:length(aux)] 
  val <- ifelse(length(aux2[aux2<0]) > 0, min(aux2[aux2>0]), aux[length(aux)])
  dat.max <- which(aux == val)
  
  aux <- mu975 - mu25.aux
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
  
  ### saveRDS
  results_directory = "/home/marcosop/Covid/app_COVID19/STpredictions/"
  file_id <- colnames(Y)[i-2]
  # saveRDS(list_out, file=paste0(results_directory,'Brazil_',file_id,'.rds'))
  saveRDS(list_out, file=paste0(results_directory,'US_',file_id,'.rds'))
  
}

#####################################################################################################
# }
#####################################################################################################
