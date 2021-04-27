###################################################################
### Packages
###################################################################
library(PandemicLP)

#states <- state_list()
#uf <- states$state_abb

#Github directory where the data is stored
dir_rds <-  "/home/marcosop/TMP/STaux"

#read the posterior file
files <- list.files(dir_rds)
pos <- grep("_de",files)
files <- files[pos]
uf <- substr(files,1,2)
state_nm <- paste0(dir_rds,"/",files[1])
data_base <- readRDS(state_nm)
uf_2 <- uf[-1]

#state_nm <- paste0(dir_rds,'/',uf[1],"_posterior_predict_ne.rds")
#data_base <- readRDS(state_nm)
#uf_2 <- uf[-1]

#get the mean sample and set it to be dates x mcmc sample
mu_t <- t(data_base$pastMu)
#include the dates in the data frame
mu_final <- data.frame(data = data_base$data$date,mu_t)
names_mu <- names(data_base$pastMu)

#get hidden objects (necessary for the pandemic_stats function)
hidden_short_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandShortPred
hidden_long_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandLongPred
hidden_mu_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandMus

for (u in uf_2) {

  #rds import for the selected state
  state_nm <- paste0(dir_rds,'/',u,"_posterior_predict_de.rds")
  data_uf <- readRDS(state_nm)

  #sum the variables predictive_Long, predictive_Short and futMu -> considering the date
  if(max(data_base$data$date) == max(data_uf$data$date)){
    data_base$predictive_Long <- data_base$predictive_Long + data_uf$predictive_Long
    data_base$predictive_Short <- data_base$predictive_Short + data_uf$predictive_Short
    data_base$futMu <- data_base$futMu + data_uf$futMu
  } else {
    dif_data <- difftime(max(data_base$data$date), max(data_uf$data$date), units="days")

    predictive_long_uf <- data_uf$predictive_Long[-c(1:dif_data),]
    matrix_zero <- matrix(0,nrow = dif_data, ncol = ncol(predictive_long_uf))
    predictive_long_uf <- rbind(predictive_long_uf,matrix_zero)

    predictive_short_uf <- data_uf$predictive_Short[-c(1:dif_data),]
    matrix_zero <- matrix(0,nrow = dif_data, ncol = ncol(predictive_short_uf))
    predictive_short_uf <- rbind(predictive_short_uf,matrix_zero)

    futMu_uf <- data_uf$futMu[-c(1:dif_data),]
    matrix_zero <- matrix(0,nrow = dif_data, ncol = ncol(futMu_uf))
    futMu_uf <- rbind(futMu_uf,matrix_zero)

    data_base$predictive_Long <- data_base$predictive_Long + predictive_long_uf
    data_base$predictive_Short <- data_base$predictive_Short + predictive_short_uf
    data_base$futMu <- data_base$futMu + futMu_uf
  }

  #create a large data frame by concatenating samples for current state in the mean data frame
  mu_t <- t(data_uf$pastMu)
  mu_2 <- data.frame(data = data_uf$data$date,mu_t)
  names_mu <- c(names_mu,names(data_uf$pastMu))
  mu_final <- rbind(mu_final,mu_2)

  #merge datasets by date since they can differ its start
  data_base$data <- merge(data_base$data,data_uf$data, by = "date", all = TRUE)
  data_base$data[is.na(data_base$data)] = 0
  data_base$data$cases.x = data_base$data$cases.x + data_base$data$cases.y
  data_base$data$deaths.x = data_base$data$deaths.x + data_base$data$deaths.y
  data_base$data$new_cases.x = data_base$data$new_cases.x + data_base$data$new_cases.y
  data_base$data$new_deaths.x = data_base$data$new_deaths.x + data_base$data$new_deaths.y
  data_base$data <- data_base$data[,-c(6:9)]
  names(data_base$data) <- c("date","cases","deaths","new_cases","new_deaths")

  #sum hidden objects (necessary for the pandemic_stats function) and sum them
  if(max(data_base$data$date) == max(data_uf$data$date)){
    hidden_short_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandShortPred
    hidden_short_total <- hidden_short_total + hidden_short_uf
    hidden_long_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandLongPred
    hidden_long_total <- hidden_long_total + hidden_long_uf
    hidden_mu_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandMus
    hidden_mu_total <- hidden_mu_total + hidden_mu_uf
  } else {
    dif_data <- difftime(max(data_base$data$date), max(data_uf$data$date), units="days")
    hidden_short_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandShortPred
    hidden_short_uf <- hidden_short_uf[-c(1:dif_data),]
    matrix_zero <- matrix(0,nrow = dif_data, ncol = ncol(hidden_short_uf))
    hidden_short_uf <- rbind(hidden_short_uf,matrix_zero)
    hidden_short_total <- hidden_short_total + hidden_short_uf

    hidden_long_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandLongPred
    hidden_long_uf <- hidden_long_uf[-c(1:dif_data),]
    matrix_zero <- matrix(0,nrow = dif_data, ncol = ncol(hidden_long_uf))
    hidden_long_uf <- rbind(hidden_long_uf,matrix_zero)
    hidden_long_total <- hidden_long_total + hidden_long_uf

    hidden_mu_uf <- methods::slot(data_uf$fit,"sim")$fullPred$thousandMus
    hidden_mu_uf <- hidden_mu_uf[-c(1:dif_data),]
    matrix_zero <- matrix(0,nrow = dif_data, ncol = ncol(hidden_mu_uf))
    hidden_mu_uf <- rbind(hidden_mu_uf,matrix_zero)
    hidden_mu_total <- hidden_mu_total + hidden_mu_uf
  }
}

#create hidden object (necessary for the pandemic_stats function)
methods::slot(data_base$fit,"sim")$fullPred$thousandShortPred <- hidden_short_total
methods::slot(data_base$fit,"sim")$fullPred$thousandLongPred <- hidden_long_total
methods::slot(data_base$fit,"sim")$fullPred$thousandMus <- hidden_mu_total

#aggregate the mean samples
mu_final <- aggregate(. ~ data, data=mu_final, FUN=sum)
mu_final <- mu_final[,-1]
mu_final <- t(mu_final)
names_mu <- unique(names_mu)
colnames(mu_final) <- names_mu
data_base$pastMu <- mu_final

#calculate summary statistics
stats <- pandemic_stats(data_base) # calculate stats
stats[[1]] <- NULL # removing the data (the app use the data coming from other object)

#rename the lists and data.frame accordingly for the online application
names(stats) <- c("df_predict","lt_predict","lt_summary","mu_plot")
names(stats$df_predict) <- c("date", "q25",  "med",  "q975", "m")
names(stats$lt_predict) <- c("date", "q25",  "med",  "q975", "m")
names(stats$lt_summary) <- c("NTC25","NTC500","NTC975","high.dat.low","high.dat.med","high.dat.upper","end.dat.low",
                             "end.dat.med","end.dat.upper")

#prepare the list to be saved
list_out <- list(df_predict = stats$df_predict, lt_predict=stats$lt_predict, lt_summary=stats$lt_summary,
                 mu_plot = stats$mu_plot, flag=0)

i = 5 # for deaths
#saveRDS - aggregated by the states
results_directory = "/home/marcosop/Covid/app_COVID19/STpredictions/"
names(data_base$data) <- c("date","n","d","n_new","d_new")
file_id <- colnames(data_base$data)[i-2]
saveRDS(list_out, file=paste0(results_directory,'Brazil_',file_id,'.rds'))
#####################################################################################################
# }
#####################################################################################################

