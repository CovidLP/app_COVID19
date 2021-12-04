classify.flag = function(obs, adj, rem_saz = FALSE){
  
  colnames(obs) = c("date", "y")
  nn = nrow(obs)
  
  if(rem_saz){
    
    dados.aux = suppressMessages(full_join(obs, adj) %>%
                                   mutate(week_days = weekdays(as.Date(date))) %>%
                                   filter(!(week_days == "domingo" | week_days == "segunda-feira")) %>%
                                   mutate(y_s = rollmean(y, k = 20, fill = NA) %>% rollmean(k = 10, fill = NA),   # suavizando os dados
                                          dif_s = c(NA, y_s %>% diff()) %>% rollmean(k = 50, fill = NA),          # diferen?a suavizada 
                                          s1 = dif_s >= 0,
                                          s2 = dif_s < 0,
                                          id = c(NA, diff(s1 - s2)) ))
    
  } else{
    
    dados.aux = suppressMessages(full_join(obs, adj) %>%
                                   mutate(y_s = rollmean(y, k = 20, fill = NA) %>% rollmean(k = 10, fill = NA),   # suavizando os dados
                                          dif_s = c(NA, y_s %>% diff()) %>% rollmean(k = 50, fill = NA),          # diferen?a suavizada 
                                          s1 = dif_s >= 0,
                                          s2 = dif_s < 0,
                                          id = c(NA, diff(s1 - s2)) ))
    
  }
  
  maxx = max(dados.aux$y_s, na.rm = TRUE)
  
  aux = dados.aux %>% filter(id == 2 |id == -2) %>% arrange(desc(date))
  
  if(aux$id[1] == -2){ # se o ?ltimo ponto identificado ? um pico, a amplitude ? calculada com base na ?ltima observa??o do y suavizado
    aux = rbind(tail(dados.aux %>% filter(!is.na(y_s)), 1) %>% mutate(id = 2), aux) 
  }
  
  est = aux %>% mutate(amp_y = c(NA, diff(y_s)), 
                       amp_mu = c(NA, diff(mu)),
                       id_df = c(NA, diff(id)),
                       est = (amp_y - amp_mu)^2/maxx^2) %>%
    filter(id_df == -4) # amplitudes corretas
  
  est1 = (est %>% summarise(crit = sum(est)))$crit
  
  est1
  
}

classify.flag.old = function(obs, adj){
  
  colnames(obs) = c("date", "y")
  
  dados.aux = suppressMessages(full_join(obs, adj) %>%
                                 mutate(y_s = rollmean(y, k = 20, fill = NA) %>% rollmean(k = 10, fill = NA),   # suavizando os dados
                                        dif_s = c(NA, y_s %>% diff()) %>% rollmean(k = 50, fill = NA),          # diferença suavizada 
                                        id = c(NA, dif_s %>% sign() %>% diff()) ) )                               # identificador de mudança de comportamento
  
  maxx = max(dados.aux$y_s, na.rm = TRUE)
  
  aux = dados.aux %>% filter(id == 2 |id == -2) %>% arrange(desc(date))
  
  if(aux$id[1] == -2){ # se o último ponto identificado é um pico, a amplitude é calculada com base na última observação do y suavizado
    aux = rbind(tail(dados.aux %>% filter(!is.na(y_s)), 1) %>% mutate(id = 2), aux) 
  }
  
  est = aux %>% mutate(amp_y = c(NA, diff(y_s)), 
                       amp_mu = c(NA, diff(mu)),
                       id_df = c(NA, diff(id)),
                       est = (amp_y - amp_mu)^2/maxx^2) %>%
    filter(id_df == -4) # amplitudes corretas
  
  est1 = (est %>% summarise(crit = sum(est)))$crit
  
  est1
  
}

classify.flag.old2 = function(obs, adj){
  
  colnames(obs) = c("date", "y")
  
  dados = full_join(obs, adj) %>% mutate(y_s = rollmean(y, k = 20, fill = NA) %>% rollmean(k = 10, fill = NA),   # suavizando os dados
                                         dif_s = c(NA, y_s %>% diff()) %>% rollmean(k = 50, fill = NA),          # diferença suavizada 
                                         id = c(NA, dif_s %>% sign() %>% diff()) )                               # identificador de mudança de comportamento
  
  aux = dados %>% filter(id == 2 |id == -2) %>% arrange(desc(date))
  
  if(aux$id[1] == -2){ # se o último ponto identificado é um pico, a amplitude é calculada com base na última observação do y suavizado
    aux = rbind(tail(dados %>% filter(!is.na(y_s)), 1) %>% mutate(id = 2), aux) 
  }
  
  est = (aux %>% mutate(amp_y = c(NA, diff(y_s)), 
                        amp_mu = c(NA, diff(mu)),
                        id_df = c(NA, diff(id)),
                        est = (amp_y - amp_mu)^2/max(y_s)^2) %>%
           filter(id_df == -4) %>% # amplitudes corretas
           summarise(crit = sum(est)))$crit
  
  #ifelse(est <= 0.2, 0, ifelse(est <= 0.5, 1, 2))
  est
}

nwaves <- function(country = TRUE, new_cases = TRUE){
  
  country_name = c("Argentina", "Australia", "Belgium", "Bolivia", "Canada", "Chile", "China", "Colombia", "Costa Rica", "Ecuador", "Ethiopia", "France", "Germany", "Greece", "Guatemala", "Honduras", "India", "Indonesia", "Iraq", "Ireland", "Italy", "Japan", "South Korea", "Mexico", "Morocco", "Netherlands", "New Zealand", "Norway", "Panama", "Paraguay", "Peru", "Poland", "Portugal", "Romania", "Russia", "Saudi Arabia", "South Africa", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Uruguay", "United States of America", "Venezuela")
  state_name = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
  
  if(country){
    
    data = apply(matrix(country_name), 1, load_covid)
    
    if(new_cases){
      
      npeaks = data %>% lapply(function(x) x$data$new_cases) %>%
        lapply(function(x) nrow(findpeaks(x, minpeakdistance  = 95)))
      
    } else{
      
      npeaks = data %>% lapply(function(x) x$data$new_deaths) %>%
        lapply(function(x) nrow(findpeaks(x, minpeakdistance  = 95)))
      
    }
    
    waves = npeaks %>% unlist
    nwaves = waves %>% unique %>% sort
    
    country_list = list()
    for(i in 1:length(nwaves)){
      pos = which(waves == nwaves[i])
      country_list[[i]] = country_name[pos]
    }
    names(country_list) = paste("wave", nwaves, sep = "")
    
    final_list = list(nwaves = nwaves, country_list = country_list)
    
  } else{ # states
    
    data = apply(matrix(state_name), 1, function(x) load_covid("Brazil", x))
    
    if(new_cases){
      
      npeaks = data %>% lapply(function(x) x$data$new_cases) %>%
        lapply(function(x) nrow(findpeaks(x, minpeakdistance  = 95)))
      
    } else{
      
      npeaks = data %>% lapply(function(x) x$data$new_deaths) %>%
        lapply(function(x) nrow(findpeaks(x, minpeakdistance  = 95)))
      
    }
    
    waves = npeaks %>% unlist
    nwaves = waves %>% unique %>% sort
    
    state_list = list()
    for(i in 1:length(nwaves)){
      pos = which(waves == nwaves[i])
      state_list[[i]] = state_name[pos]
    }
    names(state_list) = paste("wave", nwaves, sep = "")
    
    final_list = list(nwaves = nwaves, state_list = state_list)
    
  }
  
  final_list
  
}
