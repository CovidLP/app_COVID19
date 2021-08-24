classify.flag = function(obs, adj){
  
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
  
  ifelse(est <= 0.2, 0, ifelse(est <= 0.5, 1, 2))
  
}
