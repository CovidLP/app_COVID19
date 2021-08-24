nwaves <- 2:6

state_list <- list(
  wave2 = c("AL", "CE", "PI"),
  wave3 = c("MA", "PA"),
  wave4 = c("AC", "AM", "DF", "GO", "MT","PB", "PE", "RN", "RO", "RR"),
  wave5 = c("BA", "ES", "MG", "RJ", "RS", "SE", "SP", "TO"),
  wave6 = c("AP", "MS", "PR", "SC")
)

template <- readLines("template_cases_BR.R")

for(i in 1:length(nwaves)){
  
  states <- paste("c(\"",paste(state_list[[i]],collapse="\",\""),"\")",sep="")
  
  code <- gsub("_statelist_",states,template)
  code <- gsub("_nwaves_",nwaves[i],code)
  file.name <- paste0("predict_BR_",nwaves[i],"wave.R")
  write(code,file=file.name)
  cat("File ", paste0(getwd(),"/",file.name), "written.\n")
}


