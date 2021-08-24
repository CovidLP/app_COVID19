nwaves <- 2:5

state_list <- list(
  wave2 = c("AC", "CE", "DF", "GO", "PA", "PI", "RO", "RR"),
  wave3 = c("AM", "MA", "MG", "MT", "PE", "PR", "RN", "RS", "SP", "TO"),
  wave4 = c("AL", "AP", "BA", "ES","SE"),
  wave5 = c("MS", "PB", "RJ", "SC")
)

template <- readLines("template_deaths_BR.R")

for(i in 1:length(nwaves)){
  
  states <- paste("c(\"",paste(state_list[[i]],collapse="\",\""),"\")",sep="")
  
  code <- gsub("_statelist_",states,template)
  code <- gsub("_nwaves_",nwaves[i],code)
  file.name <- paste0("death_BR_",nwaves[i],"wave.R")
  write(code,file=file.name)
  cat("File ", paste0(getwd(),"/",file.name), "written.\n")
}
