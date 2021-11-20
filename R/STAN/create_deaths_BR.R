library("dplyr")
library("PandemicLP")
library("pracma")

setwd("/home/marcosop/Covid/R/STAN/")
source("utils.R")

out <- nwaves(country = FALSE,new_cases = FALSE)
nwaves <- out$nwaves
state_list <- out$state_list

template <- readLines("template_deaths_BR.R")

files <- list.files(pattern="death_BR_[1,2,3,4,5,6,7,8,9](.*?).R")
file.remove(files)

for(i in 1:length(nwaves)){
  
  states <- paste("c(\"",paste(state_list[[i]],collapse="\",\""),"\")",sep="")
  
  code <- gsub("_statelist_",states,template)
  code <- gsub("_nwaves_",nwaves[i],code)
  file.name <- paste0("death_BR_",nwaves[i],"wave.R")
  write(code,file=file.name)
  cat("File ", paste0(getwd(),"/",file.name), "written.\n")
}
