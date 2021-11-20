library("dplyr")
library("PandemicLP")
library("pracma")

setwd("/home/marcosop/Covid/R/STAN/")
source("utils.R")

out <- nwaves(new_cases = FALSE)
nwaves <- out$nwaves
country_list <- out$country_list 

template <- readLines("template_deaths_countries.R")

files <- list.files(pattern="death_[1,2,3,4,5,6,7,8,9](.*?).R")
file.remove(files)

for(i in 1:length(nwaves)){
  
  countries <- paste("c(\"",paste(country_list[[i]],collapse="\",\""),"\")",sep="")
  
  code <- gsub("_countrynames_",countries,template)
  code <- gsub("_nwaves_",nwaves[i],code)
  file.name <- paste0("death_",nwaves[i],"wave_par.R")
  write(code,file=file.name)
  cat("File ", paste0(getwd(),"/",file.name), "written.\n")
}


