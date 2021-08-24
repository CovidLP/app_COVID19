nwaves <- 2:8

country_list <- list(
  wave2 = c("Guatemala"),
  wave3 = c("China", "Ecuador", "New Zealand", "Venezuela"),
  wave4 = c("Honduras"),
  wave5 = c("Australia", "Bolivia", "Costa Rica", "India", "Iraq", "Morocco", "Paraguay", 
            "Peru", "Saudi Arabia"),
  wave6 = c("Argentina", "Belgium", "Canada", "Chile", "Colombia", "France", "Indonesia", 
            "Mexico", "Russia", "South Africa", "Sweden", "Turkey"),
  wave7 = c("Ethiopia", "Germany", "Greece", "Ireland", "Italy", "Netherlands", "Norway", 
            "Panama", "Poland", "Romania", "Switzerland", "United Kingdom", "Uruguay"),
  wave8 = c("Japan","Korea, South", "Portugal", "Spain", "Ukraine", "US")
)

template <- readLines("template_cases_countries.R")

for(i in 1:length(nwaves)){
  
  countries <- paste("c(\"",paste(country_list[[i]],collapse="\",\""),"\")",sep="")
  
  code <- gsub("_countrynames_",countries,template)
  code <- gsub("_nwaves_",nwaves[i],code)
  file.name <- paste0("predict_",nwaves[i],"wave_par.R")
  write(code,file=file.name)
  cat("File ", paste0(getwd(),"/",file.name), "written.\n")
}


