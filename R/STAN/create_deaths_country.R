nwaves <- 1:7

country_list <- list(
  wave1 = c("New Zealand"),
  wave2 = c("Australia", "Norway"),
  wave3 = c("Argentina", "Chile", "Ecuador", "Ethiopia", "Switzerland", "Venezuela"),
  wave4 = c("Belgium", "Bolivia", "Canada", "China", "Costa Rica", "Guatemala", "Honduras", 
            "India", "Ireland", "Paraguay", "Poland", "Ukraine", "Uruguay", "United States of America"),
  wave5 = c("Colombia", "France", "Germany", "Greece", "Iraq", "Mexico", "Morocco", 
            "Panama", "Peru", "Saudi Arabia", "Sweden"),
  wave6 = c("Indonesia", "Italy", "Japan", "South Korea", "Netherlands", "Portugal", 
            "Russia", "South Africa", "Spain", "United Kingdom"),
  wave7 = c("Romania", "Turkey")
)

template <- readLines("template_deaths_countries.R")

for(i in 1:length(nwaves)){
  
  countries <- paste("c(\"",paste(country_list[[i]],collapse="\",\""),"\")",sep="")
  
  code <- gsub("_countrynames_",countries,template)
  code <- gsub("_nwaves_",nwaves[i],code)
  file.name <- paste0("death_",nwaves[i],"wave_par.R")
  write(code,file=file.name)
  cat("File ", paste0(getwd(),"/",file.name), "written.\n")
}


