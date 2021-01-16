readfiles.repo <- function(){
  req <- GET("https://api.github.com/repos/thaispaiva/app_COVID19/git/trees/master?recursive=1")
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  files <- grep("STpredictions/", filelist, value = TRUE, fixed = TRUE)
  files <- unlist(lapply(strsplit(files, "/"), "[", 2))
  files <- grep(".rds", files, value = TRUE)
  
  return(files)
}

#files <- readfiles.repo()


#strsplit(files,"_")
