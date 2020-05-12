## main function to load the data - GLOBAL
loadData = function(fileName, columnName) {
  data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
    select(-Lat, -Long) %>% 
    pivot_longer(-(1:2), names_to="date", values_to=columnName)%>%
    mutate(date=as.Date(date, format="%m/%d/%y")) %>%
    rename(country = `Country/Region`, state = `Province/State`)
  save(data, file=fileName)  
  return(data)
}
