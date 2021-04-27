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

## main function to load the data - US cases
loadDataUS_cases = function(fileName, columnName) {
  data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
    filter(iso2=="US") %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key) %>%
    pivot_longer(-(1:2), names_to="date", values_to=columnName) %>%
    mutate(
date=as.Date
(date, format="%m/%d/%y")) %>%
    rename(country = Country_Region, state = Province_State)
  data = data %>% group_by(state, country, date) %>% summarise(confirmed=sum(confirmed))
  save(data, file=fileName)  
  return(data)
}

## main function to load the data - US deaths
loadDataUS_deaths = function(fileName, columnName) {
  data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
    filter(iso2=="US") %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key) %>%
    pivot_longer(-(1:3), names_to="date", values_to=columnName) %>%
    mutate(
date=as.Date
(date, format="%m/%d/%y")) %>%
    rename(country = Country_Region, state = Province_State, pop = Population)
  data = data %>% group_by(state, country, date) %>% summarise(deaths=sum(deaths), pop=sum(pop))
  save(data, file=fileName)
  return(data)
}
