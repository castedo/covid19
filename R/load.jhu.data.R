
jhu <- NULL
datadir <- "../JHU_data/csse_covid_19_data/csse_covid_19_daily_reports"
for ( p in dir(path=path, pattern="csv$") ) {
  dt <- fread(paste(datadir,p,sep='/'))
  day <- as.Date(p, format="%m-%d-%Y.csv", optional=FALSE)
  if (is.na(day)) {
    print(p)
    print(day)
  }
  dt$Date <- as.Date(p, format="%m-%d-%Y.csv", optional=FALSE)
  setnames(dt, c('Country/Region', 'Country_Region'),
               c('Country', 'Country'), skip_absent=TRUE)
  setnames(dt, c('Province/State', 'Province_State'),
               c('Province', 'Province'), skip_absent=TRUE)
  setnames(dt, c('Admin2'), c('County'), skip_absent=TRUE)
  if ( ! 'County' %in% names(dt) ) {
    dt$County <- NA
  }
  keys <- c('Date', 'Country', 'Province', 'County')
  cols <- c(keys, 'Confirmed', 'Deaths', 'Recovered')
  jhu <- rbind(jhu, melt(dt[, ..cols], keys))
}

