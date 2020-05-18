suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(zoo)
})

zoo.metrics <- function(src) {
  df <- src %>%
    group_by(Date, Metric) %>%
    summarise_at(vars(Value), sum) %>%
    ungroup() %>%
    pivot_wider(names_from=Metric, values_from=Value)
  zoo( select(df,-c(Date)), df$Date )
}

one.metric <- function(df, metric='deaths') {
  df %>% filter(Metric==metric) %>% select(-Metric)
}

read.de.pop.counties <- function(dirpath=".") {
  read_csv(paste(dirpath, "german-landkreise.csv", sep='/'))
}

read.de.pop.states <- function(dirpath=".") {
  read_csv(paste(dirpath, "german-states.csv", sep='/'))
}

read.de.counties <- function(dirpath=".") {
  df <- read_csv(paste(dirpath, "deaths-rki-by-ags.csv", sep='/'))
  keys <- c('Country', 'Province')
  ret <- df %>%
    rename(Date=time_iso8601) %>%
    mutate(Date=as.Date(Date)) %>%
    select(-sum_deaths) %>%
    pivot_longer(-'Date', names_to="County", values_to="Value") %>%
    mutate(Metric='deaths', County=as.integer(County))
  read.de.pop.counties() %>%
    select(ags, name, population) %>%
    rename(County=ags, Population=population) %>%
    inner_join(ret)
}

read.de.states <- function(dirpath=".") {
  df <- read_csv(paste(dirpath, "deaths-rki-by-state.csv", sep='/'))
  keys <- c('Country', 'Province')
  ret <- df %>%
    rename(Date=time_iso8601) %>%
    mutate(Date=as.Date(Date)) %>%
    select(-sum_deaths) %>%
    pivot_longer(-'Date', names_to="State", values_to="Value") %>%
    mutate(Metric='deaths')
  read.de.pop.states() %>%
    select(abbrev, english_name, population) %>%
    rename(State=abbrev, Population=population) %>%
    inner_join(ret)
}

de.dirpath <- "../DE_data/"
delayedAssign("DE.COUNTIES", read.de.counties(de.dirpath))
delayedAssign("DE.STATES", read.de.states(de.dirpath))

get.de.state.rates <- function() {
  DE.STATES %>%
    filter(Metric=='deaths') %>%
    select(-Metric) %>%
    group_by(State, english_name, Population) %>%
    summarise(maxg={z <- zoo(Value/Population*1e6, Date); max(diff(z,21)/3)},
              last={z <- zoo(Value/Population*1e6, Date);
                    z <- tail(coredata(diff(z,14)/2),1)}
             ) %>%
    ungroup()
}

get.de.county.rates <- function() {
  DE.COUNTIES %>%
    filter(Metric=='deaths') %>%
    select(-Metric) %>%
    group_by(County, name, Population) %>%
    summarise(maxg={z <- zoo(Value/Population*1e6, Date); max(diff(z,21)/3)},
              last={z <- zoo(Value/Population*1e6, Date);
                    z <- tail(coredata(diff(z,14)/2),1)}
             ) %>%
    ungroup()
}


read.esp.pop <- function(dirpath=".") {
  read_csv(paste(dirpath, "esp_pop.csv", sep='/'))
}

read.esp.ccaa <- function(dirpath=".") {
  pick <- cols_only(
    CCAA = col_character(),
    FECHA = col_date("%d/%m/%Y"),
    CASOS = col_double(),
    `PCR+` = col_double(),
    `TestAc+` = col_double(),
    Fallecidos = col_double()
  )
  fname <- paste(dirpath, "../data/agregados.csv", sep='/')
  ret <- read_csv(fname, col_types=pick) %>%
    filter(nchar(CCAA,type='bytes')==2) %>%
    rename(Date=FECHA, Value=Fallecidos) %>%
    select(CCAA, Date, Value) %>%
    mutate(Metric='deaths') %>%
    replace_na(list(Value=0))
  ret
}

read.esp.micromorts <- function(ccaa) {
  pop <- as.numeric(read.esp.pop() %>% filter(CCAA==ccaa) %>% select(Population))
  read.esp.ccaa() %>% filter(CCAA==ccaa & Metric=='deaths') %>% with(zoo(Value/pop*1e6, Date))
}


ita.dirpath <- "../ITA_data/dati-regioni/"

read.ita.region <- function(dirpath=ita.dirpath) {
  pick <- cols_only(
    data = col_datetime(),
    codice_regione = col_character(),
    denominazione_regione = col_character(),
#    ricoverati_con_sintomi = col_double(),
#    totale_ospedalizzati = col_double(),  # total hospitalized
#    totale_positivi = col_double(),
#    variazione_totale_positivi = col_double(),  # total change positive
#    nuovi_positivi = col_double(),
#    dimessi_guariti = col_double(),  # discharged healed
    deceduti = col_double(),
    totale_casi = col_double()
#    casi_testati = col_logical(),  # cases tested
  )
  df <- read_csv(paste(dirpath, "dpc-covid19-ita-regioni.csv", sep='/'),
                 col_types=pick)
  df %>%
    mutate(data=as.Date(data)) %>%
    rename(Date=data,
           Code=codice_regione,
           Region=denominazione_regione,
           deaths=deceduti,
           confirmed=totale_casi) %>%
    pivot_longer(-c(Date, Code, Region), names_to="Metric", values_to="Value")
}

read.ita.pop <- function(dirpath=".") {
  read_csv(paste(dirpath, "ita_pop.csv", sep='/'))
}

read.ita.micromorts <- function(code) {
  pop <- as.numeric(read.ita.pop() %>% filter(Code==code) %>% select(Population))
  read.ita.region() %>% filter(Code==code & Metric=='deaths') %>% with(zoo(Value/pop*1e6, Date))
}


read.nyt.states <- function(dirpath) {
  df <- read_csv(paste(dirpath, "us-states.csv", sep='/'))
  df %>%
    rename(Date=date,
           State=state,
           FIPS=fips,
           confirmed=cases
           ) %>%
    mutate(FIPS=as.numeric(FIPS)) %>%
    pivot_longer(c(confirmed, deaths), names_to="Metric", values_to="Value")
}

read.nyt.counties <- function(dirpath) {
  df <- read_csv(paste(dirpath, "us-counties.csv", sep='/'))
  ret <- df %>%
    rename(Date=date,
           State=state,
           County=county,
           FIPS=fips,
           confirmed=cases
           ) %>%
    mutate(FIPS=as.numeric(FIPS)) %>%
    pivot_longer(c(confirmed, deaths), names_to="Metric", values_to="Value")
  q <- with(ret, State=='Virginia' & County=='Alexandria city')
  ret[q, 'County'] <- 'Alexandria'
  ret
}

nyt.dirpath <- "../NYT_data/"
delayedAssign("NYT.COUNTIES", read.nyt.counties(nyt.dirpath))
delayedAssign("NYT.STATES", read.nyt.states(nyt.dirpath))

zoo.nyt <- function(...) {
  df <- NYT.COUNTIES %>%
    filter(...) %>%
    group_by(Date, Metric) %>%
    summarise_at(vars(Value), sum) %>%
    ungroup() %>%
    pivot_wider(names_from=Metric, values_from=Value)
  zoo( select(df,-c(Date)), df$Date )
}

zoo.nyt.micro <- function(...) {
  zoo.nyt(...)/get.pop.US(...)*1e6
}

jhu.dirpath <- "../JHU_data/csse_covid_19_data/csse_covid_19_time_series/"

read.jhu.US.file <- function(metric_name, dirpath=jhu.dirpath) {
  fpath <- paste("time_series_covid19", metric_name, "US.csv", sep='_')
  df <- read_csv(paste(dirpath, fpath, sep='/'))
  df$Population <- NULL
  df %>%
    rename(County=Admin2,
           State=Province_State) %>%
    select(-c(UID:code3, Country_Region, Lat, Long_)) %>%
    pivot_longer(-c(FIPS, County, State, Combined_Key), names_to="Date", values_to="Value") %>%
    mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
    mutate(Metric=metric_name)
}

read.jhu.US <- function(dirpath=jhu.dirpath) {
  ret <- rbind(read.jhu.US.file('deaths'), read.jhu.US.file('confirmed'))
  q <- with(ret, State=='New York' & County=='New York')
  ret[q, 'County'] <- 'New York City'
  ret
}

delayedAssign("JHU.US", read.jhu.US())

zoo.jhu.US <- function(...) {
  df <- JHU.US %>%
    filter(...) %>%
    group_by(Date, Metric) %>%
    summarise_at(vars(Value), sum) %>%
    ungroup() %>%
    pivot_wider(names_from=Metric, values_from=Value)
  zoo( select(df,-c(Date)), df$Date )
}

zoo.jhu.US.micro <- function(...) {
  zoo.jhu.US(...)/get.pop.US(...)*1e6
}


read.jhu.global.file <- function(metric_name, dirpath=jhu.dirpath) {
  fpath <- paste("time_series_covid19", metric_name, "global.csv", sep='_')
  keys <- c('Country', 'Province')
  read_csv(paste(dirpath, fpath, sep='/')) %>%
    select(-c(Lat, Long)) %>%
    rename(Country=`Country/Region`,
           Province=`Province/State`) %>%
    pivot_longer(-keys, names_to="Date", values_to="Value") %>%
    mutate(Date=as.Date(Date,format="%m/%d/%y"),
           Value=as.integer(Value)) %>%
    mutate(Metric=metric_name)
}

read.jhu.global <- function(dirpath=jhu.dirpath) {
  rbind(read.jhu.global.file('deaths'),
        read.jhu.global.file('confirmed'),
        read.jhu.global.file('recovered'))
}

delayedAssign("JHU.GLOBAL", read.jhu.global())

zoo.jhu.global <- function(...) {
  df <- JHU.GLOBAL %>%
    filter(...) %>%
    group_by(Date, Metric) %>%
    summarise_at(vars(Value), sum) %>%
    ungroup() %>%
    pivot_wider(names_from=Metric, values_from=Value)
  zoo( select(df,-c(Date)), df$Date )
}

zoo.jhu.global.micro <- function(country) {
  zoo.jhu.global(Country==country)/get.pop.global(region==country)*1e6
}

get.jhu.countries <- function() {
  JHU.GLOBAL %>%
    group_by(Country, Date, Metric) %>%
    summarise_at('Value', sum) %>%
    ungroup()
}

get.jhu.us.states <- function() {
  JHU.US %>%
    group_by(State, Date, Metric) %>%
    summarise_at('Value', sum) %>%
    ungroup()
}

get.pop.countries <- function() {
  WORLD.POP %>%
    select(-iso3) %>%
    rename(Country=region, Population=population) %>%
    inner_join(get.jhu.countries())
}

get.pop.us.states <- function() {
  US.POP %>%
    group_by(State) %>%
    summarise_at('Population', sum) %>%
    ungroup() %>%
    filter(Population > 0) %>%
    inner_join(get.jhu.us.states())
}

get.study.states <- function() {
  us <- get.pop.us.states() %>% mutate(State=paste(State,"USA",sep=", "))
  intl <- get.pop.countries() %>% rename(State=Country)
  rbind(us, intl)
}

get.state.microrates <- function(metric='deaths') {
  df <- get.study.states() %>%
    filter(Metric==metric) %>%
    mutate(Value=Value/Population*1e6) %>%
    select(-c(Metric, Population)) %>%
    pivot_wider(names_from=State, values_from=Value)
  zoo( select(df,-c(Date)), df$Date )
}

# maxg = peak weekly micromorts (average over 14 days)
# maxd = days since peak
# last = most recent weekly micromorts (averaged over 14 days)
# dleft = number of days left to exit danger levels, assuming exponential shrinkage since peak
#  danger <- 2
#    group_by(Metric, State, Population) %>%
#    summarise(maxg={z <- zoo(Value/Population*1e6, Date); max(diff(z,14)/2)},
#              maxd={z <- diff(zoo(Value/Population*1e6, Date),14)/2; min(time(z)[which(z > max(z)*0.999)])},
#              last={z <- zoo(Value/Population*1e6, Date); tail(coredata(diff(z,14)/2),1)},
#              total=max(Value/Population*1e6)
#             ) %>%
#    ungroup() %>%
#    mutate(maxd=as.numeric(max(intl$Date)-maxd),
#           dleft=ifelse(last < danger, 0, ifelse(maxg<=last, Inf, maxd*log2(last/danger)/log2(maxg/last)))
#          ) %>%
#    filter(total > 0)


# Thank you Michael Harper, from https://mikeyharper.uk
# Resolves inconsistencies between naming in data sources
# Not all datasets have inconsistences but set here and applied to all
countryNameDict <- c("Czech Republic" = "Czechia",
                     "United Kingdom" = "UK",
                     "Bosnia Herzegovina" = "Bosnia and Herzegovina",
                     "Iran, Islamic Rep." = "Iran",
                     "Macedonia" = "North Macedonia",
                     "United States" = "US",
                     "Russian Federation" = "Russia",
                     "Korea, Rep." = "Korea, South",
                     "Korea, Dem. People’s Rep." = "Korea, North",
                     "Congo, Dem. Rep." = "DRC",
                     "Congo, Rep." = "Congo",
                     "Côte d'Ivoire" = "Cote d'Ivoire")

read.world.bank.pop.data <- function(dirpath="../data/worldbank") {
  fpath <- paste(dirpath, "API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv", sep='/')
  read_csv(fpath, skip=4) %>%
    select(c("Country Name", "Country Code", "2018")) %>%
    set_names("region", "iso3", "population") %>%
    mutate(region = recode(region, !!!countryNameDict))
}

delayedAssign("WORLD.POP", read.world.bank.pop.data())

get.pop.global <- function(...) {
  as.numeric(WORLD.POP %>% filter(...) %>% select(population))
}

read.jhu.pop.US <- function(dirpath=jhu.dirpath) {
  fpath <- paste(dirpath, "time_series_covid19_deaths_US.csv", sep='/')
  ret <- read_csv(fpath) %>%
    rename(County=Admin2,
           State=Province_State) %>%
    select(FIPS, County, State, Combined_Key, Population)
  q <- with(ret, State=='New York' & County=='New York')
  ret[q, 'Population'] <- 8398748
  ret[q, 'County'] <- 'New York City'
  ret
}

delayedAssign("US.POP", read.jhu.pop.US())

get.pop.US <- function(...) {
  as.numeric(US.POP %>% filter(...) %>% summarise_at('Population', sum))
}

