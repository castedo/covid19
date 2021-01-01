suppressPackageStartupMessages({
  library(zoo)
})

lagz <- zoo:::lag.zoo

dzoo <- function(src, fun, ...) {
  src %>%
  group_by_at(vars(-Date, -Value)) %>%
  do({
    z <- fun(zoo(.$Value, .$Date), ...);
    data.frame(Date=time(z), Value=coredata(z)) 
  }) %>%
  ungroup()
}

daily.deltas <- function(z) {
  diff(na.locf(z, xout=seq(start(z), end(z), by=1)))
}

weekly.deltas <- function(z, n=1) {
  z <- na.locf(z, xout=seq(start(z), end(z), by=1))
  weekly <- z[seq(end(z), start(z), by=-7*n)]/n
  return(c(head(weekly,1), diff(weekly)))
}

scale_x_eow <- function(z) {
  end.of.weeks <- seq(end(z), start(z), by=-7)
  scale_x_date(breaks=end.of.weeks, date_labels="%b %d")
}

autoplot.weekly.bar <- function(weekly) {
  autoplot(weekly, geom='blank') +
    scale_x_date(breaks = time(weekly), date_labels = "%b %d") +
    theme(axis.text.x = element_text(angle = 60, hjust=1)) +
    geom_col(position = position_nudge(x = -3))
}

ppd.micromort <- function(retroyear) {
  log.rise <- log.slope * pmin(retroyear, 2020 - pivot.year)
  linear.rise <- linear.slope * pmax(retroyear - 2020 + pivot.year, 0)
  return(ref.mcmort * exp(log.rise) + linear.rise)
}

weibull.days <- function(shape, scale) {
  n <- ceiling(qweibull(0.995, shape, scale))
  dist <- c(pweibull(0:n, shape, scale), 1)
  dens <- zapsmall( diff(dist), digits=3 )
  dens/sum(dens)
}

weibull.lag <- function(z, shape, scale) {
  dens <- weibull.days(shape, scale)
  ret <- dens[1]*z
  for (i in 2:length(dens)) {
    t <- end(ret) + 1
    z <- zoo(coredata(z), time(z) + 1)
    z[start(ret)] <- 0
    ret[t] <- 0
    ret <- ret + dens[i]*z
  }
  ret
}

ave.daily.change <- function(cummulative) {
  w <- weekly.deltas(cummulative)
  ret <- exp(diff(log(w))/7) - 1
  ret[start(w)] <- NA
  return(ret)
}
scale_y_daily_change <- function() {
  breaks <- c(2^(-1/c(3, 7, 14))-1, 0, 2^(1/c(14, 7, 5, 3, 2))-1)
  oob_censor_any = function(...) scales::censor(..., only.finite=FALSE)
  scale_y_continuous(labels=scales::percent_format(),
                     breaks=breaks,
                     limits=(2^(1/c(-3, 2))-1),
                     oob=oob_censor_any,
                     sec.axis=sec_axis(trans='identity',
                                       breaks=breaks,
                                       labels=label_doubling_time,
                                       name="Doubling or Halving Time"))
}
label_doubling_time <- function(bp) {
  ifelse(abs(bp) < 0.01, "infinite",
                         paste(format(abs(1/log2(1+bp))), "days   "))
}

