
pd <- fread("../pulmonary-deaths.csv")
tb.rate <- with(pd[cause=='Tuberculosis'&variable=='Per100K'], zoo(value, year))
pi.rate <- with(pd[cause=='Pneumonia & Flu'&variable=='Per100K'], zoo(value, year))
tb.adjustment <- (tb.rate+pi.rate)/pi.rate
z.mcmorts <- 10*read.csv.zoo("../age-adjusted-pneumonia-flu-death-rate.csv")
z.mcmorts <- z.mcmorts[ time(z.mcmorts) != 1918 ]
tb.adjustment[I(end(z.mcmorts))] <- 1
tb.adjustment <- na.approx(tb.adjustment, xout=time(z.mcmorts))
z.mcmorts <- z.mcmorts * tb.adjustment
actual.years.in.past <- zoo(2020-time(z.mcmorts), time(z.mcmorts))

{
  recent <- window(z.mcmorts, start=2006)
  recent.fit <- lm( log(recent) ~ I(2020-time(recent)) )
  signif(exp(coef(recent.fit)[1]), 2)
} -> ref.mcmort

fit.slope <- function(z, pin.year) {
  model <- lm( z ~ I(pin.year-time(z)) + 0 )
  return(signif(coef(model)[1],2))
}

pivot.year <- 1952  # launch of oral TB antibiotic Isoniazid 
post.pivot <- window(z.mcmorts, start=pivot.year)
pre.pivot <- window(z.mcmorts, end=pivot.year-1)
log.slope <- fit.slope(log(post.pivot/ref.mcmort), 2020)
pivot.mcmorts <- signif( ref.mcmort * exp(log.slope * (2020-pivot.year)), 2)
linear.slope <- fit.slope(pre.pivot - pivot.mcmorts, pivot.year)

