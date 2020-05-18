suppressPackageStartupMessages({
  library(data.table)
})

us <- fread("../NYT_data/us-counties.csv")
us[, fips := NULL ]
setnames(
    us,
    c("date","county","state","cases","deaths"),
    c("Day","Reg3","Reg2","Confirmed Cases","Fatalities")
)
us <- melt( us, id.vars=c("Day","Reg2","Reg3") )
setnames(
    us,
    c("variable","value"),
    c("What","Count")
)
us <- us[ Count>0 ]
## us.all <- us[, sum(Count), by=.(Reg2,What,Day) ]
## setnames( us.all, "V1", "Count" )
## us.all$Reg3 <- "All"
## us <- rbind( us, us.all )
us$Reg1 <- "US"
us$Day <- as.Date( us$Day )
us[ complete.cases( us ) ]

us <- us[ ! grep("Unknown", Reg2) ]
covid <- us

covid[
    What=="Fatalities",
    maxCount := max(Count),
    by=.(Reg1,Reg2,Reg3)
]
covid[ is.na(maxCount), maxCount := 0 ]
setorder( covid, -maxCount, -Count )
covid[, maxCount := NULL ]

## just in case we missed this somewhere:
covid <- covid[ Count>0 ]

covid$Day <- as.Date( covid$Day, format="%Y-%m-%d" )

