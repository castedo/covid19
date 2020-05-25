source("util.R")
source("data.R")

trip <-
   read_csv(trip.file) %>%
   arrange(Mile) %>%
   mutate(Leg=factor(Mile,
                     levels=unique(Mile),
                     labels=sprintf("%s (%s)", unique(Mile), unique(Name)),
                    )
         )

trip.pop <-
  trip %>%
  inner_join(US.POP) %>%
  group_by(Leg, Name, Mile) %>%
  summarise_at('Population', sum) %>%
  ungroup()

jhu.data <-
  JHU.US %>%
  inner_join(trip) %>%
  group_by(Metric, Date, Leg, Mile) %>%
  summarise_at(vars(Value), sum) %>%
  ungroup()

micro.it <- function(src) {
  src %>% inner_join(trip.pop) %>% mutate(Value=Value/Population*1e6)
}

microcases <-
  jhu.data %>%
  micro.it() %>%
  filter(Metric=='confirmed')
microdeaths <-
  jhu.data %>%
  micro.it() %>%
  filter(Metric=='deaths')

hist.ref <- data.frame(
  y=c(3, 15, 35, 115, 180),
  txt=c("2010-2019", "1950-1951", "1940-1941", "1900-1901", "1865"))
hist.txt <- "Reference\nAverage*\nFlu/Pneumonia/TB"

plot.trip <- function(data) {
  xlabels <- with(data, paste(unique(Mile), unique(Name), sep="\n"))
  group <- factor(data$Date, as.character(rev(unique(data$Date))))
  ggplot(data, aes(x=Mile, y=Value, group=group, linetype=group)) +
    geom_point() +
    labs(x="Mile", linetype="As-of Date", color=hist.txt) +
    scale_x_continuous(breaks=unique(data$Mile), labels=xlabels)
}

plot.ts <- function(data) {
  ggplot(data, aes(x=Date, y=Value, group=Leg, color=Leg)) +
    scale_x_date(breaks = unique(data$Date), date_labels = "%b %d") +
    geom_point()
}

