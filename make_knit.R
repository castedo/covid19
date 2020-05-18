suppressPackageStartupMessages({
  library(utils)
  library(datasets)
  library(data.table)
  library(zoo)
  library(graphics)
  library(ggplot2)
  library(knitr)
})

knitr::opts_knit$set(
  upload.fun = knitr::image_uri
)
knitr::opts_chunk$set(
  echo = FALSE,
  include=TRUE,
  fig.path='_build/figure/'
)

args <- commandArgs(trailingOnly=TRUE)
knit(args[1], output=args[2])

