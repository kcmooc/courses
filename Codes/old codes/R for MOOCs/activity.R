library("knitr")
library("dplyr")
library("tidyverse")
library("readxl")
library("lattice")
library("RColorBrewer")
library("gridExtra")


log <- read_csv("logs.csv")
grades <- read_csv("grades.csv")
coun.code <- read_csv("country_code.csv")


colnames(log) <- make.names(colnames(log))
colnames(grades) <- make.names(colnames(grades))

log$Time <- as.Date(log$Time, "%m/%d")


grades$User.first.name <- paste(grades$First.name, sep=" ")

grades$User.last.name <- paste(grades$Last.name, sep=" ")

grades$Country <- coun.code$COUNTRY[match(grades$Country, coun.code$country)]

all.user <- grades[c("First.name","Last.name","Email.address",'Country')]

head(log)

log %>%
  filter (Time >= "2022-6-27") -> active
  active <- active[!duplicated(active$User.full.name), ]
  
active <- active["User.full.name"]
  

write_csv(active, "active.csv")
