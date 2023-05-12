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


grades$User.full.name <- paste(grades$First.name, grades$Last.name, sep=" ")

grades$Country <- coun.code$COUNTRY[match(grades$Country, coun.code$country)]

all.user <- grades[c("First.name","User.full.name", "Country", "Email.address")]

head(log)

log %>%
  filter (Time >= "2019-7-30") -> active
  active <- active[!duplicated(active$User.full.name), ]
  
  active <- active["User.full.name"]
  

inactive <- all.user[ !(all.user$User.full.name %in% active$User.full.name), ]

write_csv(inactive, "inactive.csv")
