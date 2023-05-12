library("knitr")
library("dplyr")
library("tidyverse")
library("readxl")
library("lattice")
library("RColorBrewer")
library("gridExtra")


grades <- read_csv("grades.csv")
coun.code <- read_csv("country_code.csv")

colnames(grades) <- make.names(colnames(grades))

grades$User.full.name <- paste(grades$First.name, grades$Last.name, sep=" ")

grades$Country <- coun.code$COUNTRY[match(grades$Country, coun.code$country)]

user <- grades[c("User.full.name", "Country", "Email.address")]

country.count <- count(grades,Country)

write_csv(user, "user.csv")

write_csv(country.count, "country.count.csv")

