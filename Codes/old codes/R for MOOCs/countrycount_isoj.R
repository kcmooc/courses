library("knitr")
library("dplyr")
library("tidyverse")
library("readxl")
library("lattice")
library("RColorBrewer")
library("gridExtra")

countries <- read_csv("grades.csv", 
                   col_names =c("Country"),
                   skip=1)

country.count <- count(countries,Country)

write_csv(country.count, "CountryCount.csv")

