library("knitr")
library("dplyr")
library("tidyverse")
library("readxl")
library("lattice")
library("RColorBrewer")
library("gridExtra")


log <- read_csv("logs.csv")


coun.code <- read_csv("country_code.csv")

grades <- read_csv("grades.csv", 
                   col_names =c("First",
                                "Last",
                                "country",
                                "ID",
                                "Ins",
                                "dep",
                                "Email",
                                "Quiz1",
                                "Quiz2",
                                "Quiz3",
                                "Quiz4",
                                "Quiz.total",
                                "other"),
                   skip=1)

grades$Quiz1 <- as.numeric(as.character(grades$Quiz1))
grades$Quiz2 <- as.numeric(as.character(grades$Quiz2))
grades$Quiz3 <- as.numeric(as.character(grades$Quiz3))
grades$Quiz4 <- as.numeric(as.character(grades$Quiz4))
grades$Quiz.total <- as.numeric(as.character(grades$Quiz.total))
colnames(log) <- make.names(colnames(log))
grades$User.full.name <- paste(grades$First, grades$Last, sep=" ")
grades$Country <- coun.code$COUNTRY[match(grades$country, coun.code$country)]
grades <- grades[c("User.full.name", "Country", "Email", "Quiz1", "Quiz2", "Quiz3","Quiz4","Quiz.total")]

#PULL USER INFO
all.user <- grades[c("User.full.name", "Country", "Email")]

#event count
total.count <- count(log,Event.name)

country.count <- count(grades, Country)

#count video 
log$video <- ifelse( grepl("M�dulo 1: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", 0)
log$video <- ifelse( grepl("M�dulo 2: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("M�dulo 3: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("M�dulo 4: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)


video.count <- count(log, video)
colnames(video.count) <- c("Event.name", 'n')
total.count <- rbind(total.count, video.count)

#count reading
log$reading <- ifelse( grepl("M�dulo 1: Lectura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", 0) 
log$reading <- ifelse( grepl("M�dulo 2: Lectura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 
log$reading <- ifelse( grepl("M�dulo 3: Lectura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 
log$reading <- ifelse( grepl("M�dulo 4: Lectura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 


read.count <- count(log, reading)
colnames(read.count) <- c("Event.name", "n")
total.count <- rbind(total.count, read.count)


# ACTIVITY COUNT


log %>%
  filter(grepl("Programa de estudio", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Syllabus Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 1: Video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 1: Video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 1: Video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 1: Lectura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 1: Lectura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 1: Lectura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 2: Video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 2: Video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 2: Video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M�dulo 2: Lectura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%  filter(grepl("M�dulo 2: Lectura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 2: Lectura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 3: Video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 3: Video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 3: Video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M�dulo 3: Lectura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 3: Lectura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 3: Lectura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 3: Lectura 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M�dulo 4: Video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 4: Video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 4: Video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M�dulo 4: Video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 4: Video 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 4: Lectura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M�dulo 4: Lectura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

all.user <- all.user[!is.na(all.user$Country), ]

all.user[is.na(all.user)] <- 0


GROUP1 <- all.user[apply(all.user == 0, 1, sum) <= 5, ]

GROUP4 <- all.user[apply(all.user == 0, 1, sum) >= 26, ]

left <- all.user[ !(all.user$Email %in% GROUP1$Email), ]

left <- left[ !(left$Email %in% GROUP4$Email), ]

GROUP2 <- left[apply(left == 0, 1, sum) <= 13, ]

GROUP3 <- left[ !(left$Email %in% GROUP2$Email), ]


write_csv(GROUP1, "group1.csv")

write_csv(GROUP2, "group2.csv")

write_csv(GROUP3, "group3.csv")

write_csv(GROUP4, "group4.csv")

write_csv(total.count, "counts.csv")

write_csv(country.count, "countrycounts.csv")

