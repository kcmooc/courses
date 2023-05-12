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
g$video <- ifelse( grepl("M?dulo 1: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", 0)
log$video <- ifelse( grepl("M?dulo 2: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("M?dulo 3: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("M?dulo 4: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)


video.count <- count(log, video)
colnames(video.count) <- c("Event.name", 'n')
total.count <- rbind(total.count, video.count)

#count reading
og$reading <- ifelse( grepl("M?dulo 1: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", 0) 
log$reading <- ifelse( grepl("M?dulo 2: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 
log$reading <- ifelse( grepl("M?dulo 3: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 
log$reading <- ifelse( grepl("M?dulo 4: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 

read.count <- count(log, reading)
colnames(read.count) <- c("Event.name", "n")
total.count <- rbind(total.count, read.count)




# ACTIVITY COUNT


log %>%
  filter(grepl("Ementa", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Syllabus Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M1: V1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M1: V2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M1: V3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M1: V4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M1: V5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M1: L1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("M2: V1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M2: V2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)
log %>%
  filter(grepl("M2: V3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M2: V4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M2: V5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M2: V6", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 6")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M2: V7", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 7")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)




log %>%
  filter(grepl("M2: L1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("M3: V1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M3: V2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M3: V3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M3: V4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("M3: L1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M4: V1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M4: V2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M4: V3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M4: V4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M4: L1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


all.user <- all.user[!is.na(all.user$Country), ]


all.user[is.na(all.user)] <- 0


write_csv(all.user, "alluser.csv")


GROUP1 <- all.user[apply(all.user == 0, 1, sum) <= 5, ]

GROUP4 <- all.user[apply(all.user == 0, 1, sum) >= 25, ]

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



