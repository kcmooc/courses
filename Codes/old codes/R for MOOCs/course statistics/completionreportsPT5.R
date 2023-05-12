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
                                "Quiz5",
                                "Quiz.total",
                                "other"),
                   skip=1)

grades$Quiz1 <- as.numeric(as.character(grades$Quiz1))
grades$Quiz2 <- as.numeric(as.character(grades$Quiz2))
grades$Quiz3 <- as.numeric(as.character(grades$Quiz3))
grades$Quiz4 <- as.numeric(as.character(grades$Quiz4))
grades$Quiz5 <- as.numeric(as.character(grades$Quiz5))
grades$Quiz.total <- as.numeric(as.character(grades$Quiz.total))
colnames(log) <- make.names(colnames(log))
grades$User.full.name <- paste(grades$First, grades$Last, sep=" ")
grades$Country <- coun.code$COUNTRY[match(grades$country, coun.code$country)]
grades <- grades[c("User.full.name", "Country", "Email", "Quiz1", "Quiz2", "Quiz3","Quiz4","Quiz5","Quiz.total")]

#PULL USER INFO
all.user <- grades[c("User.full.name", "Country", "Email")]

#event count
total.count <- count(log,Event.name)

country.count <- count(grades, Country)

#count video 
log$video <- ifelse( grepl("M?dulo 1: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", 0)
log$video <- ifelse( grepl("M?dulo 2: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("M?dulo 3: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("M?dulo 4: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("M?dulo 5: Video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)


video.count <- count(log, video)
colnames(video.count) <- c("Event.name", 'n')
total.count <- rbind(total.count, video.count)

#count reading
log$reading <- ifelse( grepl("M?dulo 1: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", 0) 
log$reading <- ifelse( grepl("M?dulo 2: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 
log$reading <- ifelse( grepl("M?dulo 3: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 
log$reading <- ifelse( grepl("M?dulo 4: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 
log$reading <- ifelse( grepl("M?dulo 5: Leitura", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading) 


read.count <- count(log, reading)
colnames(read.count) <- c("Event.name", "n")
total.count <- rbind(total.count, read.count)




# ACTIVITY COUNT


log %>%
  filter(grepl("Programa do curso", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Syllabus Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

# Modulo 1 videos
log %>%
  filter(grepl("M?dulo 1: V?deo 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 1: V?deo 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 1: V?deo 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 1: V?deo 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 1: V?deo 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

# Modulo 1 Leituras 1

log %>%
  filter(grepl("M?dulo 1: Leitura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 1: Leitura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 1: Leitura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 1: Leitura 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

# Modulo 2 VIDEOS

log %>%
  filter(grepl("M?dulo 2: V?deo 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 2: V?deo 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 2: V?deo 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 2: V?deo 4",Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 2: V?deo 5",Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

# Modulo 2 Leituras 

log %>%
  filter(grepl("M?dulo 2: Leitura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 2: Leitura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 2: Leitura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


# Modulo 3 VIDEOS

log %>%
  filter(grepl("M?dulo 3: V?deo 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 3: V?deo 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)
log %>%
  filter(grepl("M?dulo 3: V?deo 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 3: V?deo 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 3: V?deo 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


# Modulo 3 leituras 
log %>%
  filter(grepl("M?dulo 3: Leitura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 3: Leitura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 3: Leitura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


# Modulo 4 videos 

log %>%
  filter(grepl("M?dulo 4: V?deo 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M?dulo 4: V?deo 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 4: V?deo 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 4: V?deo 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 4: V?deo 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

# Modulo 4 leituras

log %>%
  filter(grepl("M?dulo 4: Leitura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 4: Leitura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 4: Leitura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 4: Leitura 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


# Modulo 5 videos

log %>%
  filter(grepl("M?dulo 5: V?deo 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: V?deo 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: V?deo 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: V?deo 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: V?deo 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: V?deo 6", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 6")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

# Modulo 5 leituras

log %>%
  filter(grepl("M?dulo 5: Leitura 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: Leitura 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: Leitura 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M?dulo 5: Leitura 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Reading 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


all.user[is.na(all.user)] <- 0


GROUP1 <- all.user[apply(all.user == 0, 1, sum) <= 9, ]

GROUP4 <- all.user[apply(all.user == 0, 1, sum) >= 44, ]

left <- all.user[ !(all.user$Email %in% GROUP1$Email), ]

left <- left[ !(left$Email %in% GROUP4$Email), ]

GROUP2 <- left[apply(left == 0, 1, sum) <= 22, ]

GROUP3 <- left[ !(left$Email %in% GROUP2$Email), ]


write_csv(GROUP1, "group1.csv")

write_csv(GROUP2, "group2.csv")

write_csv(GROUP3, "group3.csv")

write_csv(GROUP4, "group4.csv")

write_csv(total.count, "counts.csv")
