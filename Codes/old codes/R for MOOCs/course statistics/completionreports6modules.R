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
                                "Quiz6",
                                "Quiz.total",
                                "other"),
                   skip=1)


grades$Quiz1 <- as.numeric(as.character(grades$Quiz1))
grades$Quiz2 <- as.numeric(as.character(grades$Quiz2))
grades$Quiz3 <- as.numeric(as.character(grades$Quiz3))
grades$Quiz4 <- as.numeric(as.character(grades$Quiz4))
grades$Quiz4 <- as.numeric(as.character(grades$Quiz5))
grades$Quiz4 <- as.numeric(as.character(grades$Quiz6))
grades$Quiz.total <- as.numeric(as.character(grades$Quiz.total))
colnames(log) <- make.names(colnames(log))

grades$User.full.name <- paste(grades$First, grades$Last, sep=" ")
grades$Country <- coun.code$COUNTRY[match(grades$country, coun.code$country)]
grades <- grades[c("User.full.name", "Country", "Email", "Quiz1", "Quiz2", "Quiz3","Quiz4", "Quiz5","Quiz6", "Quiz.total")]

#PULL USER INFO
all.user <- grades[c("User.full.name", "Country", "Email")]

#event count
total.count <- count(log,Event.name)

country.count <- count(grades,Country)

#count video 
log$video <- ifelse( grepl("Module 1 video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", 0)
log$video <- ifelse( grepl("Module 2 video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("Module 3 video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)
log$video <- ifelse( grepl("Module 4 video", log$Event.context) & log$Event.name =="Course module viewed", "Intructional video viewed", log$video)



video.count <- count(log, video)
colnames(video.count) <- c("Event.name", 'n')
total.count <- rbind(total.count, video.count)


#count reading
log$reading <- ifelse(grepl("Module 1 reading", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", 0)
log$reading <- ifelse( grepl("Module 2 reading", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading)
log$reading <- ifelse( grepl("Module 3 reading", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading)
log$reading <- ifelse( grepl("Module 4 reading", log$Event.context) & log$Event.name =="Course module viewed", "Reading material viewed", log$reading)


read.count <- count(log, reading)
colnames(read.count) <- c("Event.name", "n")
total.count <- rbind(total.count, read.count)


# end of TOTAL event count


# ACTIVITY COUNT
log %>%
  filter(grepl("Syllabus", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Syllabus Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 6", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 6")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 7", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 7")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 8", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 8")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Intro reading 9", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 9")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Intro reading 10", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user, User.full.name)
colnames(user) <- c("User.full.name", "Intro reading 10")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 1 video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 1 video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 1 video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 1 video 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 1 video 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)
log %>%
  filter(grepl("Module 1 video 6", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Video 6 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 1 reading 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 1 reading 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 1 reading 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 1 reading 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 1 reading 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 1 reading 6", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 1 Reading 6 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 2 video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 2 video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 2 video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 2 video 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 2 video 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 2 video 6", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Video 6 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 2 reading 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 2 reading 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 2 reading 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 2 Reading 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 3 video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 3 video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 3 video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 3 video 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 3 video 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Video 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 3 reading 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 3 reading 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 3 reading 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.User.full.name)
colnames(user) <- c("User.full.name", "Module 3 Reading 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 4 video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 4 video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 4 video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 4 video 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)
log %>%
  filter(grepl("Module 4 video 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Video 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)





log %>%
  filter(grepl("Module 4 reading 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 4 reading 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("Module 4 reading 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 4 reading 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 4 reading 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 4 Reading 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 5 video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 5 video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 5 video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 5 video 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 5 video 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Video 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 5 reading 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Reading 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 5 reading 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 5 Reading 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 6 video 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 6 Video 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 6 video 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 6 Video 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 6 video 3", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 6 Video 3 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 6 video 4", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 6 Video 4 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 6 video 5", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 6 Video 5 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("Module 6 reading 1", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 6 Reading 1 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("Module 6 reading 2", Event.context)) %>%
  filter(Event.name=="Course module viewed") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name)
colnames(user) <- c("User.full.name", "Module 6 Reading 2 Views")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

all.user[is.na(all.user)] <- 0

GROUP1 <- all.user[apply(all.user == 0, 1, sum) <= 13, ]

GROUP4 <- all.user[apply(all.user == 0, 1, sum) >= 63, ]

left <- all.user[ !(all.user$Email %in% GROUP1$Email), ]

left <- left[ !(left$Email %in% GROUP4$Email), ]

GROUP2 <- left[apply(left == 0, 1, sum) <= 33, ]

GROUP3 <- left[ !(left$Email %in% GROUP2$Email), ]


write_csv(GROUP1, "group1.csv")

write_csv(GROUP2, "group2.csv")

write_csv(GROUP3, "group3.csv")

write_csv(GROUP4, "group4.csv")

write_csv(total.count, "counts.csv")
