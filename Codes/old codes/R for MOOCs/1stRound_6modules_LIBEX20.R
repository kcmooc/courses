library("plyr")
library("tidyverse")
library("readxl")


log <- read_csv("logs.csv")
#delete places in column names in the log file
colnames(log) <- make.names(colnames(log))

#pull grades and rename the columns
grade <- read_csv("grades.csv", 
                  col_names =c("First",
                               "Last",
                               "Country",
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
                  skip = 1)

grade$Quiz1 <- as.numeric(as.character(grade$Quiz1))
grade$Quiz2 <- as.numeric(as.character(grade$Quiz2))
grade$Quiz3 <- as.numeric(as.character(grade$Quiz3))
grade$Quiz4 <- as.numeric(as.character(grade$Quiz4))
grade$Quiz5 <- as.numeric(as.character(grade$Quiz5))
grade$Quiz6 <- as.numeric(as.character(grade$Quiz6))
grade$Quiz.total <- as.numeric(as.character(grade$Quiz.total))


#combine first and last name
grade$User.full.name <- paste(grade$First, grade$Last, sep=" ")

#pull out useful info
grades <- grade[c("User.full.name", "Country", "Email", "Quiz1", "Quiz2", "Quiz3","Quiz4","Quiz5","Quiz6", "Quiz.total")]

#PULL USER INFO
all.user <- grades[c("User.full.name", "Country", "Email", "Quiz1", "Quiz2", "Quiz3","Quiz4","Quiz5","Quiz6", "Quiz.total")]


#select users who participated in discussion forum for each modudule
log %>%
  filter(grepl("M1", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) -> "user" 
  user <- count(user,User.full.name) 
  colnames(user) <- c("User.full.name", "Forum1")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M2", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name) 
colnames(user) <- c("User.full.name", "Forum2")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

log %>%
  filter(grepl("M3", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name) 
colnames(user) <- c("User.full.name", "Forum3")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("M4", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name) 
colnames(user) <- c("User.full.name", "Forum4")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)



log %>%
  filter(grepl("M5", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name) 
colnames(user) <- c("User.full.name", "Forum5")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)


log %>%
  filter(grepl("M6", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) -> "user" 
user <- count(user,User.full.name) 
colnames(user) <- c("User.full.name", "Forum6")
all.user <- merge(all.user, user, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

write_csv(all.user, "all.user.csv")

#filter out who passed this time
yes <- filter(all.user, 
              Quiz1>=70 & 
                Quiz2>=70 & 
                Quiz3>=70 & 
                Quiz4>=70 & 
                Quiz5>=70 &
                Quiz6>=70 & 
                Quiz.total>=70 &
                Forum1>= 3 &
                Forum2>= 3 &
                Forum3>= 3 &
                Forum4>= 3 &
                Forum5>= 3 &
                Forum6>= 3 )

write_csv(yes, "yes.csv")

##End of the process####################################################




#read request form 
request <- read_csv("requests.csv", 
                    skip = 3,
                    col_names = c(
                      "serial",
                      "SID",
                      "1",
                      "2",
                      "3",
                      "4",
                      "5",
                      "6",
                      "Email",
                      "7",
                      "8",
            
                      "9"
                    ))
request <- request[c("serial", "SID", "Email")]

#clear duplicates from  the request form
request <-request[!duplicated(request$Email),]


request$Email <-  tolower(request$Email)
grades$Email <- tolower(grades$Email)


#check their status in all.user file

verified <- merge(request, grades, by.x = "Email", by.y = "Email", all.x = TRUE)

verified <- arrange(verified,  serial )


verified$Round <-  "1"

verified <- verified[, c(19,2,3,4,1,5:18)]

write_csv(verified, "verified.csv")


pass <- filter(grades, 
                 Quiz1>=70 & 
                 Quiz2>=70 & 
                 Quiz3>=70 & 
                 Quiz4 >=70 & 
                 Quiz5 >=70 & 
                 Quiz6 >=70 )

pass <- pass[!(pass$Email %in% verified$Email), ]
write_csv(pass, "passed_didn't_apply.csv")
