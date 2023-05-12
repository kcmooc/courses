library("plyr")
library("tidyverse")
library("readxl")
##TestingTesting
##2nd Testing

log <- read_csv("logs.csv")
#delete spaces in column names in the log file
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
                               "Quiz.total",
                               "other"),
                  skip = 1)

grade$Quiz1 <- as.numeric(as.character(grade$Quiz1))
grade$Quiz2 <- as.numeric(as.character(grade$Quiz2))
grade$Quiz3 <- as.numeric(as.character(grade$Quiz3))
grade$Quiz4 <- as.numeric(as.character(grade$Quiz4))
grade$Quiz.total <- as.numeric(as.character(grade$Quiz.total))


#combine first and last name
grade$User.full.name <- paste(grade$First, grade$Last, sep=" ")

#pull out useful info
grades <- grade[c("User.full.name", "Country", "Email", "Quiz1", "Quiz2", "Quiz3","Quiz4", "Quiz.total")]


grades



#select users who participated in discussion forum for each modudule
log %>%
  filter(grepl("M1", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) %>%
  unique() -> "module1"

module1$module <- 1


log %>%
  filter(grepl("M2", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) %>%
  unique() -> "module2"

module2$module <- 1


log %>%
  filter(grepl("M3", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) %>%
  unique() -> "module3"

module3$module <- 1


log %>%
  filter(grepl("M4", Event.context)) %>%
  filter(Event.name == "Some content has been posted.") %>%
  select(User.full.name) %>%
  unique() -> "module4"

module4$module <- 1


#combine tables based on user name, so it has grades and forum participation

m12 <- merge(module1, module2, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

m123 <- merge(m12, module3, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

m1234 <- merge(m123, module4, by.x = "User.full.name", by.y = "User.full.name", all= TRUE)

all.user <- merge(grades, m1234, by.x = "User.full.name", by.y = "User.full.name", all= TRUE )

colnames(all.user) [9:12]<- c("Forum1", "Forum2","Forum3", "Forum4")
write_csv(all.user, "all.csv")
#filter out who passed this time
yes <- filter(all.user, 
              Quiz1>=70 & 
                Quiz2>=70 & 
                Quiz3>=70 & 
                Quiz4 >=70 & 
                Quiz.total>= 70 &
                Forum1 == 1 &
                Forum2 == 1 &
                Forum3 == 1 &
                Forum4 == 1 )

write_csv(yes, "yes.csv")

progress_yesInclcuded <- filter(all.user,
                                Quiz1>=70 |
                                  Quiz2>=70 |
                                  Quiz3>=70 |
                                  Quiz4 >=70 |
                                  Quiz.total>= 70 |
                                  Forum1 == 1 |
                                  Forum2 == 1 |
                                  Forum3 == 1 |
                                  Forum4 == 1 )
left <- progress_yesInclcuded[ !(progress_yesInclcuded$Email %in% yes$Email), ]
left <- progress_yesInclcuded[ !(progress_yesInclcuded$Email %in% yes$Email ), ]
write_csv(left, "progress.csv")


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
                      "7",
                      "8",
                      "Email",
                      "9"
                    ))
request <- request[c("serial", "SID", "Email")]

request$Email <-  tolower(request$Email)
all.user$Email <- tolower(all.user$Email)

#clear duplicates from  the request form
request <-request[!duplicated(request$Email),]


#check their status in all.user file

verified <- merge(request, all.user, by.x = "Email", by.y = "Email", all.x = TRUE)

verified <- arrange(verified,  serial )


verified$Round <-  "1"

verified <- verified[, c(15,2,3,4,1,5:14)]

write_csv(verified, "verified.csv")

#convert NA to 0
verified[is.na(verified)] <- 0



no <- filter(verified, 
             Quiz1<70 |
               Quiz2<70 | 
               Quiz3<70 |
               Quiz4<70 |
               Quiz.total< 70 |
               Forum1==0|Forum2==0|Forum3==0|Forum4==0)

write_csv(no, "no.csv")

pass <- filter(all.user, 
               Quiz1>=70 & 
                 Quiz2>=70 & 
                 Quiz3>=70 & 
                 Quiz4 >=70 & 
                 Quiz.total>= 70 &
                 Forum1 == 1 &
                 Forum2 == 1 &
                 Forum3 == 1 &
                 Forum4 == 1 )

pass <- pass[!(pass$Email %in% verified$Email), ]
write_csv(pass, "passed_didn't_apply.csv")
