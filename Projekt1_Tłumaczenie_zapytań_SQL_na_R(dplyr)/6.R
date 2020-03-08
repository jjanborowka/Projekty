###  ZADANIE 6
options(stringsAsFactors = FALSE)
Tags <- read.csv("Tags.csv.gz")
Users <- read.csv("Users.csv.gz")
Votes <- read.csv("Votes.csv.gz")
Posts <- read.csv("Posts.csv.gz")
PostLinks <- read.csv("PostLinks.csv.gz")
Comments <- read.csv("Comments.csv.gz")
Badges <- read.csv("Badges.csv.gz")

df_sql <- function(){
df_sql_6 <- sqldf::sqldf("SELECT DISTINCT
Users.Id,
Users.DisplayName,
Users.Reputation,
Users.Age,
Users.Location
FROM (
SELECT
Name, UserID
FROM Badges
WHERE Name IN (
SELECT
Name
FROM Badges
WHERE Class=1
GROUP BY Name
HAVING COUNT(*) BETWEEN 2 AND 10
)
AND Class=1
) AS ValuableBadges
JOIN Users ON ValuableBadges.UserId=Users.Id")
df_sql_6}
df_sql_6 <- df_sql()
test <- sqldf::sqldf("SELECT
Name, UserID
FROM Badges
WHERE Name IN (
SELECT
Name
FROM Badges
WHERE Class=1
GROUP BY Name
HAVING COUNT(*) BETWEEN 2 AND 10
)
AND Class=1
")
library(dplyr)

### BAZE 
df_baze <- function(){  
inner_tmp <-  (Badges[Badges[,"Class"]==1,"Name"])
inner_tmp<- (table(inner_tmp))
inner_tmp <- as.data.frame(inner_tmp[2<=inner_tmp& inner_tmp<=10],StringAsFactor=FALSE)
inner_tmp <- as.data.frame(as.character(inner_tmp[,"inner_tmp"]))
colnames(inner_tmp) <- "Name"
ValuableBadges <- Badges[(Badges$Name%in% inner_tmp$Name)& Badges$Class==1, c("Name","UserId")]
wynik <- merge(Users[,c("Id","DisplayName","Reputation","Age","Location")],ValuableBadges,by.x="Id",by.y="UserId")
wynik <- wynik[!duplicated(wynik$Id),1:5]
all_equal(wynik,df_sql_6)}


### DPLYR
df_dplyr <- function(){
inner_tmp <- filter(Badges,Class==1)%>%select(Name)%>%group_by(Name)%>% count(Name)%>% filter(2<=n& n<=10)%>% select(Name)
ValuableBadges <- filter(Badges,Class==1)%>% select(Name,UserId)%>% filter(Name %in% inner_tmp$Name)
wynik <- inner_join(ValuableBadges,Users,by=c("UserId"="Id"))
wynik <- distinct(select(wynik,Id=UserId,DisplayName,Reputation,Age,Location))
all_equal(df_sql_6,wynik)
}
 


### DATA.TABLR
library(data.table)
df_data.table <- function(){
Users.dt <- data.table(Users)
Badges.dt <- data.table(Badges)
inner_tmp.dt <- Badges.dt[Class==1,.N,by=Name]
inner_tmp.dt <- inner_tmp.dt[2<=N&N<=10]

ValuableBadges.dt <- Badges.dt[Name%in%inner_tmp.dt[,Name]&Class==1,.(Name,UserId)]
ValuableBadges.dt <- ValuableBadges.dt[!duplicated(ValuableBadges.dt[,UserId])]
wynik.dt <- Users.dt[ValuableBadges.dt,on=c(Id="UserId"),.(Id,DisplayName,Reputation,Age,Location)]
all_equal(df_sql_6,wynik.dt)}



library(microbenchmark)
microbenchmark(
  sql=df_sql(),
  baze = df_baze(),
  dplyr = df_dplyr(),
  data.table=df_data.table()
)

