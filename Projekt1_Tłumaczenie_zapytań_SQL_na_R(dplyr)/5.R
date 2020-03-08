# zadanie 5 

options(stringsAsFactors=FALSE)
Tags <- read.csv("Tags.csv.gz")
Users <- read.csv("Users.csv.gz")
Votes <- read.csv("Votes.csv.gz")
Posts <- read.csv("Posts.csv.gz")
PostLinks <- read.csv("PostLinks.csv.gz")
Comments <- read.csv("Comments.csv.gz")
Badges <- read.csv("Badges.csv.gz")
library(dplyr)
df_sql <- function(){
df_sql_5 <- sqldf::sqldf("SELECT
Posts.Title,
CmtTotScr.CommentsTotalScore
FROM (
SELECT
PostID,
UserID,
SUM(Score) AS CommentsTotalScore
FROM Comments
GROUP BY PostID, UserID
) AS CmtTotScr
JOIN Posts ON Posts.ID=CmtTotScr.PostID AND Posts.OwnerUserId=CmtTotScr.UserID
WHERE Posts.PostTypeId=1
ORDER BY CmtTotScr.CommentsTotalScore DESC
LIMIT 10")
df_sql_5
}
df_sql_5 <- df_sql()
test <-  sqldf::sqldf("SELECT
PostID,
UserID,
SUM(Score) AS CommentsTotalScore
FROM Comments
GROUP BY PostID, UserID")
### BAZE 
df_baze <- function(){
Posts2 <- Comments[,c("PostId","UserId","Score")]
Posts2 <- aggregate(Posts2$Score,by=list("PostId"=Posts2$PostId,"UserId"=Posts2$UserId),sum)
colnames(Posts2) <- c("PostId","UserId","CommentsTotalScore")
Posts1 <- Posts[Posts[,"PostTypeId"] ==1,c("Title","Id","OwnerUserId")]
wynik <- merge(Posts1,Posts2,by.x=c("Id","OwnerUserId"),by.y=c("PostId","UserId"))
wynik <- na.omit(wynik)
wynik <- wynik[,c("Title","CommentsTotalScore")]
wynik <- wynik[rev(order(wynik[["CommentsTotalScore"]])),]
wynik <- slice(wynik,c(1:10))
rownames(wynik) <- c(1:10)
all_equal(df_sql_5,wynik)}
assertthat::assert_that(df_baze())
library(dplyr)

df_dplyr <- function(){
CmtTotScr <- select(Comments,c("PostId","UserId","Score")) %>% group_by(PostId,UserId) %>%
  summarise("CommentsTotalScore" = sum (Score))
Posts1 <- select(filter(Posts,PostTypeId==1),c("Title","Id","OwnerUserId"))
wynik <- left_join(Posts1,CmtTotScr,by=c("Id"="PostId","OwnerUserId"="UserId")) %>%
  select(Title,CommentsTotalScore) %>% arrange(desc(CommentsTotalScore))
df_dplyr_5 <- slice(wynik,1:10)
all_equal(df_sql_5,df_dplyr_5)
}
### data.table
library(data.table)
df_table <- function(){
Comments.dt <- data.table(Comments)
Posts.dt <-  data.table(Posts)
CmtTotScr.dt <- Comments.dt[,CommentsTotalScore:=.(sum(Score)),keyby=.(PostId,UserId)][,.(PostId,UserId,CommentsTotalScore)]
wynik.dt <- merge(Posts.dt[PostTypeId==1,.(Title,Id,OwnerUserId)],CmtTotScr.dt,by.x =c("Id","OwnerUserId"),by.y=c("PostId","UserId"),all.x = TRUE,all.y=FALSE)
wynik.dt <- wynik.dt[!(duplicated(wynik.dt))]
df_data.table_5 <- head(wynik.dt[,.(Title,CommentsTotalScore)][order(-CommentsTotalScore)],10)
all_equal(df_sql_5,df_data.table_5)}

library(microbenchmark)

microbenchmark(
sql = df_sql(),
baze = df_baze(),
dplyr = df_dplyr(),
data.tabl = df_table()
)