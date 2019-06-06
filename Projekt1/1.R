### ZADANIE 1 
options(stringsAsFactors=FALSE)
Tags <- read.csv("Tags.csv.gz")
Users <- read.csv("Users.csv.gz")
Votes <- read.csv("Votes.csv.gz")
Posts <- read.csv("Posts.csv.gz")
PostLinks <- read.csv("PostLinks.csv.gz")
Comments <- read.csv("Comments.csv.gz")
Badges <- read.csv("Badges.csv.gz")
df_sql <- function(){
df_sql_1<- sqldf::sqldf("SELECT
Users.DisplayName,
Users.Age,
Users.Location,
SUM(Posts.FavoriteCount) AS FavoriteTotal,
Posts.Title AS MostFavoriteQuestion,
MAX(Posts.FavoriteCount) AS MostFavoriteQuestionLikes
FROM Posts
JOIN Users ON Users.Id=Posts.OwnerUserId
WHERE Posts.PostTypeId=1
GROUP BY OwnerUserId
ORDER BY FavoriteTotal DESC
LIMIT 10")
df_sql_1      }
### BAZE 

df_sql_1 <- df_sql()
df_baze <- function(){
tmp <- Posts[Posts[,"PostTypeId"]==1,]

left <-  aggregate(tmp$FavoriteCount,by=list("OwnerUserId"=tmp$OwnerUserId),FUN= function(x){sum(x,na.rm = TRUE)})
colnames(left)[2] <-"FavoriteTotal"

righ <-  aggregate(tmp$FavoriteCount,by=list("OwnerUserId"=tmp$OwnerUserId),FUN=function(x){max(x,na.rm = TRUE,warn=-10)})
colnames(righ)[2] <- "MostFavoriteQuestionLikes"
title_max<- merge(tmp[,c("Title","OwnerUserId","FavoriteCount")],righ,by.x=c("OwnerUserId","FavoriteCount"),by.y=c("OwnerUserId","MostFavoriteQuestionLikes"))
colnames(title_max)[2] <- "MostFavoriteQuestionLikes"
title_max_sum <- merge(title_max,left,by="OwnerUserId")



wynik <- merge(Users[,c("Age","DisplayName","Location","Id")],title_max_sum,by.x="Id",by.y="OwnerUserId")
wynik <- wynik[,c("DisplayName","Age","Location","FavoriteTotal","Title","MostFavoriteQuestionLikes")]
wynik <- head(wynik[rev(order(wynik[["FavoriteTotal"]])),],10)
colnames(wynik)[5] <- "MostFavoriteQuestion"
row.names(wynik) <- 1:10
all.equal(wynik,df_sql_1)}


### Dyplyr
library(dplyr)
df_dplyr <- function(){
tmp <- filter(Posts,PostTypeId==1)
max_sum_title <- select(tmp,FavoriteCount,OwnerUserId,Title) %>% group_by(OwnerUserId)%>%summarise(FavoriteTotal = sum(FavoriteCount,na.rm = TRUE),MostFavoriteQuestionLikes=as.integer(max(FavoriteCount,na.rm=TRUE)))
max_sum_title <- inner_join(select(tmp,Title,OwnerUserId,FavoriteCount),max_sum_title,by=c("OwnerUserId"="OwnerUserId","FavoriteCount"="MostFavoriteQuestionLikes"))
max_sum_title <- rename(max_sum_title,MostFavoriteQuestionLikes=FavoriteCount)
wynik <- inner_join(select(Users,Age,DisplayName,Location,Id),max_sum_title,by=c("Id"="OwnerUserId")) %>% arrange(desc(FavoriteTotal))%>% slice(1:10) %>%
  select(DisplayName,Age,Location,FavoriteTotal,Title,MostFavoriteQuestionLikes)
wynik <- rename(wynik,MostFavoriteQuestion=Title)
wynik}


### DATA.TABLE

library(data.table)
df_data.table <- function(){
Posts.dt <- data.table(Posts)
Users.dt <- data.table(Users)

max_sum_title.dt <- Posts.dt[PostTypeId==1,.(MostFavoriteQuestion=Title[which.max(FavoriteCount)],MostFavoriteQuestionLikes=as.double(max(FavoriteCount,na.rm = TRUE))
                                             ,FavoriteTotal=as.integer(sum(FavoriteCount,na.rm = TRUE))),by=OwnerUserId]
wynik.dt <- merge(max_sum_title.dt,Users.dt[,.(Id,Age,DisplayName,Location)],by.x="OwnerUserId",by.y="Id")
wynik.dt <- wynik.dt[order(-FavoriteTotal),.(DisplayName,Age,Location,FavoriteTotal,MostFavoriteQuestion,MostFavoriteQuestionLikes=as.integer(MostFavoriteQuestionLikes))][1:10]

all_equal(df_sql_1,wynik.dt)}





library(microbenchmark)

microbenchmark(
  sql=df_sql(),
  baze =df_baze(),
  dplyr = df_dplyr(),
  data.table =df_data.table()
)
