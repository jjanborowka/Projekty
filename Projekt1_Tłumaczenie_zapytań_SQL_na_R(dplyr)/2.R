#zadanie 2!!
options(stringsAsFactors=FALSE)
Tags <- read.csv("Tags.csv.gz")
Users <- read.csv("Users.csv.gz")
Votes <- read.csv("Votes.csv.gz")
Posts <- read.csv("Posts.csv.gz")
PostLinks <- read.csv("PostLinks.csv.gz")
Comments <- read.csv("Comments.csv.gz")
Badges <- read.csv("Badges.csv.gz")

df_sql <- function(Posts){
df_sql_2 <-sqldf::sqldf("SELECT
          Posts.ID,
          Posts.Title,
          Posts2.PositiveAnswerCount
          FROM Posts
          JOIN (
          SELECT
          Posts.ParentID,
          COUNT(*) AS PositiveAnswerCount
          FROM Posts
          WHERE Posts.PostTypeID=2 AND Posts.Score>0
          GROUP BY Posts.ParentID
          ) AS Posts2
          ON Posts.ID=Posts2.ParentID
          ORDER BY Posts2.PositiveAnswerCount DESC
          LIMIT 10")
df_sql_2}
df_sql_2 <- df_sql(Posts)
### BASE R!!!
baze <- function(Posts){ 
Posts2 <- as.data.frame(table("ParentID"=Posts[Posts[,"PostTypeId"]==2 & Posts[,"Score"]>0,"ParentId"]),responseName = "PositiveAnswerCount")# wybieram i tworze posts2
wynik <- merge(as.data.frame(Posts[,c("Id","Title")],stringsASFacotrs=FALSE),Posts2,all.x =TRUE,all.y=FALSE,by.x="Id",by.y="ParentID")# robie Joina
wynik <- wynik[!(is.na(wynik[["PositiveAnswerCount"]])),]# Usuwam NA
d <- wynik[rev(order(wynik[["Id"]])),]# najpierw sortuje po ID
d <- d[rev(order(d[["PositiveAnswerCount"]])),]# POTEM  po answer Count
df_base_2 <- slice(d,c(1:10))# Biorę 10 pierwszych wierszy
row.names(df_base_2) <- c(1:10)# nazwy wierszzy
dplyr::all_equal(df_sql_2,df_base_2)}
#### DPLYR 
library(dplyr)
df_dplyr_2 <- function(Posts){
Posts1 <- (select(Posts,c("Id","Title")))
Posts2 <-(select(filter(Posts,PostTypeId==2&Score>0),ParentId))
Posts2 <-  Posts2 %>% group_by(ParentId) %>%  count(name = "PositiveAnswerCount")
wynik <- left_join(Posts1,Posts2,by =c("Id"="ParentId"))
wynik <- arrange(wynik,Id)
wynik<- arrange(wynik,desc(PositiveAnswerCount))
wynik <- slice(wynik,1:10)###NWM CZY MOŻE zostać
all_equal(df_sql_2,wynik)}
# NA nie usuwane bo i poco jak bire tylko 10 pierwszych 


####DATA.TABLE
library(data.table)
###SPRÓBUJ POTEM NA TESTACH BEZ KEY
df_table_2 <- function(Posts){
Posts.dt <- data.table(Posts)

Posts2.dt <- (Posts.dt[PostTypeId==2 & Score>0,.(PositiveAnswerCount=.N),by=ParentId])

wynik <- merge(Posts.dt[,list(Id,Title)],Posts2.dt,by.x="Id",by.y = "ParentId",all.x = TRUE,all.y = FALSE)
wynik <- wynik[order(Id)][order(-PositiveAnswerCount)]
wynik <- wynik[1:10]
all_equal(df_sql_2,wynik)}
library(microbenchmark)

microbenchmark(
df_sql_2<- df_sql(Posts),
df_baze <- baze(Posts),
dyplyr<- df_dplyr_2(Posts),
data.table <- df_table_2(Posts)
)

baze(Posts)

