# zadanie 4 

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
df_sql_4<- sqldf::sqldf("SELECT
Questions.Id,
Questions.Title,
BestAnswers.MaxScore,
Posts.Score AS AcceptedScore,
BestAnswers.MaxScore-Posts.Score AS Difference
FROM (
SELECT Id, ParentId, MAX(Score) AS MaxScore
FROM Posts
WHERE PostTypeId==2
GROUP BY ParentId
) AS BestAnswers
JOIN (
SELECT * FROM Posts
WHERE PostTypeId==1
) AS Questions
ON Questions.Id=BestAnswers.ParentId
JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
WHERE Difference>50
ORDER BY Difference DESC")
df_sql_4
}
test1 <- sqldf::sqldf("SELECT Id, ParentId, MAX(Score) AS MaxScore
FROM Posts
WHERE PostTypeId==2
GROUP BY ParentId")
library(dplyr)

df_sql_4 <- df_sql()
### BAZE
df_baze <- function(){
w <- Posts[Posts[,"PostTypeId"]==2,]
BestAnswer1 <- aggregate(w$Score,by=list("ParentId"=w$ParentId),max)
BestAnswer <- merge(BestAnswer1,w[,c("ParentId","Id","Score")],by.x=c("ParentId","x"),by.y=c("ParentId","Score"),all=FALSE)
BestAnswer <- BestAnswer[!duplicated(BestAnswer$ParentId),]
colnames(BestAnswer)[2] <- "MaxScore"
all_equal(BestAnswer,test1)
Question <-  (Posts[Posts[,"PostTypeId"]==1,])
Przejscie <- merge(Question,BestAnswer,all=TRUE,by.x="Id",by.y="ParentId",sosrt=TRUE)
wynik <- merge(Przejscie,Posts[,c("Score","Id")],all.x=FALSE,all.y=TRUE,by.x="AcceptedAnswerId",by.y="Id",sort=TRUE)
diff <-  wynik$MaxScore -Posts$Score
wynik <- cbind(wynik,diff)
wynik <- wynik[wynik[,"diff"]>50,c("Id","Title","MaxScore","Score.y","diff")]
wynik <- na.omit(wynik)
wynik <- wynik[rev(order(wynik[,"diff"])),]
colnames(wynik) <- c("Id","Title","MaxScore","AcceptedScore","Difference")
row.names(wynik) <- c(1:length(wynik[["Id"]]))
all_equal(wynik,df_sql_4)}

df_base()

###DPLYR
df_dplyr <- function(){
BestAnswer <- select(filter(Posts,PostTypeId==2),c(Id,ParentId,Score))%>% group_by(ParentId) %>% arrange(-Score,.by_group=TRUE) %>%summarise("Id"=Id[1],"MaxScore"=as.integer(max(Score)))%>%
              select(Id,ParentId,MaxScore)
Question <-  filter(Posts,PostTypeId==1)

join1 <- inner_join(Question,BestAnswer,by=c("Id"="ParentId"))
join2 <- right_join(join1,Posts,by=c("AcceptedAnswerId"="Id"))

wynik <- select(join2,Id,Title.x,MaxScore)
wynik <- rename(wynik,Id=Id,Title=Title.x,MaxScore=MaxScore)
wynik <- bind_cols(wynik,"AcceptedScore"=Posts$Score,"Difference"=(wynik$MaxScore-Posts$Score))
wynik <- filter(wynik,Difference>50) %>% arrange(-Difference)
all_equal(df_sql_4,wynik)}

###DATA.TABLE 
library(data.table)
df_datatable <- function(){
Posts.dt <- data.table(Posts)
BestAnswer.dt <- Posts.dt[order(-Score)][PostTypeId==2,.(Id[1],max(Score)),by=ParentId]
BestAnswer.dt <- BestAnswer.dt[,.(Id=V1,ParentId,MaxScore=V2)][order(ParentId)]

Question.dt <- Posts.dt[PostTypeId==1,] 

join1.dt <- merge(Question.dt,BestAnswer.dt,all.x=TRUE,all.y=TRUE,by.x="Id",by.y="ParentId")
join2.dt <- merge(join1.dt,Posts.dt[,.(Id)],all.y=TRUE,by.x="AcceptedAnswerId",by.y="Id")
wynik.dt <- data.table(Id=join2.dt[,Id],Title=join2.dt[,Title],MaxScore=join2.dt[,MaxScore],AcceptedScore=Posts.dt[,Score],Difference=(join2.dt[,MaxScore]-Posts.dt[,Score]))
wynik.dt <- wynik.dt[Difference>50][order(-Difference)]
all_equal(wynik.dt,df_sql_4)}
  
library(microbenchmark)

microbenchmark(
  sql=df_sql(),
  baze=df_baze(),
  dplyr= df_dplyr(),
  data.table=df_datatable()
)