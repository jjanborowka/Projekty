###ZADANIE 3 
options(stringsAsFactors = FALSE)
Tags <- read.csv("Tags.csv.gz")
Users <- read.csv("Users.csv.gz")
Votes <- read.csv("Votes.csv.gz")
Posts <- read.csv("Posts.csv.gz")
PostLinks <- read.csv("PostLinks.csv.gz")
Comments <- read.csv("Comments.csv.gz")
Badges <- read.csv("Badges.csv.gz")
df_sql <- function(){
df_sql_3 <- sqldf::sqldf("SELECT
Posts.Title,
UpVotesPerYear.Year,
MAX(UpVotesPerYear.Count) AS Count
FROM (
SELECT
PostId,
COUNT(*) AS Count,
STRFTIME('%Y', Votes.CreationDate) AS Year
FROM Votes
WHERE VoteTypeId=2
GROUP BY PostId, Year
) AS UpVotesPerYear
JOIN Posts ON Posts.Id=UpVotesPerYear.PostId
WHERE Posts.PostTypeId=1
GROUP BY Year")
df_sql_3}

df_sql_3 <- df_sql()
test1 <- sqldf::sqldf("SELECT
PostId,
COUNT(*) AS Count,
STRFTIME('%Y', Votes.CreationDate) AS Year
FROM Votes
WHERE VoteTypeId=2
GROUP BY PostId, Year")
#### BAZE
library(dplyr)
df_baze <- function(){
d <- substring(Votes[Votes[,"VoteTypeId"]==2,]$CreationDate,1,4)
UpVotesPerYer <- as.data.frame(cbind("PostId"=Votes[Votes[,"VoteTypeId"]==2,"PostId"],"Year"=d),stringsAsFactors = FALSE)
UpVotesPerYer <- as.data.frame(table(UpVotesPerYer),responseName = "Count",stringsAsFactors = FALSE)
UpVotesPerYer <- UpVotesPerYer[UpVotesPerYer[,"Count"]>0,][,c("PostId","Count","Year")]
UpVotesPerYer <- cbind(("PostId"=as.integer(UpVotesPerYer$PostId)),UpVotesPerYer[,c("Count","Year")])
colnames(UpVotesPerYer) <- c("PostId","Count","Year")
join <- merge(Posts,UpVotesPerYer,all=TRUE,by.x="Id",by.y="PostId")
join <- join[join[,"PostTypeId"]==1,c("Title","Year","Count")]
join<- join[rev(order(join$Count)),]
lewa <- aggregate(join$Count,by=list("Year"=join$Year),FUN = max)
prawa <-join[,c("Title","Year")]
prawa <- aggregate(join$Title,by=list("Year"=join$Year),FUN = function(x){x[1]})### PORPAW 
wynik <- cbind(prawa,lewa)
colnames(wynik) <- c("Year","Title","NWM","Count")
wynik <- wynik[,c("Year","Title","Count")]
all_equal(df_sql_3,wynik)}



#### DPLYR
df_dplyr <- function(){
Year <- filter(Votes,VoteTypeId==2) %>% pull(CreationDate)
Year <- substring(Year,1 ,4)

Year <- as.data.frame(Year)
UpVotesPerYer <- filter(Votes,VoteTypeId==2)%>% select(PostId)%>% bind_cols(Year)%>% group_by(PostId,Year) %>% count(name="Count")
wynik <- full_join(Posts,UpVotesPerYer,by=c("Id"="PostId"))
prawy <- filter(wynik,PostTypeId==1)%>% select(c(Title,Year,Count)) %>% arrange(desc(Count)) %>%group_by(Year,.drop=TRUE) %>% summarise(Title=Title[1],Count=as.integer(max(Count)))

wynik <- select(prawy,c(Title,Year,Count))
wynik <- slice(wynik,c(2:8))
all_equal(df_sql_3,wynik)}

### DATA TABLE
library(data.table)
df_data <-function(){ 
Votes.dt <- data.table(Votes)
Votes.dt <- Votes.dt[VoteTypeId==2,]
Posts.dt <- data.table(Posts)
Year <- Votes.dt[,CreationDate] 
Year <- substring(Votes.dt[,CreationDate],1,4)
UpVotesPerYear.dt <- data.table(PostId=Votes.dt[,PostId],Year=Year) 
UpVotesPerYear.dt<- UpVotesPerYear.dt[,.(Count=.N),by=.(PostId,Year)]
wynik.dt <-merge(Posts.dt,UpVotesPerYear.dt,all=TRUE,by.x="Id",by.y="PostId") 
wynik.dt <- wynik.dt[PostTypeId==1,]
wynik.dt <- wynik.dt[order(-Count),.(Title=(Title)[1],Count=max(Count)),by=Year]
wynik.dt <- wynik.dt[order(Year),.(Title,Year,Count)]
wynik.dt <- wynik.dt[1:7]
all_equal(df_sql_3,wynik.dt)}



library(microbenchmark)
microbenchmark(
  sql =df_sql(),
  baze =df_baze(),
  dplyr = df_dplyr(),
  data.table= df_data()
)


