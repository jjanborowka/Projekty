# W tym pliku znajdują się kod który był pierwotnie rozbity na wiele 
# różnych plików dlatego wiele wczytań pakietów itp się powtarza 
# zaznaczę kiedy zaczyna się następny plik 
# Nazwy plików csv również mogą być nie poprawne z racij tego , że każdy z tych plików był używany wielokrotnie 

##PLIK uzywany do testoania jednej konkretnej metody algorytmu na całej paczce zbiorów

library(microbenchmark)
library(mclust)
library(dendextend)

library(dbscan)


working_directory <- getwd()
files_directory <- file.path(working_directory, "Zbiory_benchmarkowe")
zbiory <- list.files(files_directory, "data\\.gz$", recursive=TRUE)
etykiety <- list.files(files_directory, "labels0\\.gz$", recursive=TRUE)
pliki <- cbind(zbiory, etykiety)
liczba_zbiorow <- length(zbiory)
blad1<- cbind("błąd","błąd","błąd","błąd")
for (i in 1:46){
  
  set <- paste(files_directory, pliki[i, 1], sep = "/")
  label <- paste(files_directory, pliki[i, 2], sep = "/")
  set_name <- sub('.*/', '', set)
  set <- read.table(set)
  label <- read.table(label)
  label <- as.vector(label[,1])
  k <- max(unique(label))
  hclust_fast <- function(){
    czas <- microbenchmark(podzial <- cutree(hclust(dist(set),method = "median"),k=k),times = 10,unit = "ms")
    rn <- adjustedRandIndex(label,podzial)
    fm <- FM_index(label,podzial)
    czas <- summary(czas)
    czas <- czas$mean
    dane1 <- cbind(set_name,rn,fm,czas)
    write.table(dane1, "TESTY/fast_czynapewno.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}
  tryCatch(hclust_fast(),error= function(e){beepr::beep(3)
    write.table(blad1, "TESTY/hclust_fast_poprawa.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})
  
}

## PLIk używany dotestów wielu metod naraz na całej grupie zbiorów lub próbce uzywany też do testów standaryzacij
library(dbscan)
library(genie)
library(microbenchmark)



working_directory <- getwd()
files_directory <- file.path(working_directory, "Zbiory_benchmarkowe")
zbiory <- list.files(files_directory, "data\\.gz$", recursive=TRUE)
etykiety <- list.files(files_directory, "labels0\\.gz$", recursive=TRUE)
pliki <- cbind(zbiory, etykiety)
liczba_zbiorow <- length(zbiory)
dane <-cbind("Nazwa Zbioru","Indeks Randa", "FM","Parametr M","Czas")
write.table(dane, "TESTY/moj_s.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)
dane <- cbind("Nazwa Zbioru","Indeks Randa", "FM","Czas")
write.table(dane, "TESTY/genie_s.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)
write.table(dane, "TESTY/genie_turbo_s.css", sep = ",", append = T,col.names =FALSE ,row.names = FALSE)
write.table(dane, "TESTY/hclust_fast_s.csv", sep = ",", append = T,col.names =FALSE ,row.names = FALSE)
write.table(dane, "TESTY/hclust_slow_s.csv", sep = ",", append = T,col.names = FALSE ,row.names = FALSE)
dane <-cbind("Nazwa Zbioru","Indeks Randa", "FM","Parametr M","Czas","minPoint")
write.table(dane, "TESTY/hdbscan_1.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)

blad1 <- cbind("bład","bład","bład","bład")
blad2 <- cbind("błąd","błąd","błąd","błąd","błąd")
probka <- sample(1:46,15)
for (i in 1:46 ){
  
  set <- paste(files_directory, pliki[i, 1], sep = "/")
  label <- paste(files_directory, pliki[i, 2], sep = "/")
  set_name <- sub('.*/', '', set)
  set <- read.table(set)
  label <- read.table(label)
  label <- label$V1
  k <- max(unique(label))
  #HDBSCAN
  hdb <- function(){
    for (t in c(10,20,100,50,5)){
      czas <- microbenchmark(podzial <- hdbscan(set,minPts = t)$cluster,times = 1,unit = "ms")
      rn <- adjustedRandIndex(label,podzial)
      fm <- FM_index(label,podzial)
      czas <- summary(czas)
      czas <- czas$mean
      dane1 <- cbind(set_name,rn,fm,czas,t)
      write.table(dane1, "TESTY/hdbscan1_S.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}}
  tryCatch(hdb(),error= function(e){beepr::beep(3)
    write.table(blad2, "TESTY/hdbscan_1.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})

#HCLUST_SLOW
hclust_slow <- function(){
  czas <- microbenchmark(podzial <- cutree(hclust(dist(set),method = "ward.D2"),k=k),times = 1,unit = "ms")
  rn <- adjustedRandIndex(label,podzial)
  fm <- FM_index(label,podzial)
  czas <- summary(czas)
  czas <- czas$mean
  dane1 <- cbind(set_name,rn,fm,czas)
  write.table(dane1, "TESTY/hclust_slow.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}
tryCatch(hclust_slow(),error= function(e){beepr::beep(3)
  write.table(blad1, "TESTY/hclust_slow_s.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})
#HCLUST_FAST
hclust_fast <- function(){
  czas <- microbenchmark(podzial <- cutree(hclust(dist(set),method = "single"),k=k),times = 1,unit = "ms")
  rn <- adjustedRandIndex(label,podzial)
  fm <- FM_index(label,podzial)
  czas <- summary(czas)
  czas <- czas$mean
  dane1 <- cbind(set_name,rn,fm,czas)
  write.table(dane1, "TESTY/hclust_fast.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}
tryCatch(hclust_fast(),error= function(e){beepr::beep(3)
  write.table(blad1, "TESTY/hclust_fast_s.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})
#Genie
genie1 <- function(){
  czas <- microbenchmark(podzial <- cutree(hclust2(dist(set)),k=k),times = 1,unit = "ms")
  rn <- adjustedRandIndex(label,podzial)
  fm <- FM_index(label,podzial)
  czas <- summary(czas)
  czas <- czas$mean
  dane1 <- cbind(set_name,rn,fm,czas)
  write.table(dane1, "TESTY/genie.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}
tryCatch(genie1(),error= function(e){beepr::beep(3)
  write.table(blad1, "TESTY/genie_s.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})
# genie_TURBO
genie_turbo <- function(){
  czas <- microbenchmark(podzial <- cutree(hclust2(dist(set),useVpTree = TRUE),k=k),times = 1,unit = "ms")
  rn <- adjustedRandIndex(label,podzial)
  fm <- FM_index(label,podzial)
  czas <- summary(czas)
  czas <- czas$mean
  dane1 <- cbind(set_name,rn,fm,czas)
  write.table(dane1, "TESTY/genie_turbo.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}
tryCatch(genie_turbo(),error= function(e){beepr::beep(3)
  write.table(blad1, "TESTY/genie_turbo_.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})
# MOJ
M <- c(k,k+2,k+6)
for (z in M){
  moj <- function(){
    czas <- microbenchmark(podzial <- Spectral_algoritm_cluster(set,z,k),times = 1,unit = "ms")
    rn <- adjustedRandIndex(label,podzial)
    fm <- FM_index(label,podzial)
    czas <- summary(czas)
    czas <- czas$mean
    dane1 <- cbind(set_name,rn,fm,z,czas)
    write.table(dane1, "TESTY/moj.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}
  tryCatch(moj(),error= function(e){beepr::beep(3)
    write.table(blad1, "TESTY/moj_s.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})
}
}

# PLik do testów metod funckij hclust
library(microbenchmark)
library(mclust)
library(dendextend)
working_directory <- getwd()
files_directory <- file.path(working_directory, "Zbiory_benchmarkowe")
zbiory <- list.files(files_directory, "data\\.gz$", recursive=TRUE)
etykiety <- list.files(files_directory, "labels0\\.gz$", recursive=TRUE)
pliki <- cbind(zbiory, etykiety)
probka <- sample(1:46,13) 
dane <- cbind("Nazwa _zbioru","Metoda", "RN","FM","Czas")
write.table(dane, "TESTY/hclust_test_poprwa.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)
metody <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
k <- 1
blad <- cbind("błąd","błąd","błąd","błąd","błąd")
for (i in probka){
  
  set <- paste(files_directory, pliki[i, 1], sep = "/")
  label <- paste(files_directory, pliki[i, 2], sep = "/")
  set_name <- sub('.*/', '', set)
  set <- read.table(set)
  label <- read.table(label)
  label <- as.vector(label[,1])
  kpo <- max(unique(label))
  for (j in metody){ 
    gosia <-function(j,i)  {
      qt <- microbenchmark(q <- cutree(hclust(dist(set),method = j),k=kpo),times = 10,unit = "ms")
      qt <- summary(qt)
      qt <- qt$mean
      qr <- adjustedRandIndex(label,q)
      qm <- FM_index(label,q)
      dane <- cbind(set_name,j,qr,qm,qt)
      write.table(dane, "TESTY/hclust_test_czynapewno.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)}
    tryCatch(gosia(j,i),error= function(e){write.table(blad, "TESTY/hclust_test_poprwa.csv", sep = ",", append = T,col.names = FALSE,row.names = FALSE)})
  }
  k <- 1+k
}



