Mnn <- function(X,M){
  #'Przymuje macierz lub data.frame
  #'Funkcja wyznacza M najblirzszych sąsiadów punktów podanych w 
  #'formacie macierzy gdzie mjesce i,j to j-ta współrzędna i-tego punktu.
  #'Zwraca macierz gdzie miesce i,j to j-ty indeks sonsiad i-tego punktu.
  n <- length(X[,1])
  X <- as.matrix(dist(X,method = "euclidean",upper = TRUE))
  X <- apply(X,1,order)
  X<- t(X[2:(M+1),])
  X
}
Mnn_graph <- function(S){
  #' Funkcja zwraca graf sąsiedstwa dla wygenrowanej wczesniej macierzy  sąsiedstwa
  # Używam pakietu igraph
  n <- nrow(S)
  m <- ncol(S)
  # Tworzę dwie ramki z jedna kolumną zawierajacą numery wierzchołków 
  # a drugą numery jednego z ich sąsiadów (wierzchołki powarzają się tyle 
  # razy ile najbliższych sąsiadów wyznaczono)
  vartex1 <- rep(1:nrow(S), each = m)
  nigerbous1 <- unlist(as.data.frame(t(S)))
  edges <- cbind(vartex1, nigerbous1)
  edges_symetrical <- cbind(vartex1, nigerbous1)
  # Używam powstałych ramek do stworzenia 0,1 macierzy sąsiedstwa 
  G <- matrix(0, nrow = n, ncol = n)
  G[edges] <- 1
  G[edges_symetrical] <- 1
  # korzystam z pakietu igraph aby z powstałej macierzy stworzyć graf
  G<- graph_from_adjacency_matrix(G,mode = "undirected")
  # dodaję krawędzie do grafu aby był on spójny 
  if (!count_components(G)==1){
    g <- !duplicated(clusters(G)$membership)
    c <- (1:n)[g]
    if (length(c)>2){
      d <- c[2:(length(c)-1)]
      d <- rep(d,each=2)
      c <- c(c[1],d,c[length(c)])
    }
    G<- add_edges(G,c)
    return(G)}
  return(G)
}
Laplacian_eigen <- function(G,k){
  #'Funkcja graf sąsiedstwa zwraca k wektorów odopwiadajacyh najmniejszym wartościom
  #'własnym laplasjanu tego grafu.
  # Korzystam z wbudowanej funkcij pakietu igraph 
  # do wyznaczenia laplasianu 
  # warto zauważyć ,że funkcij zwraca macierz rzadką
  L <- laplacian_matrix(G)
  L <- eigen(L)
  n <- length(L$value)
  g <- L$vectors[,(n):(n-k+1)]
  # Zdecydowałem się urzyć k najmnijszych 
  # ponieważ z testów wynika lepsze działanie w takim wypadku 
  g
}
Spectral_algoritm_cluster <- function(X,M,k){
  #'Funkcja wyznacza podział zbioru na k grup 
  #'używjac algorytmu spektralnego
  #' M = liczba najbliższych sąsiadów 
  W <-  Mnn(X,M)
  D <- Mnn_graph(W)
  Z <- Laplacian_eigen(D,k)
  wynik <- kmeans(Z,k)
  wynik$cluster
  
}

