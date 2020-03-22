library(igraph)
n <- 7 # numero de vertices
i <- 1 # iterador
VerImpares <- 0

gimpar <- 0
m <- read.table(row.names=1,file = file.choose(),header = TRUE, sep=',')

m <- as.matrix(m)
g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)
plot(g)
plot(g,edge.label=round(E(g)$weight, 3))
adjlist <-get.adjedgelist(g)
dist <- distances(g)
adj <- get.adjacency(g)
grau <- degree(g)
while(i <= n){  #descobrir o numero de vertices de grau impar
  if(grau[i] %% 2 == 1){
    gimpar <- gimpar + 1 
    VerImpares[gimpar] <- i  # primeira posição no vetor é 1 nao 0
  }
  i <- i + 1
}
i <- 0
matDist <- matrix(c(0),ncol=n,nrow = n)

for(i in VerImpares){
  for(u in VerImpares){
    matDist[i,u] <- dist[i,u] 
  }  
}
g <- graph.adjacency(matDist, mode ="undirected",weighted = TRUE)
for(i in VerImpares){
print(letters[i])
}

matDist
x <- gimpar
for(i in n:1){
  print(i)
}
for(i in n:1){
  print(i)
  print(VerImpares[x])
  if( i != VerImpares[x]){
    print("uauauauaua")
matDist <- matDist[(-i),]
matDist <- matDist[,(-i)]
  }else{
    x<- x-1 
        if(x==0){
          x=1 # nao deixar o iterador chegar a 0
  }
  }
}

  