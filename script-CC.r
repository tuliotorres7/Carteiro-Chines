library(igraph)
library(clue)


n <- 7 # numero de vertices
i <- 1 # iterador
VerImpares <- 0

gimpar <- 0
m <- read.table(row.names=1,file = "/home/tulio/Área de Trabalho/amatrix.csv",header = TRUE, sep=',')

m <- as.matrix(m)

g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

round(E(g)$weight,3)



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
  if( i != VerImpares[x]){
matDist <- matDist[(-i),]
matDist <- matDist[,(-i)]
  }else{
    x<- x-1 
        if(x==0){
          x=1 # nao deixar o iterador chegar a 0
  }
  }
}



matDist
diag(matDist) = 1000
plot(g)
g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

t <- solve_LSAP(matDist, maximum = FALSE)

t <- as.vector(t)

plot(g,edge.label=round(E(g)$weight,3))
matDist<- as.matrix(matDist)
typeof(matDist)
for( i in 1:gimpar){
  if(is.na(VerImpares[i]) == FALSE){
  g <- add_edges(g, c(VerImpares[i],VerImpares[t[i]]))
  
  E(g)$weight[length(E(g)$weight)]<-matDist[i,t[i]]
  
  VerImpares[t[i]] = NA
  plot(g)
  }
}

plot(g,edge.label=round(E(g)$weight,3))

#tkplot(g,edge.label=round(E(g)$weight,3))

is_connected(g)
clu <- components(g)
print("Vertice inicial, letra")
#v <- scan()
v <- "a"
caminho <- v
length(caminho)
narestas = length(E(g)$weight)
i<-1
degree(g,1)
for(i in 1:narestas){
 i <- i+1
    print(i)
    viz <- neighbors(g, v)
    viz
    #viz<- as.integer(viz)
    qtvizinho <- length(viz)
    x <-1
    
    while(length(neighbors(g, v)) == qtvizinho){
      plot(g)
      #if (is_connected(g - edge(c(v,viz[x])))){
      viz[x]$name
      viz[x]
      if (is_connected(delete.edges(g,E(g,P=c(v,viz[x]$name))))){
        plot(g)
        v
        viz[x]
        #g <- g - edge(c(v,viz[x]))
        g<- delete.edges(g,E(g,P=c(v,viz[x]$name)))
        plot(g)
        viz[x]
        
        caminho[length(caminho)+1] <- viz[x]$name
        viz[x]
        caminho
        print("degree")
        print(degree(g,v))
        
        }else{
          if(x == length(viz)){
          g<- delete.edges(g,E(g,P=c(v,viz[x]$name)))
          plot(g)
          
      class(k <- viz[x]$name)
      viz[x]$name
      viz[x]
      viz$name
          
      
      letters[viz]
          caminho[length(caminho)+1] <- viz[x]$name
          if(degree(g,v) == 0){
            print("deletaaa")
            print(g,v)
            g<-delete_vertices(g,v)
            plot(g)
            } 
        }
      }
    x<- x + 1
    v<- caminho[length(caminho)]
      }
plot(g)
caminho    
}
caminho
