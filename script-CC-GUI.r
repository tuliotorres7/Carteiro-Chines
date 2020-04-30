library(gWidgets)
library(gWidgets2)
library(gWidgets2RGtk2)
library(igraph)
library(clue)

n <- 7 # numero de vertices
i <- 1 # iterador
VerImpares <- 0
gimpar <- 0
matDist <- matrix(c(0),ncol=n,nrow = n)


fCarregarGrafo <- function(h,...) {
m <- read.table(row.names=1,file = "/Users/Tulio/Desktop/matrix.csv",header = TRUE, sep=',')
m <- as.matrix(m)
g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

}

fMostrar <- function(h,...) {

plot(g,edge.label=round(E(g)$weight, 3))
}


fEncontrarPrimos <- function(h,...) {
n<-7
dist <- distances(g)
grau <- degree(g)
i<-1
gimpar <- 0
while(i <= n){  #descobrir o numero de vertices de grau impar
  if(grau[i] %% 2 == 1){
    gimpar <- gimpar + 1 
    VerImpares[gimpar] <- i  # primeira posiÃ§Ã£o no vetor Ã© 1 nao 0
  }
  i <- i + 1
}


matDist <- matrix(c(0),ncol=n,nrow = n)

for(i in VerImpares){
  for(u in VerImpares){
    matDist[i,u] <- dist[i,u] 
  }  
}

g <- graph.adjacency(matDist, mode ="undirected",weighted = TRUE)

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

plot(g)

}

fMostrarArestasArtificiais <- function(h,...) {
n<-7
dist <- distances(g)
grau <- degree(g)
i<-1
gimpar <- 0
while(i <= n){  #descobrir o numero de vertices de grau impar
  if(grau[i] %% 2 == 1){
    gimpar <- gimpar + 1 
    VerImpares[gimpar] <- i  # primeira posiÃ§Ã£o no vetor Ã© 1 nao 0
  }
  i <- i + 1
}


matDist <- matrix(c(0),ncol=n,nrow = n)

for(i in VerImpares){
  for(u in VerImpares){
    matDist[i,u] <- dist[i,u] 
  }  
}

g <- graph.adjacency(matDist, mode ="undirected",weighted = TRUE)

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

g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)
E(g)$color<- "blue"
diag(matDist) = 1000

t <- solve_LSAP(matDist, maximum = FALSE)

t <- as.vector(t)

plot(g,edge.label=round(E(g)$weight,3))


matDist<- as.matrix(matDist)

for( i in 1:gimpar){
  if(is.na(VerImpares[i]) == FALSE){
    g <- add_edges(g, c(VerImpares[i],VerImpares[t[i]]))
    
    E(g)$weight[length(E(g)$weight)]<-matDist[i,t[i]]
    E(g)$color[length(E(g)$weight)]<- "red"
    VerImpares[t[i]] = NA
    plot(g)
  }
}
plot(g,edge.label=round(E(g)$weight,3))

}
o<-1
fNext <- function(h,...){
n <- 7 # numero de vertices
i <- 1 # iterador
VerImpares <- 0

gimpar <- 0
m <- read.table(row.names=1,file = "/Users/Tulio/Desktop/matrix.csv",header = TRUE, sep=',')

m <- as.matrix(m)

g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

plot(g,edge.label=round(E(g)$weight, 3))



dist <- distances(g)
grau <- degree(g)


while(i <= n){  #descobrir o numero de vertices de grau impar
  if(grau[i] %% 2 == 1){
    gimpar <- gimpar + 1 
    VerImpares[gimpar] <- i  # primeira posiÃÂ§ÃÂ£o no vetor ÃÂ© 1 nao 0
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

plot(g)



g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)
E(g)$color<- "blue"
diag(matDist) = 1000

t <- solve_LSAP(matDist, maximum = FALSE)

t <- as.vector(t)

plot(g,edge.label=round(E(g)$weight,3))


matDist<- as.matrix(matDist)

for( i in 1:gimpar){
  if(is.na(VerImpares[i]) == FALSE){
    g <- add_edges(g, c(VerImpares[i],VerImpares[t[i]]))
    
    E(g)$weight[length(E(g)$weight)]<-matDist[i,t[i]]
    E(g)$color[length(E(g)$weight)]<- "red"
    E(g)$color
    VerImpares[t[i]] = NA
    plot(g)
  }
}

plot(g,edge.label=round(E(g)$weight,3))

v <- "a"
caminho <- v
length(caminho)
narestas = length(E(g)$weight)
i<-1
degree(g,1)

V(g)$color <- "SkyBlue2"

for(i in 1:narestas){
  viz <- neighbors(g, v)
  qtvizinho <- length(viz)
  x <-1
  del<-TRUE
  while(del){
    del<-TRUE
    V(g)$color <- "SkyBlue2"
    V(g)[v]$color <-"Red"   
    plot(g)
    if (is_connected(delete.edges(g,E(g,P=c(v,viz[x]$name))))){
      
      E(g,P=c(v,viz[x]$name))$color<-"green"
      plot(g)
      Sys.sleep(3)
      g<- delete.edges(g,E(g,P=c(v,viz[x]$name)))
      
      del<-FALSE
      viz[x]
      caminho[length(caminho)+1] <- viz[x]$name
    }else{
      if(x == length(viz)){
        
        
        E(g,P=c(v,viz[x]$name))$color<-"green"
        plot(g)
        Sys.sleep(3)
        g<- delete.edges(g,E(g,P=c(v,viz[x]$name)))
        del<-FALSE
        plot(g)
        caminho[length(caminho)+1] <- viz[x]$name
        
        if(degree(g,v) == 0){
          plot(g)
          Sys.sleep(3)
          g<-delete_vertices(g,v)
          plot(g)
          Sys.sleep(3)
        }
      }
    }
    x<- x + 1
  }
  v<- caminho[length(caminho)]
  plot(g)
  Sys.sleep(3)
}

}
fCaminhar<- function(h,...) {

button_next<- gbutton("Next", handler= fNext)

windowNext <-gwindow("gWindowNext")

# creation of the main container
LittleGroup<-ggroup(cont=windowNext)

# creation of a subcontainer
group2<-ggroup(horizontal=FALSE, container=LittleGroup)

tmp2<-gframe("Caminhar", container=group2)
add(tmp2,button_next)

# adding a space for graphics to the main container
add(LittleGroup, ggraphics())

}



button_carregarGrafo <- gbutton("1-Carregar", handler = fCarregarGrafo)

button_mostrar <- gbutton("2-Mostrar Grafo", handler = fMostrar)

button_encontrarPrimos <- gbutton("3- Encontrar primos", handler = fEncontrarPrimos)

button_mostrarArestasArtificiais <- gbutton("4- Mostrar arestas artificiais", handler = fMostrarArestasArtificiais)

button_caminhar<- gbutton("Caminhar", handler= fCaminhar)


# creation of the main window
window<-gwindow("gWidgetsDensity")

# creation of the main container
BigGroup<-ggroup(cont=window)

# creation of a subcontainer
group<-ggroup(horizontal=FALSE, container=BigGroup)

tmp<-gframe("Quadro", container=group)
add(tmp,button_carregarGrafo)
add(tmp,button_mostrar)
add(tmp,button_encontrarPrimos)
add(tmp,button_mostrarArestasArtificiais)
add(tmp,button_caminhar)



# adding a space for graphics to the main container
add(BigGroup, ggraphics())
