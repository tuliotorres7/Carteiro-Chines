library(gWidgets)
library(gWidgets2)
library(gWidgets2RGtk2)
library(igraph)
library(clue)



fCarregarGrafo <- function(h,...) {


n <- 7 # numero de vertices
i <- 1 # iterador
VerImpares <- 0
gimpar <- 0




m <- read.table(row.names=1,file = "/Users/Tulio/Desktop/matrix.csv",header = TRUE, sep=',')
m <- as.matrix(m)
g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

}

fNext <- function(h,...) {
plot(g,edge.label=round(E(g)$weight, 3))
}


fMostrar <- function(h,...) {
plot(g,edge.label=round(E(g)$weight, 3))
}


fEncontrarPrimos <- function(h,...) {

gimpar <- 0
dist <- distances(g)
grau <- degree(g)

while(i <= n){  #descobrir o numero de vertices de grau impar
  if(grau[i] %% 2 == 1){
    gimpar <- gimpar + 1 
    VerImpares[gimpar] <- i  # primeira posiÃ§Ã£o no vetor Ã© 1 nao 0
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

}


button_carregarGrafo <- gbutton("1-Carregar", handler = fCarregarGrafo)

button_mostrar <- gbutton("2-Mostrar Grafo", handler = fMostrar)

button_encontrarPrimos <- gbutton("3- Encontrar primos", handler = fEncontrarPrimos)

button_next<- gbutton("next", handler= fnext)




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
add(tmp,button_next)



# adding a space for graphics to the main container
add(BigGroup, ggraphics())
