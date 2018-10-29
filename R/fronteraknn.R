#' Grafica frontera de decision para knn en dos variables
#' @param modelo Objeto knn
#' @param data Dataframe correspondiente a los puntos a graficar
#' @param prob Probabilidad de corte para decidir clase
#' @return Grafico con frontera de decision
#' @export 
fronteraknn <- function(modelo,data,prob){
  options(warn=-1)
  suppressMessages(library(ROCR))
  suppressMessages(library(data.table))
  suppressMessages(library(scales))
  options(warn=0)
  
  data <- data.table(data)
  if (nrow(attr(modelo$terms,"factors"))!=3) {
    stop("Funciona para modelo con dos predictores")
  }
  
  y=row.names(attr(modelo$terms,"factors"))[1]
  x1=row.names(attr(modelo$terms,"factors"))[2]
  x2=row.names(attr(modelo$terms,"factors"))[3]

  data$y <- unlist(data[,y,with=F])
  data$x1num <- unlist(data[,x1,with=F])
  data$x2num <- unlist(data[,x2,with=F])
  
  m <- 101
  grid <- expand.grid(x1num=seq(min(data$x1num),max(data$x1num),length=m),
                      x2num=seq(min(data$x2num),max(data$x2num),length=m))
  grid2 <- grid
  names(grid2) <- c(x1,x2)
  yhat <- predict(modelo,grid2,type="prob")[,2]
  yhat <- ifelse(yhat>prob,"red","blue")
  
  ###################
  xmin<-min(data$x1num,na.rm = T)
  xmax<-max(data$x1num,na.rm = T)
  ymin<-min(data$x2num,na.rm = T)
  ymax<-max(data$x2num,na.rm = T)
  plot(grid, col = alpha(yhat,0.1), pch = 20, cex = 1,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="",ylab="",xaxt="n",yaxt="n")
  par(new=T)
  plot(data[data$y==0,]$x1num,data[data$y==0,]$x2num,col="blue",pch=20,cex=0.3,main="",xlim=c(xmin,xmax),ylim=c(ymin,ymax),cex.lab=1.5, cex.axis=1.5, cex.main=1.5,xlab=x1,ylab=x2)
  par(new=T)
  plot(data[data$y==1,]$x1num,data[data$y==1,]$x2num,col="red",pch=20,cex=1,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xaxt="n",yaxt="n",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,xlab="",ylab="")
}