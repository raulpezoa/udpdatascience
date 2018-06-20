#' Grafica frontera de decision para regresion logistica en dos variables
#' @param modelo Objeto glm 
#' @param data Dataframe correspondiente a los puntos a graficar
#' @return Grafico con frontera de decision
#' @export 
fronterasvm <- function(modelo,data){
  suppressMessages(library(e1071))
  suppressMessages(library(data.table))
  suppressMessages(library(scales))
  data=data.table(data)
  if (length(attr(modelo$terms,"term.labels"))!=2) {
    stop("Funciona para modelo con dos predictores")
  }
  
  y=substr(attr(modelo$terms,"variables")[2],1,nchar(attr(modelo$terms,"variables")[2]))
  x1=attr(modelo$terms,"term.labels")[1]
  x2=attr(modelo$terms,"term.labels")[2]
  tipox1=is.numeric(unlist(data[,x1,with=F]))
  tipox2=is.numeric(unlist(data[,x2,with=F]))
  
  if (!tipox1){
    nombrevar <- unique(unlist(data[,x1,with=F]))[2]
    data$x1num <- ifelse(unlist(data[,x1,with=F])==nombrevar,1,0)
  } else{
    data$x1num <- unlist(data[,x1,with=F])
  }
  
  if (!tipox2){
    nombrevar <- unique(unlist(data[,x2,with=F]))[2]
    data$x2num <- ifelse(unlist(data[,x2,with=F])==nombrevar,1,0)
  } else{
    data$x2num <- unlist(data[,x2,with=F])
  }
  
  data$y <- as.factor(unlist(data[,y,with=F]))
  kernel <- ifelse(summary(modelo)$kernel==0,"linear",ifelse(summary(modelo)$kernel==1,"polynomial",ifelse(summary(modelo)$kernel==2,"radial","sigmoid")))
  
  modelo <- svm(y~x1num+x2num,data=data,kernel=kernel,cost=summary(modelo)$cost,gamma=summary(modelo)$gamma,degree=summary(modelo)$degree,type="C-classification")
  
  
  
  m <- 101
  grid <- expand.grid(x1num=seq(min(data$x1),max(data$x1),length=m),
                      x2num=seq(min(data$x2),max(data$x2),length=m))
  
  
  yhat <- predict(modelo,grid)
  
  yhat <- ifelse(yhat==0,"blue","red")
  
  
  ###################
  xmin<-min(data$x1,na.rm = T)
  xmax<-max(data$x1,na.rm = T)
  ymin<-min(data$x2,na.rm = T)
  ymax<-max(data$x2,na.rm = T)
  plot(grid, col = alpha(yhat,0.1), pch = 20, cex = 1,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="",ylab="",xaxt="n",yaxt="n")
  par(new=T)
  plot(data[data$y==0,]$x1,data[data$y==0,]$x2,col="blue",pch=20,cex=0.3,main="",xlim=c(xmin,xmax),ylim=c(ymin,ymax),cex.lab=1.5, cex.axis=1.5, cex.main=1.5,xlab=x1,ylab=x2)
  par(new=T)
  plot(data[data$y==1,]$x1,data[data$y==1,]$x2,col="red",pch=20,cex=1,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xaxt="n",yaxt="n",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,xlab="",ylab="")
}