#' Grafica frontera de decision para regresion logistica en dos variables
#' @param modelo Objeto glm 
#' @param data Dataframe correspondiente a los puntos a graficar
#' @param prob Probabilidad de corte para decidir clase
#' @return Grafico con frontera de decision
#' @export 
fronteraglm <- function(modelo,data,prob){
  options(warn=-1)
  suppressMessages(library(ROCR))
  suppressMessages(library(data.table))
  suppressMessages(library(scales))
  options(warn=0)
  
  data <- data.table(data)
  if (length(modelo$coefficients)!=3) {
    stop("Funciona para modelo con dos predictores")
  }
  
  y=all.vars(formula(modelo))[1]
  x1=all.vars(formula(modelo))[2]
  x2=all.vars(formula(modelo))[3]
  beta0=modelo$coefficients[1]
  beta1=modelo$coefficients[2]
  beta2=modelo$coefficients[3]
  tipox1=is.numeric(unlist(data[,x1,with=F]))
  tipox2=is.numeric(unlist(data[,x2,with=F]))
  
  if (!tipox1){
    nombrevar <- names(modelo$coefficients)[2]
    nombrevar <- gsub(x1,"",nombrevar)
    data$x1num <- ifelse(unlist(data[,x1,with=F])==nombrevar,1,0)
  } else{
    data$x1num <- unlist(data[,x1,with=F])
  }
  
  if (!tipox2){
    nombrevar <- names(modelo$coefficients)[3]
    nombrevar <- gsub(x2,"",nombrevar)
    data$x2num <- ifelse(unlist(data[,x2,with=F])==nombrevar,1,0)
  } else{
    data$x2num <- unlist(data[,x2,with=F])
  }
  
  data$y <- unlist(data[,y,with=F])
  
  m <- 101
  grid <- expand.grid(x1num=seq(min(data$x1num),max(data$x1num),length=m),
                      x2num=seq(min(data$x2num),max(data$x2num),length=m))
  yhat <- (ifelse(1/(1+exp(-(beta0+beta1*grid$x1num+beta2*grid$x2num)))>prob,"red","blue"))
  
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