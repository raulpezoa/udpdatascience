#' Realiza validacion cruzada de k capas para un modelo svm
#' @param formula formula del modelo svm
#' @param ncapas Numero de capas a usar 
#' @param data Set de datos
#' @param cost Costo de ubicar puntos del lado incorrecto del margen o frontera
#' @param gamma gamma, usado para todos los kernel, excepto lineal
#' @param kernel kernel a usar, puede ser linear,radial,sigmoid o polynomial
#' @param degree grado, en caso de usar kernel polynomial
#' @return Lista con sensibilidad y especificidad promedio en conjunto de validacion
#' @export 
ValidacionCruzadasvm <- function(formula,ncapas,data,cost=1,gamma=1,kernel="radial",degree=3){
  library(e1071)
  library(data.table)
  #recuperamos var dependiente
  vardepend <- as.character(formula)[2]
  data <- data.table(data)
  data$y <-   as.factor(unlist(data[,vardepend,with=F]))
  #Creamos n capas iguales
  capas <- crearCapas(data$y,k=ncapas,list=F)
  
  #creamos vector donde guardaremos resultados
  lista.sensitivity <- vector()
  lista.specificity <- vector()
  
  #Hacemos validacion cruzada n capas
  for(i in 1:ncapas){
    #segmentamos la data
    IndicesValidacion <- which(capas==i)
    DataVal <- data[IndicesValidacion, ]
    DataEntr <- data[-IndicesValidacion, ]
    svmmodel <- svm(formula,data=DataEntr,kernel=kernel,cost=cost,gamma=gamma,degree=degree,type="C-classification")
    
    
    #validamos
    DataVal$prediccionclase <- predict(svmmodel,DataVal)
    tabla<-table(pred=factor(DataVal$prediccionclase,levels = c(0,1)),true=factor(unlist(DataVal[,vardepend,with=F]),levels = c(0,1)))
    sensitivity.val <- tabla[2,2]/sum(tabla[,2])
    specificity.val <- tabla[1,1]/sum(tabla[,1])
    lista.sensitivity <- c(lista.sensitivity,sensitivity.val)
    lista.specificity <- c(lista.specificity,specificity.val)
  }
  return(list(mean.sensitivity=mean(lista.sensitivity),mean.specificity=mean(lista.specificity)))
}