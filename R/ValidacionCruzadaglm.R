#' Realiza validacion cruzada de k capas para un modelo glm
#' @param formula formula del modelo glm
#' @param ncapas Numero de capas a usar 
#' @param specificity Especificidad (1-falsos positivos) a fijar en el set de entrenamiento para encontrar probabilidad de corte
#' @param data Set de datos
#' @return Lista con sensibilidad y especificidad promedio en conjunto de validacion
#' @export 
ValidacionCruzadaglm <- function(formula,ncapas,specificity,data){
  options(warn=-1)
  suppressMessages(library(ROCR))
  suppressMessages(library(data.table))
  suppressMessages(library(scales))
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
    reglog <- glm(formula,data=DataEntr,family = binomial())
    DataEntr$predprob<-predict(reglog, DataEntr,type="response")
    pr <- ROCR::prediction(DataEntr$predprob, unlist(DataEntr[,vardepend,with=F]))
    perf <- ROCR::performance(pr, measure = "sens", x.measure = "spec")
    cutoffs <- data.frame(cut=perf@alpha.values[[1]], spec=perf@x.values[[1]], 
                          sens=perf@y.values[[1]])
    probcorte <- cutoffs[cutoffs$spec<=specificity,]$cut[1]
    
    #Elegida la probabilidad de corte, validamos
    DataVal$predprob<-predict(reglog, DataVal,type="response")
    DataVal$prediccionclase <- ifelse(DataVal$predprob>=probcorte,1,0)
    tabla<-table(pred=factor(DataVal$prediccionclase,levels = c(0,1)),true=factor(unlist(DataVal[,vardepend,with=F]),levels = c(0,1)))
    sensitivity.val <- tabla[2,2]/sum(tabla[,2])
    specificity.val <- tabla[1,1]/sum(tabla[,1])
    lista.sensitivity <- c(lista.sensitivity,sensitivity.val)
    lista.specificity <- c(lista.specificity,specificity.val)
  }
  options(warn=0)
  return(list(mean.sensitivity=mean(lista.sensitivity),mean.specificity=mean(lista.specificity)))
}

