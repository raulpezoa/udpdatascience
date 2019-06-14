#' Obtiene probabilidad de corte para especifidad determinada
#' @param modelo Objeto glm 
#' @param data Dataframe con datos
#' @param spec Especificidad buscada
#' @return Curva ROC, AUC y probabilidad de corte
#' @export 
probcorte <- function(modelo,data,spec,tipo="glm",grafico=T){
  options(warn=-1)
  suppressMessages(library(ROCR))
  suppressMessages(library(data.table))
  suppressMessages(library(scales))
  options(warn=0)
  data <- data.table(data)
  if (tipo=="glm"){
    y=all.vars(formula(modelo))[1]
    data$y <- unlist(data[,y,with=F])
    predprob <- predict(modelo,data,type='response')
    pr <- prediction(predprob, data$y)
    perf <- performance(pr, measure = "sens", x.measure = "spec")
    if (grafico){
      plot(perf)
    }
    cutoffs <- data.frame(cut=perf@alpha.values[[1]], spec=perf@x.values[[1]], 
                          sens=perf@y.values[[1]])
    prob <- cutoffs[cutoffs$spec<=spec,]
    prob <- prob$cut[1]
    
    perf <- performance(pr, measure = "auc")
    auc <- unlist(perf@y.values)
    return(list(prob=prob,auc=auc))
  } else if (tipo=="knn"){
    y=row.names(attr(modelo$terms,"factors"))[1]
    data$y <- unlist(data[,y,with=F])
    predprob <- predict(modelo,data,type='prob')[,2]
    pr <- prediction(predprob, data$y)
    perf <- performance(pr, measure = "sens", x.measure = "spec")
    if (grafico){
      plot(perf)
    }
    
    cutoffs <- data.frame(cut=perf@alpha.values[[1]], spec=perf@x.values[[1]], 
                          sens=perf@y.values[[1]])
    prob <- cutoffs[cutoffs$spec>=spec,]
    prob <- prob$cut[nrow(prob)]
    perf <- performance(pr, measure = "auc")
    auc <- unlist(perf@y.values)
    return(list(prob=prob,auc=auc))
  }
}
