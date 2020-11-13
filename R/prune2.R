#' Poda arbol en profundidad deseada
#' @param arbol arbol profundo
#' @param i nivel deseado
#' @export 

prune2 <- function(arbol,i){
  options(warn=-1)
  suppressMessages(library(rpart))
  options(warn=0)
  cp <- arbol$cptable[,1]
  if (length(cp)<i){
    i <- length(cp)
  }
  cp.deseado <- cp[i]*1.0000001
  arbol.podado <- prune(arbol,cp=cp.deseado)
  return(arbol.podado)
}