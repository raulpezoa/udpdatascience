#' Devuelve formula en formato >=0 para modelo glm
#' @param modelo Modelo glm
#' @param spec Especificidad
#' @return Formula en formato >=0 para modelo glm
#' @export 
formulaglm <- function(modelo,spec=0.8){
 
  b0 <- as.numeric(modelo$coefficients[1])
  p0 <- probcorte(modelo,modelo$data,spec=spec,grafico=F)$prob
  formula <- b0+log((1-p0)/p0)
  for (var in 2:length(modelo$coefficients)){
    formula <- paste(formula,ifelse(as.numeric(modelo$coefficients[var])>=0,"+",""),as.numeric(modelo$coefficients[var]),"*",names(modelo$coefficients[var]),sep="")
    names(modelo$coefficients[2])
  }
  return(formula)
}