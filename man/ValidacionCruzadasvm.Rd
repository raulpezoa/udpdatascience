% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidacionCruzadasvm.R
\name{ValidacionCruzadasvm}
\alias{ValidacionCruzadasvm}
\title{Realiza validacion cruzada de k capas para un modelo svm}
\usage{
ValidacionCruzadasvm(
  formula,
  ncapas,
  data,
  cost = 1,
  gamma = 1,
  kernel = "radial",
  degree = 3
)
}
\arguments{
\item{formula}{formula del modelo svm}

\item{ncapas}{Numero de capas a usar}

\item{data}{Set de datos}

\item{cost}{Costo de ubicar puntos del lado incorrecto del margen o frontera}

\item{gamma}{gamma, usado para todos los kernel, excepto lineal}

\item{kernel}{kernel a usar, puede ser linear,radial,sigmoid o polynomial}

\item{degree}{grado, en caso de usar kernel polynomial}
}
\value{
Lista con sensibilidad y especificidad promedio en conjunto de validacion
}
\description{
Realiza validacion cruzada de k capas para un modelo svm
}
