% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fdr_gam.R
\name{fdr.gam}
\alias{fdr.gam}
\title{Parallel computing fdr and power of change points estimation for different \code{gamma} and \code{nu}}
\usage{
fdr.gam(c, mu, Gamma, Nu, b, th, B = 100, level = 0.1, iter = 100)
}
\arguments{
\item{c}{number of cpu cores used for parallel computing}

\item{mu}{a vector of piecewise constant}

\item{Gamma}{a vector of different \code{gamma}s}

\item{Nu}{a vector of different \code{nu}s}

\item{b}{a scalar of location tolerance, specified by user}

\item{th}{a vector of true change points locations}

\item{B}{Montelarlo iteration times}

\item{level}{FDR control level}

\item{iter}{iteration times for each combination of \code{gamma} and \code{nu}}
}
\value{
a list of matrix with the same length as \code{Nu}, FDR and Power for different \code{Gamma} are displayed within each matrix
}
\description{
Parallel computing fdr and power of change points estimation for different \code{gamma} and \code{nu}
}
\examples{
\donttest{
size=12000
a = 1
A = a*(1:119)
H = seq(100,11900,100)
mu = GenMu(A,H,size=size)
z = GenZ(nu=2,size=size)
Gamma = seq(1,5,1)
Nu = seq(0,2,0.5)
model = fdr.gam(2,mu,Gamma,Nu,8,H,iter=100)
}
}
