#' Illustration plot of the procedure t0 detect change points
#' @inheritParams  GenDY
#' @inheritParams  GenDY
#' @inheritParams  GenDY
#' @param whichcp output of the function \code{\link{which.cp}}
#' @inheritParams  Fdr
#' @param Tmax a vector of true peak locations
#' @param Tmin a vector true valley locations
#'
#' @return a figure plot showing detection of change points
#' @export
#' @import latex2exp
#' @examples
#' set.seed(2019)
#' L = 1200
#' A = c(2.8,0,-2.4,0,-3,0.5,3,5,2,0)/1.5
#' Tmax = c(150,410,680,770,980)
#' Tmin = c(250,320,550,1000,1100)
#' H = c(150,250,320,410,550,680,770,980,1000,1100)
#' mu = GenMu(A,H,L); z = GenZ(nu=2,L)
#' y1 = GenDY(mu=mu,z=z,gamma=6)
#' chest = ch.est(nu=2,gamma=6,size=L,B=100)
#' chp= which.cp(y1,chest,level=0.1)
#' illu.plot(mu,z,gamma=6,chp,b=5,Tmax,Tmin)
illu.plot = function(mu,z,gamma,whichcp,b,Tmax,Tmin){
  y1 = GenDY(mu=mu,z=z,gamma=gamma)
  locmax = which.peaks(y1)
  locmin = which.peaks(y1,decreasing = T)
  oldpar = par(mfrow=c(1,2),mar=c(4,4,1,1))
  on.exit(par(oldpar))
  plot(mu+z,xlab="",ylab="",type="l",col="blue",bty="l")
  title(xlab="t",ylab="y",mgp=c(2.5,2.5,0),cex.lab=1.0)
  plot(y1,xlab="",ylab="",type="l",col="blue",lwd=2,bty="l")
  title(xlab="t",ylab=latex2exp::TeX("$y_{\\gamma}'"),mgp=c(2.5,2.5,0),cex.lab=1.0)
  abline(h=whichcp$thresh,lty=2)
  abline(h=-whichcp$thresh,lty=2)
  abline(h=0)
  points(locmax,y1[locmax],pch=16,col="green")
  points(locmin,y1[locmin],pch=16,col="red")
  points(whichcp$peak,y1[whichcp$peak],pch=24,col="green")
  points(whichcp$vall,y1[whichcp$vall],pch=25,col="red")
  for(i in 1:length(Tmax)) rect(Tmax[i]-b,0,Tmax[i]+b,1,col="cyan",density=0)
  for(i in 1:length(Tmin)) rect(Tmin[i]-b,0,Tmin[i]+b,-1,col="red",density=0)
}
