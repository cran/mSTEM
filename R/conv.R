#' Compute convolution function using FFT, similar to the function 'conv' in matlab
#' @references Matlab document on 'conv' \url{https://www.mathworks.com/help/matlab/ref/conv.html}
#' @param u vector
#' @param v vector
#' @param shape if 'same', return central part of the convolution,the same size as u;
#'              ortherwise return the whole sequence with size lenth(u)+length(v)-1
#'
#' @return a vector of convolution, as specified by shape.
#' @export
#'
#' @examples
#' u = c(-1,2,3,-2,0,1,2)
#' v = c(2,4,-1,1)
#' w = conv(u,v,'same')
conv = function(u,v,shape=c("same","full")){
  shape = match.arg(shape)
  lx <- length(u)
  ly <- length(v)
  n <- lx + ly - 1
  w <- fft(fft(c(u,rep(0,n-lx))) * fft(c(v,rep(0,n-ly))),inverse=TRUE)/n
  w = Re(w)
  if(shape=="same") w = w[floor(ly/2+1):(floor(ly/2)+lx)]
  return(w)
}




