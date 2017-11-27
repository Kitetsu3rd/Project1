#' what is the function does: Conditional variable Rejection Sampling
#'
#' This function implements conditional variabel rejection sampling for rvs with bounded support x,y and which have have bounded pdf.
#'
#' The second paragraph will show up somewhere else and should be addition
#'
#' @param fj the joint pdf that we are sampling from
#' @param N the number of attempted samples.
#' @param lbx lower bound of support x of fj
#' @param ubx upper bound of support x of fj
#' @param lby upper bound of support y of fj
#' @param uby upper bound of support y of fj
#' @return A vector containing samples from pdf
#' @export
#' @examples
#'jointPFF <- function(x,y){
#'ifelse(0<x & x <1 & 0<y & y<1 & 0<x+y & x+y<1, 24*x*y, 0)}
#'twoDsample(fj = jointPFF, N=100, lbx=0, ubx=1, lby=0, uby=1)
#'ggplot(twoDsample(fj = jointPFF, N=10000, lbx=0, ubx=1, lby=0, uby=1), aes(x, y)) +  geom_density_2d()

jointPFF <- function(x){
  x1 = x[1]
  x2 = x[2]
  ifelse(0<x1 & x1 <1 & 0<x2 & x2<1 & 0<x1+x2 & x1+x2<1, 24*x1*x2, 0)}

twoDsample(fj = jointPFF, N=100, lbx=0, ubx=1, lby=0, uby=1)

twoDsample <- function(fj, N, lbx, ubx, lby, uby) {
  library(cubature)
  if (missing(lbx)){
    lbx = -Inf
  }
  if (abs(adaptIntegrate(fj, c(lbx, lby), c(ubx, uby), maxEval=10000)$integral - 1) > 0.001) {
    stop("Error: not a pdf. The area under the function you given should be 1")
  }
  else{
    maxf <- max(replicate(100000,fj(c(runif(1,lbx,ubx),runif(1,lby,uby)))))
    twos = c()
    n = 0
    while (n < N) {
      two <- c(runif(1,lbx,ubx),runif(1,lby,uby))
      if (runif(1, 0, maxf) < jointPFF(two)){
        twos = c(twos, two)
        n = n + 1
      }
    }
    data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))])
  }
}

ggplot(twoDsample(fj = jointPFF, N=5000, lbx=0, ubx=1, lby=0, uby=1), aes(x, y)) +  geom_density_2d()

