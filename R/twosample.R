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

twoDsample <- function(f, N, lbx=-5000, ubx=5000, lby=-5000, uby=5000) {
  library(cubature)
  if (abs(adaptIntegrate(f, c(lbx, lby), c(ubx, uby), maxEval=10000)$integral - 1) > 0.001) {
    stop("Error: Bound is missing/wrong or the function is not a pdf. The area under the function you given should be 1")
  }
  else{
    dmvnorm = function(x,mu,sig){
      x1 = x[1]
      x2 = x[2]
      mu1 = mu[1]
      mu2 = mu[2]
      sig1 = sig[1]
      sig2 = sig[2]
      exp(-1/2*((x1-mu1)^2/sig1^2 - 2*(x1-mu1)*(x2-mu2)/sig1/sig2 + (x2-mu2)^2/sig2^2))/(2*pi*sig1*sig2)
    }
    op = optim(c((ubx+lbx)/2,(uby+lby)/2), f, control = list(fnscale = -1))
    maxf = op$value
    mu = c(op$par)
    sd = 2/maxf
    C = maxf/dmvnorm(c(mu[1],mu[2]),c(mu[1],mu[2]),c(sd,sd))
    twos = c()
    n = 0
    while (n < N) {
      two = mvrnorm(1, mu, matrix(c(sd,0,0,sd),2,2))
      if (runif(1, 0, C * dmvnorm(two,mu,c(sd,sd))) < f(two)){
        twos = c(twos, two)
        n = n + 1
      }
    }
    return(data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))]))
  }
}

