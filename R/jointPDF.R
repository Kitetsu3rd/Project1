#' what is the function does: Conditional variable Rejection Sampling
#'
#' This function implements conditional variabel rejection sampling for rvs with bounded support x,y and which have have bounded pdf.
#'
#' The second paragraph will show up somewhere else and should be addition
#'
#' @param fj the joint pdf that we are sampling from
#' @param N the number of attempted samples.
#' @param lbx lower bound of support x of f
#' @param ubx upper bound of support x of f
#' @param lby upper bound of support y of f
#' @param uby upper bound of support y of f
#' @param maxfj bond of fj
#'
#' @return A vector containing samples from pdf
#' @export
#'
#' @examples
#'
#'jointPDF <- function(x,y){
#'ifelse(0<x & x <1 & 0<y & y<1 & 0<x+y & x+y<1, 24*x*y, 0)}
#'twoDsample(f = jointPDF, N=100, lbx=0, ubx=1, lby=0, uby=1, maxfj = 6)

twoDsample <- function(fj, N, lbx, ubx, lby, uby, maxfj) {
  two <- replicate(N,c(runif(1,lbx,ubx),runif(1,lby,uby)))
  unis <- runif(N, 0, maxfj)
  f1 = -1
  f3 = 0
  twos = c()
  for (i in 1:N) {
    f1 = f1 + 2
    f2 = f1 + 1
    f3 = f3 + 1
    if (unis[f3]<fj(two[f1],two[f2])){
      twos = c(twos, c(two[f1],two[f2]))
    }
  }
  data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))])
}
