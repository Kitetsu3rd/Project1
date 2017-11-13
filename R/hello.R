#' what is the function does: Single variable Rejection Sampling
#'
#' This function implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#' The second paragraph will show up somewhere else and should be addition
#'
#' @param f the pdf that we are sampling from
#' @param N the nimber of attempted samples.
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f
#' @param maxf bond of f
#'
#' @return A vector containing samples from pdf
#' @export
#'
#' @examples
#'
#' betaPDF <- function(x) {
#' ifelse(0 < x & x < 1, 2*x, 0)}
#'oneDsample(f = betaPDF, N=100, lb = 0, ub = 1, maxf = 2)
#' hist(oneDsample(f = betaPDF, N=1000000, lb = 0, ub = 1, maxf = 2))
#'
#'jointPDF <- function(x,y){
#'ifelse(0<x & x <1 & 0<y & y<1 & 0<x+y & x+y<1, 24*x*y, 0)}
#'twoDsample(f = jointPDF, N=100, lbx=0, ubx=1, lby=0, uby=1, maxfj = 6)

oneDsample <- function(f, N, lb, ub, maxf) {
  ones <- runif(N, lb, ub)
  unis <- runif(N, 0, maxf)
  ones[unis < f(ones)]
}

