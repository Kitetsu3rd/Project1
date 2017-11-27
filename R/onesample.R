#' what is the function does: Single variable Rejection Sampling
#'
#' This function implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#' The second paragraph will show up somewhere else and should be addition
#'
#' Usage
#' oneDsample(f, N, lb = -Inf, ub = Inf)
#'
#'Arguments
#' @param f the pdf that we are sampling from.
#' @param N the nimber of output samples.
#' @param lb lower bound of support of f, deault is infinity.
#' @param ub upper bound of support of f, default is infinity.
#'
#' @return A vector containing samples from pdf
#' @export
#'
#' @examples
#'
#' betaPDF <- function(x) {
#' ifelse(0 < x & x < 1, 3*x^2, 0)}
#' oneDsample(f = betaPDF, N=100, lb = 0, ub = 1)
#' ggplot(oneDsample(f = betaPDF, N=5000, lb=0, ub=1), aes(x)) + geom_density()

oneDsample <- function(f, N, lb = -Inf, ub = Inf, mean = 0) {
  if (abs(integrate(f, lb, ub)$val - 1) > 0.001) {
    stop("Error: Bound is missing/wrong or the function is not a pdf. The area under the function you given should be 1")
  }
  else{
    if (lb != -Inf & ub != Inf){
      maxf <- max(f(runif(100000,lb,ub)))
      ones = c()
      n = 0
      while (n < N) {
        one <-runif(1,lb,ub)
        if (runif(1, 0, maxf) < f(one)){
          ones = c(ones, one)
          n = n + 1
        }
      }
      return(data.frame(x=ones))
    }
    else {
      x <- runif(200000,-5000,5000)
      maxf <- max(f(x))
      mu=x[which(f(x)==maxf)]
      sd = 2/maxf
      C = maxf/dnorm(mu,mu,sd)
      ones = c()
      n = 0
      while (n < N) {
        one = rnorm(1, mu, sd)
        if (runif(1, 0, C * dnorm(one,mu,sd)) < f(one)){
          ones = c(ones, one)
          n = n + 1
        }
      }
      return(data.frame(x=ones))
    }
  }
}


f<- function(x) dnorm(x,-100,10000)
f<- function(x) 1/pi/(1+x^2)
f<- function(x){ifelse(0 < x & x < 1, 3*x^3, 0)}
f<- function(x) {ifelse(0<=x, dlnorm(x,mean=0,sdlog=1),0)}
a<-oneDsample(f,10000)

oneDsampleplot(a)
