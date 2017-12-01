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
  bdtest <- runif(1000000,-50,50)
  if (f(-50) == 0 & f(50) == 0){
    lb = min(bdtest[which(f(bdtest)>0)])
    ub = max(bdtest[which(f(bdtest)>0)])
  }
  if (abs(integrate(f, lb, ub)$val - 1) > 0.001) {
    stop("Error: Bound is missing/wrong or the function is not a pdf. The area under the function you given should be 1")
  }
  else{
    if (lb != -Inf & ub != Inf){
      maxf <- optimize(f,c(lb,ub),maximum = TRUE)
      maxf <- maxf$objective
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
      max <- optimize(f,c(-5000,5000),maximum = TRUE)
      maxf <- max$objective          #get the maximum of the given pdf
      mu=max$maximum                 #set the x as the mean of normal distribution where we get the maximum
      sd = 2/maxf                    #based on the pdf of normal distribution, we have standard deviation = 1/sqrt(2*pi)/maxf,
                                     #but we make it a little bit larger because the given pdf might be flat
      C = maxf/dnorm(mu,mu,sd)       #The value we balance the normal distribution science we increase the sd.
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



