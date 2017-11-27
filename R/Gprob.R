Gprob <-function(condition,data){
  len = length(data)
  if (len == 2){
    meanxy = mean(condition(data$x,data$y))
    return(meanxy)
  }
  else if (len == 1){
    meanx = mean(condition(data$x))
    return(meanx)
  }
  else {
    stop("Error: invalid data format")
  }
}
