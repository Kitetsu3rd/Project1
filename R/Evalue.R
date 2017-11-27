Evalue <-function(g,data){
  len = length(data)
  if (len == 2){
    meanxy = as.numeric(mean(g(data$x,data$y)))
    return(meanxy)
  }
  else if (len == 1){
    meanx = mean(g(data$x))
    return(meanx)
  }
  else {
    stop("Error: invalid data format")
  }
}


