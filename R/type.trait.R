type.trait <-
function(type) {
  result<-NULL
  for (i in 1:length(type)) {
    result<-c(result,if (type[i]==Env$voc[61,1]) {3} else
    if (type[i]==Env$voc[62,1]) {2} else
    {1})
  }
  return(result)
}
