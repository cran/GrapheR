type.ligne <-
function(type) {
  result<-NULL
  for (i in 1:length(type)) {
    result<-c(result,if (type[i]==Env$voc[133,1]) {"p"} else
    if (type[i]==Env$voc[134,1]) {"l"} else
    if (type[i]==Env$voc[136,1]) {"o"} else
    if (type[i]==Env$voc[137,1]) {"h"} else
    {"b"})
  }
  return(result)
}
