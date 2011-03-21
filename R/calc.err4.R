calc.err4 <-
function(err.type,x,y,niv1,proportions) {
  value=list(inf=NULL,sup=NULL)
  if (err.type==Env$vocab[121,1]) {
    value$inf=tapply(y,x,function(x) length(x[x==niv1])/length(x)-binom.test(length(x[x==niv1]),length(x))$conf.int[1])
    value$sup=tapply(y,x,function(x) binom.test(length(x[x==niv1]),length(x))$conf.int[2]-length(x[x==niv1])/length(x))
  } else {
    value$inf=rep(0,length(proportions))
    value$sup=value$inf
  }
  return(value)
}

