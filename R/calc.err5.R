calc.err5 <-
function(err.type,var1,var2) {
  value=list(inf=NULL,sup=NULL)
  if (err.type==Env$vocab[119,1]) {
    value$inf=tapply(var1,var2,sd)
    value$sup=value$inf
  } else if (err.type==Env$vocab[120,1]) {
    value$inf=tapply(var1,var2,function(x) sd(x)/sqrt(length(x)))
    value$sup=value$inf
  } else if (err.type==Env$vocab[121,1]) {
    if (any(tapply(var1,var2,length)<30)==TRUE) {
	value$inf=tapply(var1,var2,function(x) mean(x)-wilcox.test(x,conf.int=TRUE)$conf.int[1])
	value$sup=tapply(var1,var2,function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2]-mean(x))
	tkmessageBox(title=Env$vocab[239,1],message=Env$vocab[240,1],icon="info",type="ok")
    } else {
	value$inf=tapply(var1,var2,function(x) mean(x)-t.test(x)$conf.int[1])
	value$sup=value$inf
    }
  } else {
    value$inf=rep(0,nlevels(factor(var2)))
    value$sup=value$inf
  }
  return(value)
}

