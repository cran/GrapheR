variables.class <-
function() {
  Env$l.var$var.num<-NULL
  for (i in 1:ncol(Env$dataset)) {
    if (is.numeric(Env$dataset[,i])) {Env$l.var$var.num<-c(Env$l.var$var.num,names(Env$dataset)[i])}
  }
  if (is.null(Env$l.var$var.num)) {Env$l.var$var.num<-""}
  Env$l.var$var.fact<-NULL
  for (i in 1:ncol(Env$dataset)) {
    if (is.factor(Env$dataset[,i])) {Env$l.var$var.fact<-c(Env$l.var$var.fact,names(Env$dataset)[i])}
  }
  if (is.null(Env$l.var$var.fact)) {Env$l.var$var.fact<-""}
}
