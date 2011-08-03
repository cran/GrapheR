convert.variable <-
function(type) {
  num<-as.numeric(tclvalue(tkcurselection(Env$l.fr4$var.list)))+1
  if (type=="character") {
    Env$dataset[,num]<-factor(Env$dataset[,num])
  } else {
    if (tclvalue(Env$l.var$regroup1)==0) {
	Env$dataset[,num]<-factor(Env$dataset[,num])
    } else {
	if (tclvalue(Env$l.var$regroup1)=="long") {
	  Env$dataset[,num]<-cut(Env$dataset[,num],breaks=as.numeric(tclvalue(Env$l.var$regroup3)),labels=as.character(1:as.numeric(tclvalue(Env$l.var$regroup3))))
	} else {
	  Env$dataset[,num]<-cut(Env$dataset[,num],breaks=quantile(Env$dataset[,num],probs=seq(0,1,1/as.numeric(tclvalue(Env$l.var$regroup3))),na.rm=TRUE),labels=as.character(1:as.numeric(tclvalue(Env$l.var$regroup3))))
	}
    }
  }
  tclvalue(Env$l.var$regroup1)<-0
  tclvalue(Env$l.var$regroup2)<-"long"
  tkconfigure(Env$l.fr4$rb.noregroup,state="disabled")
  tkconfigure(Env$l.fr4$rb.regroup1,state="disabled")
  tkconfigure(Env$l.fr4$rb.regroup2,state="disabled")
  tkconfigure(Env$l.fr4$rb.regroup3,state="disabled")
  tkconfigure(Env$l.fr4$curs.wdg,state="disabled",foreground="grey")
  tkconfigure(Env$l.fr4$curs.lab,foreground="grey")
  tkconfigure(Env$l.fr4$but,state="disabled")
  variables.class()
  msg(text=Env$voc[35,1],type="info")
}

