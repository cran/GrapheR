data.load2 <-
function() {
  if (nchar(tclvalue(tkcurselection(Env$l.fr1$obj.list)))>0) {
    tables<-NULL
    for (i in 1:length(ls(.GlobalEnv))) {
	if (is.data.frame(get(ls(.GlobalEnv)[i]))) {tables<-c(tables,ls(.GlobalEnv)[i])}
    }
    Env$dataset<-get(tables[as.numeric(tclvalue(tkcurselection(Env$l.fr1$obj.list)))+1],pos=.GlobalEnv)
    if (exists("var.list",where=Env$l.fr2)) {
	tkdelete(Env$l.fr2$var.list,0,"end")
	for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr2$var.list,"end",colnames(Env$dataset)[i])}
	tkconfigure(Env$l.fr2$type.wdg,text="")
	tkconfigure(Env$l.fr2$resume.wdg,state="normal")
	tkdelete(Env$l.fr2$resume.wdg,"0.0","end")
	tkconfigure(Env$l.fr2$resume.wdg,state="disabled")
    }
    if (exists("var.list",where=Env$l.fr3)) {
	tkdelete(Env$l.fr3$var.list,0,"end")
	for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr3$var.list,"end",colnames(Env$dataset)[i])}
    }
    if (exists("var.list",where=Env$l.fr4)) {
	tkdelete(Env$l.fr4$var.list,0,"end")
	for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr4$var.list,"end",colnames(Env$dataset)[i])}
    }
    variables.class()
    msg(text=Env$voc[21,1],type="info")
  } else {
    msg(text=Env$voc[20,1],type="error")
  }
}

