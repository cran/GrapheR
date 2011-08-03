data.load1 <-
function() {
  file<-tclvalue(tkgetOpenFile(filetypes=paste("{{.",tclvalue(Env$l.var$extension),"} {.",tclvalue(Env$l.var$extension),"}}",sep="")))
  if (!nchar(file)) {
    msg(text=Env$voc[19,1],type="error")
  } else {
    Env$dataset<-read.table(file,dec=ifelse(tclvalue(Env$l.var$sepdec)==Env$voc[9,1],".",","),header=ifelse(tclvalue(Env$l.var$header)==1,TRUE,FALSE),
	na.strings=tclvalue(Env$l.var$na),sep=if (tclvalue(Env$l.var$sepcol)==Env$voc[5,1]) {""} else if (tclvalue(Env$l.var$sepcol)==Env$voc[6,1]) {","} else {";"})
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
  }
}

