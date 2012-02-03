rename.variable <-
function() {
  if(nchar(tclvalue(tkget(Env$l.fr3$nom.wdg)))>0) {
    if (nchar(tclvalue(tkcurselection(Env$l.fr3$var.list)))>0) {
	names(Env$dataset)[as.numeric(tclvalue(tkcurselection(Env$l.fr3$var.list)))+1]<-tclvalue(tkget(Env$l.fr3$nom.wdg))
	tkdelete(Env$l.fr3$var.list,0,"end")
	for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr3$var.list,"end",colnames(Env$dataset)[i])}
	tkdelete(Env$l.fr3$nom.wdg,0,"end")
	if (exists("var.list",where=Env$l.fr2)) {
	  tkdelete(Env$l.fr2$var.list,0,"end")
	  for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr2$var.list,"end",colnames(Env$dataset)[i])}
	}
	if (exists("var.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$var.list,0,"end")
	  for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr4$var.list,"end",colnames(Env$dataset)[i])}
	}
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
    variables.class()
    if (exists("fact.wdg",where=Env$l.fr5)) {
	tkconfigure(Env$l.fr5$fact.wdg,values=Env$l.var$var.fact)
	tclvalue(Env$l.var$facteur1) <- ""
	Env$l.var$levels.temp <- NULL
	tkdelete(Env$l.fr5$liste.actual,0,"end")
	tkdelete(Env$l.fr5$liste.new,0,"end")
    }
  } else {
    msg(text=Env$voc[24,1],type="error")
  }
}
