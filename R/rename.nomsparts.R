rename.nomsparts <-
function() {
  if(nchar(tclvalue(tkget(Env$l.fr3$noms.wdg)))>0) {
    if (nchar(tclvalue(tkcurselection(Env$l.fr3$noms.list)))>0) {
	Env$l.var$nomsparts[as.numeric(tclvalue(tkcurselection(Env$l.fr3$noms.list)))+1]<-tclvalue(tkget(Env$l.fr3$noms.wdg))
	tkdelete(Env$l.fr3$noms.list,0,"end")
	for (i in 1:length(Env$l.var$nomsparts)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$nomsparts[i])}
	tkdelete(Env$l.fr3$noms.wdg,0,"end")
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$nomsparts)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$nomsparts[i])}
	}
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
  } else {
    msg(text=Env$voc[24,1],type="error")
  }
}
