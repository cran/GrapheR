rename.legende <-
function(value.list,value.nom) {
  if(nchar(value.nom)>0) {
    if (nchar(value.list)>0) {
	if (tclvalue(Env$l.var$moyprop)=="moy") {
	  Env$l.var$noms2[as.numeric(value.list)+1]<-value.nom
	  if (exists("noms.list",where=Env$l.fr6)) {
	    tkdelete(Env$l.fr6$noms.list,0,"end")
	    for (i in 1:length(Env$l.var$noms2)) {tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$noms2[i])}
	    tkdelete(Env$l.fr6$noms.wdg,0,"end")
	  }
	  if (exists("noms.list",where=Env$l.fr4)) {
	    tkdelete(Env$l.fr4$noms.list,0,"end")
	    for (i in 1:length(Env$l.var$noms2)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms2[i])}
	  }
	} else {
	  Env$l.var$nomsprop[as.numeric(value.list)+1]<-value.nom
	  if (exists("noms.list",where=Env$l.fr4)) {
	    tkdelete(Env$l.fr4$noms.list,0,"end")
	    for (i in 1:length(Env$l.var$nomsprop)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$nomsprop[i])}
	  }
	  if (exists("noms.list",where=Env$l.fr6)) {
	    tkdelete(Env$l.fr6$noms.list,0,"end")
	    for (i in 1:length(Env$l.var$nomsprop)) {tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$nomsprop[i])}
	    tkdelete(Env$l.fr6$noms.wdg,0,"end")
	  }
	}
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
  } else {
    msg(text=Env$voc[24,1],type="error")
  }
}

