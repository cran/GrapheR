rename.legende2 <-
function(value.list,value.nom) {
  if(nchar(value.nom)>0) {
    if (nchar(value.list)>0) {
	Env$l.var$noms1[as.numeric(value.list)+1]<-value.nom
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
	}
	if (exists("noms.list",where=Env$l.fr5)) {
	  tkdelete(Env$l.fr5$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr5$noms.list,"end",Env$l.var$noms1[i])}
	  tkdelete(Env$l.fr5$noms.wdg,0,"end")
	}
	if (exists("noms.list",where=Env$l.fr6)) {
	  tkdelete(Env$l.fr6$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$noms1[i])}
	  tkdelete(Env$l.fr6$noms.wdg,0,"end")
	}
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
  } else {
    msg(text=Env$voc[24,1],type="error")
  }
}

