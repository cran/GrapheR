rename.noms1 <-
function(value.list,value.nom) {
  if(nchar(value.nom)>0) {
    if (nchar(value.list)>0) {
	Env$l.var$noms1[as.numeric(value.list)+1]<-value.nom
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
	  tkdelete(Env$l.fr3$noms.wdg,0,"end")
	}
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
	}
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
  } else {
    msg(text=Env$voc[24,1],type="error")
  }
}
