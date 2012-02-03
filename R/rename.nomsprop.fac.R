rename.nomsprop.fac <-
function(value.list,value.nom) {
  if(nchar(value.nom)>0) {
    if (nchar(value.list)>0) {
	Env$l.var$nomsprop.fac[as.numeric(value.list)+1]<-value.nom
	tkdelete(Env$l.fr3$noms.list,0,"end")
	for (i in 1:length(Env$l.var$nomsprop.fac)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$nomsprop.fac[i])}
	tkdelete(Env$l.fr3$noms.wdg,0,"end")
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
  } else {
    msg(text=Env$voc[24,1],type="error")
  }
}
