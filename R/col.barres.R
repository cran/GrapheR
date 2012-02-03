col.barres <-
function() {
  if (tclvalue(Env$l.var$plusieurs)==0) {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$couleur1A),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$couleur1A)<-temp
	tkconfigure(Env$l.fr4$colbarres.wdg,bg=tclvalue(Env$l.var$couleur1A))
    }
  } else {
    if (nchar(tclvalue(tkcurselection(Env$l.fr4$noms.list)))>0) {
	temp<-tclvalue(tcl("tk_chooseColor",initialcolor=Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1],title=Env$voc[64,1]))
	if (nchar(temp)>0) {
	  Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1]<-temp
	  tkconfigure(Env$l.fr4$colbarres.wdg,bg=Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1])
	}
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
  }
}
