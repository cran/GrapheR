symboles <-
function(num) {
  if (tclvalue(Env$l.var$plusieurs)==0) {
    tclvalue(Env$l.var$symboleA)<-as.character(num)
    for (i in 1:8) {tkconfigure(Env$l.fr4$l.symboles[[i]],borderwidth=0)}
    tkconfigure(Env$l.fr4$l.symboles[[num]],borderwidth=2)
  } else {
    if (nchar(tclvalue(tkcurselection(Env$l.fr4$noms.list)))>0) {
	Env$l.var$symboleB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1]<-num
	for (i in 1:8) {tkconfigure(Env$l.fr4$l.symboles[[i]],borderwidth=0)}
	tkconfigure(Env$l.fr4$l.symboles[[num]],borderwidth=2)
    } else {
	msg(text=Env$voc[25,1],type="error")
    }
  }
}
