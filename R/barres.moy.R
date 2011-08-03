barres.moy <-
function() {
  tkconfigure(Env$l.fr1$titre1,foreground="black")
  tkconfigure(Env$l.fr1$moyvar.lab,foreground="black")
  tkconfigure(Env$l.fr1$moyvar.wdg,state="readonly")
  tkconfigure(Env$l.fr1$moyfac1.lab,foreground="black")
  tkconfigure(Env$l.fr1$moyfac1.wdg,state="readonly")
  tkconfigure(Env$l.fr1$moyfac2.lab,foreground="black")
  tkconfigure(Env$l.fr1$moyfac2.wdg,state="readonly")
  tkconfigure(Env$l.fr1$titre2,foreground="grey")
  tkconfigure(Env$l.fr1$propvar.lab,foreground="grey")
  tkconfigure(Env$l.fr1$propvar.wdg,state="disabled")
  tkconfigure(Env$l.fr1$propnivx.lab,foreground="grey")
  tkconfigure(Env$l.fr1$propnivx.list,state="disabled")
  tkconfigure(Env$l.fr1$propfac.lab,foreground="grey")
  tkconfigure(Env$l.fr1$propfac.wdg,state="disabled")
  tclvalue(Env$l.var$stack)<-0
  if (exists("noms.list",where=Env$l.fr3)) {
    tkdelete(Env$l.fr3$noms.list,0,"end")
    for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
    tkdelete(Env$l.fr3$noms.wdg,0,"end")
  }
  if (exists("type.wdg",where=Env$l.fr5)) {
    tclvalue(Env$l.var$erreur)<-""
    tkconfigure(Env$l.fr5$type.wdg,values=Env$voc[c(95:98),1])
  }
  if (nchar(tclvalue(tkget(Env$l.fr1$moyfac2.wdg)))>0 & tclvalue(tkget(Env$l.fr1$moyfac2.wdg))!=Env$voc[82,1]) {
    tclvalue(Env$l.var$plusieurs)<-1
    Env$l.var$couleur1B<-grey.colors(nlevels(Env$dataset[,tclvalue(Env$l.var$facteur2)]))
    Env$l.var$col.borduresB<-rep("black",nlevels(Env$dataset[,tclvalue(Env$l.var$facteur2)]))
    Env$l.var$hachuresB<-rep(1,nlevels(Env$dataset[,tclvalue(Env$l.var$facteur2)]))
    if (exists("noms.list",where=Env$l.fr4)) {
	tkconfigure(Env$l.fr4$noms.list,state="normal")
	tkdelete(Env$l.fr4$noms.list,0,"end")
	for (i in 1:length(Env$l.var$noms2)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms2[i])}
	tkconfigure(Env$l.fr4$colbarres.wdg,bg=Env$l.var$couleur1B[1])
	tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
	for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[1]]],borderwidth=2)
	tkconfigure(Env$l.fr4$stack.lab,foreground="black")
	tkconfigure(Env$l.fr4$stack.wdg,state="normal")
	tkdeselect(Env$l.fr4$stack.wdg)
    }
    active.legende()
    if (exists("noms.list",where=Env$l.fr6)) {
	tkdelete(Env$l.fr6$noms.list,0,"end")
	for (i in 1:length(Env$l.var$noms2)) {tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$noms2[i])}
    }
  } else {
    tclvalue(Env$l.var$plusieurs)<-0
    tclvalue(Env$l.var$couleur1A)<-"white"
    tclvalue(Env$l.var$col.borduresA)<-"black"
    tclvalue(Env$l.var$hachuresA)<-"1"
    if (exists("noms.list",where=Env$l.fr4)) {
	tkconfigure(Env$l.fr4$noms.list,state="normal")
	tkdelete(Env$l.fr4$noms.list,0,"end")
	tkconfigure(Env$l.fr4$noms.list,state="disabled")
	tkconfigure(Env$l.fr4$colbarres.wdg,bg=tclvalue(Env$l.var$couleur1A))
	tkconfigure(Env$l.fr4$colbordures.wdg,bg=tclvalue(Env$l.var$col.borduresA))
	for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	tkconfigure(Env$l.fr4$l.hachures[[as.numeric(tclvalue(Env$l.var$hachuresA))]],borderwidth=2)
	tkconfigure(Env$l.fr4$stack.lab,foreground="grey")
	tkdeselect(Env$l.fr4$stack.wdg)
	tkconfigure(Env$l.fr4$stack.wdg,state="disabled")
    }
    active.legende()
  }
  active.erreur()
}

