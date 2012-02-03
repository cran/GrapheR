fr5.openCo <-
function() {
  Env$l.frames$Fr5.status<-1
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.fr5$type.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[94,1],font=Env$police)
  Env$l.fr5$type.wdg<-ttkcombobox(Env$l.frames$Fr5,values="",textvariable=Env$l.var$erreur,font=Env$police,state="readonly")
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    tkconfigure(Env$l.fr5$type.wdg,values=Env$voc[c(95:98),1])
  } else {
    tkconfigure(Env$l.fr5$type.wdg,values=Env$voc[c(95,97,98),1])
  }
  tkgrid(Env$l.fr5$type.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr5$type.wdg,row=0,column=1,sticky="w")
}
