fr5.close <-
function() {
  Env$l.frames$Fr5.status<-0
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_bas.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.fr5$vide<-tklabel(Env$l.frames$Fr5,text="",font=Env$police2)
  tkgrid(Env$l.fr5$vide)
}
