fr2.close <-
function() {
  Env$l.frames$Fr2.status<-0
  tkconfigure(Env$l.wdg$but.lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_bas.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr2)) {tkdestroy(Env$l.fr2[[i]])}
  Env$l.fr2<-list()
  Env$l.fr2$vide<-tklabel(Env$l.frames$Fr2,text="",font=Env$police2)
  tkgrid(Env$l.fr2$vide)
}

