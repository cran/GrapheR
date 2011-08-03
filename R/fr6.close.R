fr6.close <-
function() {
  Env$l.frames$Fr6.status<-0
  tkconfigure(Env$l.wdg$but.lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_bas.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr6)) {tkdestroy(Env$l.fr6[[i]])}
  Env$l.fr6<-list()
  Env$l.fr6$vide<-tklabel(Env$l.frames$Fr6,text="",font=Env$police2)
  tkgrid(Env$l.fr6$vide)
}

