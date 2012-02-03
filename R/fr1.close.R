fr1.close <-
function() {
  Env$l.frames$Fr1.status<-0
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_bas.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$vide<-tklabel(Env$l.frames$Fr1,text="",font=Env$police2)
  tkgrid(Env$l.fr1$vide)
}
