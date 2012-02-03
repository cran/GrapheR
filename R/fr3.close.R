fr3.close <-
function() {
  Env$l.frames$Fr3.status<-0
  tkconfigure(Env$l.wdg$but.lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_bas.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr3)) {tkdestroy(Env$l.fr3[[i]])}
  Env$l.fr3<-list()
  Env$l.fr3$vide<-tklabel(Env$l.frames$Fr3,text="",font=Env$police2)
  tkgrid(Env$l.fr3$vide)
}
