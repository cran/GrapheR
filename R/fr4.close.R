fr4.close <-
function() {
  Env$l.frames$Fr4.status<-0
  tkconfigure(Env$l.wdg$but.lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_bas.gif",fsep=.Platform$file.sep)))
  if (exists("l.hachures",where=Env$l.fr4)) {
    for (i in 1:9) {tkdestroy(Env$l.fr4$l.hachures[[i]])}
  }
  if (exists("l.symboles",where=Env$l.fr4)) {
    for (i in 1:8) {tkdestroy(Env$l.fr4$l.symboles[[i]])}
  }
  for (i in 1:length(Env$l.fr4)) {
    if (names(Env$l.fr4)[i]!="l.hachures" & names(Env$l.fr4)[i]!="l.symboles") {
	tkdestroy(Env$l.fr4[[i]])
    }
  }
  Env$l.fr4<-list()
  Env$l.fr4$vide<-tklabel(Env$l.frames$Fr4,text="",font=Env$police2)
  tkgrid(Env$l.fr4$vide)
}

